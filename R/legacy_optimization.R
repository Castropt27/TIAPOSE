# ============================================================
# legacy_optimization.R
# Funcoes reutilizaveis baseadas nos scripts originais de O2/O3
# ============================================================

LEGACY_DEFAULT_OUTPUT_DIR <- file.path("outputs", "optimization", "growing_window")
LEGACY_STORE_NAMES <- c("baltimore", "lancaster", "philadelphia", "richmond")
LEGACY_DAY_NAMES <- c("su", "mo", "tu", "we", "th", "fr", "sa")

legacy_output_dir <- function(output_dir = LEGACY_DEFAULT_OUTPUT_DIR) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir
}

legacy_load_city_data <- function(city) {
  path <- file.path("data", paste0(city, ".csv"))

  if (!file.exists(path)) {
    stop(paste("Ficheiro nao encontrado:", path))
  }

  df <- read.csv(path, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]

  df$Num_Customers[df$Num_Customers < 0] <- NA
  df$Num_Customers <- as.numeric(forecast::na.interp(ts(df$Num_Customers, frequency = 7)))

  df
}

legacy_create_C_pred <- function(gw_iter, initial_train = 574, h = 7) {
  start_idx <- initial_train + (gw_iter - 1) * h + 1
  end_idx   <- start_idx + h - 1

  C_pred <- matrix(0, nrow = 4, ncol = 7)

  for (s in seq_along(LEGACY_STORE_NAMES)) {
    df <- legacy_load_city_data(LEGACY_STORE_NAMES[s])

    if (end_idx > nrow(df)) {
      stop("Nao ha dados suficientes para esta iteracao growing window.")
    }

    C_pred[s, ] <- df$Num_Customers[start_idx:end_idx]
  }

  rownames(C_pred) <- LEGACY_STORE_NAMES
  colnames(C_pred) <- LEGACY_DAY_NAMES
  C_pred
}

legacy_calcupper <- function(prev) {
  upper <- numeric(84)

  for (store in 1:4) {
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      C <- prev[store, day]

      upper[idx + 1] <- ceiling(C / 6) + 2
      upper[idx + 2] <- ceiling(C / 7) + 2
      upper[idx + 3] <- 30
    }
  }

  upper
}

legacy_calc_rh_total <- function(s) {
  J_idx <- seq(1, 84, by = 3)
  X_idx <- seq(2, 84, by = 3)
  sum(round(s[J_idx])) + sum(round(s[X_idx]))
}

legacy_build_solution_df <- function(best_s, method, gw_iter, omega = NA_real_, rh_limit = NA_real_) {
  data.frame(
    Metodo = method,
    GW_Iteration = gw_iter,
    omega = omega,
    RH_Limit = rh_limit,
    Variable = paste0("x", 1:84),
    Value = round(best_s),
    stringsAsFactors = FALSE
  )
}

legacy_build_result_df <- function(method, gw_iter, omega, rh_limit, rh_total, lucro) {
  data.frame(
    Metodo = method,
    GW_Iteration = gw_iter,
    omega = omega,
    RH_Limit = rh_limit,
    RH_Total = rh_total,
    Lucro = lucro,
    stringsAsFactors = FALSE
  )
}

legacy_build_pareto_solution_df <- function(pareto_pars, gw_iter) {
  if (is.null(dim(pareto_pars))) {
    pareto_pars <- matrix(pareto_pars, nrow = 1)
  }

  out <- data.frame()

  for (i in seq_len(nrow(pareto_pars))) {
    temp <- data.frame(
      Metodo = "NSGA-II",
      GW_Iteration = gw_iter,
      solution_id = i,
      Variable = paste0("x", 1:84),
      Value = round(pareto_pars[i, ]),
      stringsAsFactors = FALSE
    )
    out <- rbind(out, temp)
  }

  out
}

legacy_run_pso_week <- function(C_pred,
                                gw_iter,
                                omega = 0.7,
                                rh_limit = NA_real_,
                                objective_name = "O2",
                                maxit = 100,
                                swarm_size = 80,
                                max_units_week = 10000) {

  if (!requireNamespace("pso", quietly = TRUE)) {
    return(list(
      status = "not_implemented",
      message = "O objetivo legado requer o pacote 'pso'. Instale com: install.packages('pso')"
    ))
  }

  lower <- rep(0, 84)
  upper <- legacy_calcupper(C_pred)
  best_history <- numeric(0)

  objective_pso <- function(s) {
    s_repaired <- repair_solution(
      s = s,
      C_pred = C_pred,
      max_units_week = max_units_week
    )

    rh_total <- legacy_calc_rh_total(s_repaired)

    score <- eval_solution(
      s = s_repaired,
      C_pred = C_pred,
      is_weekend = IS_WEEKEND,
      objective = objective_name
    )

    if (!is.na(rh_limit) && rh_total > rh_limit) {
      score <- score - (10000 * (rh_total - rh_limit))
    }

    best_history <<- c(best_history, score)
    -score
  }

  result <- pso::psoptim(
    par = runif(84, lower, upper),
    fn = objective_pso,
    lower = lower,
    upper = upper,
    control = list(
      maxit = maxit,
      s = swarm_size,
      w = omega,
      c.p = 1.5,
      c.g = 1.5
    )
  )

  best_s <- repair_solution(
    s = result$par,
    C_pred = C_pred,
    max_units_week = max_units_week
  )

  rh_total <- legacy_calc_rh_total(best_s)
  best_score <- eval_solution(
    s = best_s,
    C_pred = C_pred,
    is_weekend = IS_WEEKEND,
    objective = objective_name
  )

  if (!is.na(rh_limit) && rh_total > rh_limit) {
    best_score <- best_score - (10000 * (rh_total - rh_limit))
  }

  history <- if (length(best_history) > 0) cummax(best_history) else numeric(0)
  plan <- build_plan_table(best_s, C_pred)

  summary <- data.frame(
    Objetivo = objective_name,
    Cidade = "todas",
    Semana = paste("Semana", gw_iter),
    Lucro_Total = sum(plan$Lucro),
    RH_Total = rh_total,
    Unidades_Vendidas = sum(plan$Unidades),
    Valor_Objetivo = round(best_score, 2),
    stringsAsFactors = FALSE
  )

  result_df <- legacy_build_result_df(
    method = "PSO",
    gw_iter = gw_iter,
    omega = omega,
    rh_limit = rh_limit,
    rh_total = rh_total,
    lucro = best_score
  )

  solution_df <- legacy_build_solution_df(
    best_s = best_s,
    method = "PSO",
    gw_iter = gw_iter,
    omega = omega,
    rh_limit = rh_limit
  )

  list(
    status = "ok",
    objective = objective_name,
    score = best_score,
    summary = summary,
    plan = plan,
    history = history,
    solution = best_s,
    result = result_df,
    solution_df = solution_df
  )
}

legacy_run_nsga2_week <- function(C_pred,
                                  gw_iter,
                                  objective_name = "O3",
                                  popsize = 60,
                                  generations = 100,
                                  max_units_week = 10000) {

  if (!requireNamespace("mco", quietly = TRUE)) {
    return(list(
      status = "not_implemented",
      message = "O objetivo legado requer o pacote 'mco'. Instale com: install.packages('mco')"
    ))
  }

  lower <- rep(0, 84)
  upper <- legacy_calcupper(C_pred)

  nsga_objective <- function(s) {
    s_repaired <- repair_solution(
      s = s,
      C_pred = C_pred,
      max_units_week = max_units_week
    )

    rh_total <- legacy_calc_rh_total(s_repaired)
    lucro <- eval_solution(
      s = s_repaired,
      C_pred = C_pred,
      is_weekend = IS_WEEKEND,
      objective = objective_name
    )

    c(rh_total, -lucro)
  }

  set.seed(100 + gw_iter)

  res <- mco::nsga2(
    fn = nsga_objective,
    idim = 84,
    odim = 2,
    lower.bounds = lower,
    upper.bounds = upper,
    popsize = popsize,
    generations = generations
  )

  pareto_idx <- which(res$pareto.optimal)
  pareto_values <- res$value[pareto_idx, ]
  pareto_pars <- res$par[pareto_idx, ]

  if (is.null(dim(pareto_values))) {
    pareto_values <- matrix(pareto_values, nrow = 1)
  }
  if (is.null(dim(pareto_pars))) {
    pareto_pars <- matrix(pareto_pars, nrow = 1)
  }

  pareto_df <- data.frame(
    Metodo = "NSGA-II",
    GW_Iteration = gw_iter,
    omega = NA_real_,
    RH_Limit = NA_real_,
    Total_HR = pareto_values[, 1],
    Lucro = -pareto_values[, 2],
    stringsAsFactors = FALSE
  )
  pareto_df <- pareto_df[order(pareto_df$Total_HR), ]

  pareto_df$Unidades_Vendidas <- 0
  for (i in seq_len(nrow(pareto_pars))) {
    sol_rep <- repair_solution(
      s = pareto_pars[i, ],
      C_pred = C_pred,
      max_units_week = max_units_week
    )
    plan <- build_plan_table(sol_rep, C_pred)
    pareto_df$Unidades_Vendidas[i] <- sum(plan$Unidades)
  }

  # Add solution_id column to match app expectations
  pareto_df$solution_id <- seq_len(nrow(pareto_df))

  med_hr <- stats::median(pareto_df$Total_HR)
  rec_id <- which.min(abs(pareto_df$Total_HR - med_hr))

  rec_s <- repair_solution(
    s = pareto_pars[rec_id, ],
    C_pred = C_pred,
    max_units_week = max_units_week
  )
  rec_plan <- build_plan_table(rec_s, C_pred)
  rec_sum <- data.frame(
    Objetivo = objective_name,
    Cidade = "todas",
    Semana = paste("Semana", gw_iter),
    Lucro_Total = sum(rec_plan$Lucro),
    Total_HR = legacy_calc_rh_total(rec_s),
    Unidades_Vendidas = sum(rec_plan$Unidades),
    Valor_Objetivo = round(eval_solution(rec_s, C_pred, IS_WEEKEND, objective_name), 2),
    stringsAsFactors = FALSE
  )

  result_df <- data.frame(
    Metodo = "NSGA-II",
    GW_Iteration = gw_iter,
    omega = NA_real_,
    RH_Limit = NA_real_,
    Total_HR = pareto_df$Total_HR,
    Lucro = pareto_df$Lucro,
    solution_id = pareto_df$solution_id,
    stringsAsFactors = FALSE
  )

  solution_df <- legacy_build_pareto_solution_df(pareto_pars, gw_iter)

  list(
    status = "ok",
    objective = objective_name,
    pareto_df = pareto_df,
    recommended_id = rec_id,
    recommended_summary = rec_sum,
    recommended_plan = rec_plan,
    result = result_df,
    solution = solution_df
  )
}

legacy_run_o2_growing_window <- function(n_iter = 20,
                                        initial_train = 574,
                                        h = 7,
                                        output_dir = LEGACY_DEFAULT_OUTPUT_DIR,
                                        omega = 0.7,
                                        maxit = 100,
                                        swarm_size = 80,
                                        write_outputs = TRUE) {

  output_dir <- legacy_output_dir(output_dir)
  results_list <- vector("list", n_iter)
  pso_df <- data.frame()
  pso_solutions_df <- data.frame()
  all_convergence <- data.frame()

  for (i in seq_len(n_iter)) {
    C_pred <- legacy_create_C_pred(i, initial_train = initial_train, h = h)
    temp <- legacy_run_pso_week(
      C_pred = C_pred,
      gw_iter = i,
      omega = omega,
      rh_limit = NA_real_,
      objective_name = "O2",
      maxit = maxit,
      swarm_size = swarm_size
    )

    results_list[[i]] <- temp
    pso_df <- rbind(pso_df, temp$result)
    pso_solutions_df <- rbind(pso_solutions_df, temp$solution_df)

    conv_df <- data.frame(
      GW_Iteration = i,
      Evaluation = seq_along(temp$history),
      Best_Profit = temp$history,
      stringsAsFactors = FALSE
    )
    all_convergence <- rbind(all_convergence, conv_df)
  }

  summary_df <- data.frame(
    Iteration = seq_len(n_iter),
    Best_Score = sapply(results_list, function(x) x$score),
    stringsAsFactors = FALSE
  )

  final_summary <- data.frame(
    Method = "Particle Swarm Optimization",
    Objective = "O2",
    Score_Mean = mean(summary_df$Best_Score),
    Score_Median = stats::median(summary_df$Best_Score),
    Score_Min = min(summary_df$Best_Score),
    Score_Max = max(summary_df$Best_Score),
    stringsAsFactors = FALSE
  )

  best_solutions_df <- data.frame()
  for (i in seq_len(n_iter)) {
    temp <- data.frame(
      Iteration = i,
      Variable = paste0("x", 1:84),
      Value = round(results_list[[i]]$solution),
      stringsAsFactors = FALSE
    )
    best_solutions_df <- rbind(best_solutions_df, temp)
  }

  if (write_outputs) {
    utils::write.csv(pso_df, file.path(output_dir, "pso_growingwindow_scores.csv"), row.names = FALSE)
    utils::write.csv(summary_df, file.path(output_dir, "pso_growingwindow_summary.csv"), row.names = FALSE)
    utils::write.csv(all_convergence, file.path(output_dir, "pso_growingwindow_convergencia.csv"), row.names = FALSE)
    utils::write.csv(best_solutions_df, file.path(output_dir, "pso_growingwindow_best_solutions.csv"), row.names = FALSE)

    p_conv <- ggplot2::ggplot(all_convergence, ggplot2::aes(x = Evaluation, y = Best_Profit, group = GW_Iteration)) +
      ggplot2::geom_line(alpha = 0.35) +
      ggplot2::labs(
        title = "Convergencia - PSO com Growing Window",
        x = "Avaliacoes da funcao",
        y = "Melhor lucro encontrado"
      ) +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      file.path(output_dir, "pso_growingwindow_convergencia.png"),
      plot = p_conv,
      width = 10,
      height = 6
    )

    txt_file <- file.path(output_dir, "pso_growingwindow_resultados.txt")
    sink(txt_file)
    cat("=============================================\n")
    cat("Particle Swarm Optimization - Growing Window\n")
    cat("=============================================\n\n")
    cat("Resumo por iteracao:\n")
    print(summary_df)
    cat("\nResumo final:\n")
    print(final_summary)
    sink()
  }

  list(
    results_list = results_list,
    summary_df = summary_df,
    final_summary = final_summary,
    convergence = all_convergence,
    best_solutions_df = best_solutions_df,
    pso_df = pso_df,
    pso_solutions_df = pso_solutions_df,
    output_dir = output_dir
  )
}

legacy_plot_o3_week <- function(pso_df, nsga_df, gw) {
  pso_week <- pso_df[pso_df$GW_Iteration == gw, , drop = FALSE]
  nsga_week <- nsga_df[nsga_df$GW_Iteration == gw, , drop = FALSE]

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = nsga_week,
      ggplot2::aes(x = RH_Total, y = Lucro),
      color = "steelblue",
      size = 2
    ) +
    ggplot2::geom_line(
      data = nsga_week,
      ggplot2::aes(x = RH_Total, y = Lucro),
      color = "steelblue",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = pso_week,
      ggplot2::aes(x = RH_Total, y = Lucro, color = factor(omega)),
      size = 2
    ) +
    ggplot2::geom_line(
      data = pso_week,
      ggplot2::aes(x = RH_Total, y = Lucro, color = factor(omega), group = omega),
      linewidth = 0.7
    ) +
    ggplot2::labs(
      title = paste("Comparacao Pareto - PSO vs NSGA-II | GW", gw),
      subtitle = "Azul = NSGA-II | Cores = PSO",
      x = "RH Total",
      y = "Lucro",
      color = "omega"
    ) +
    ggplot2::theme_minimal()
}

legacy_run_o3_growing_window <- function(n_gw = 10,
                                        initial_train = 574,
                                        h = 7,
                                        rh_limits = seq(45, 95, by = 5),
                                        omegas = c(0.1, 0.5, 0.7),
                                        output_dir = LEGACY_DEFAULT_OUTPUT_DIR,
                                        popsize = 60,
                                        generations = 100,
                                        write_outputs = TRUE) {

  output_dir <- legacy_output_dir(output_dir)
  pso_df <- data.frame()
  pso_solutions_df <- data.frame()
  nsga_df <- data.frame()
  nsga_solutions_df <- data.frame()

  for (gw in seq_len(n_gw)) {
    C_pred <- legacy_create_C_pred(gw, initial_train = initial_train, h = h)

    for (omega in omegas) {
      for (rh in rh_limits) {
        temp <- legacy_run_pso_week(
          C_pred = C_pred,
          gw_iter = gw,
          omega = omega,
          rh_limit = rh,
          objective_name = "O3",
          maxit = generations,
          swarm_size = popsize
        )

        pso_df <- rbind(pso_df, temp$result)
        pso_solutions_df <- rbind(pso_solutions_df, temp$solution_df)
      }
    }

    nsga_temp <- legacy_run_nsga2_week(
      C_pred = C_pred,
      gw_iter = gw,
      objective_name = "O3",
      popsize = popsize,
      generations = generations
    )

    nsga_df <- rbind(nsga_df, nsga_temp$result)
    nsga_solutions_df <- rbind(nsga_solutions_df, nsga_temp$solution)

    if (write_outputs) {
      p_week <- legacy_plot_o3_week(pso_df, nsga_df, gw)
      ggplot2::ggsave(
        filename = file.path(output_dir, paste0("GW_", gw, "_PSO_vs_NSGA.png")),
        plot = p_week,
        width = 10,
        height = 6
      )
    }
  }

  all_pareto <- rbind(
    nsga_df[, c("Metodo", "GW_Iteration", "omega", "RH_Limit", "RH_Total", "Lucro")],
    pso_df[, c("Metodo", "GW_Iteration", "omega", "RH_Limit", "RH_Total", "Lucro")]
  )

  best_global <- all_pareto[order(-all_pareto$Lucro), ][1, , drop = FALSE]
  best_pso <- pso_df[order(-pso_df$Lucro), ][1, , drop = FALSE]
  best_nsga <- nsga_df[order(-nsga_df$Lucro), ][1, , drop = FALSE]

  if (write_outputs) {
    utils::write.csv(pso_df, file.path(output_dir, "PSO_resultados_completos.csv"), row.names = FALSE)
    utils::write.csv(nsga_df, file.path(output_dir, "NSGA_resultados_completos.csv"), row.names = FALSE)
    utils::write.csv(all_pareto, file.path(output_dir, "Comparacao_PSO_NSGA_completa.csv"), row.names = FALSE)
    utils::write.csv(pso_solutions_df, file.path(output_dir, "PSO_solucoes_completas.csv"), row.names = FALSE)
    utils::write.csv(nsga_solutions_df, file.path(output_dir, "NSGA_solucoes_completas.csv"), row.names = FALSE)
    utils::write.csv(best_global, file.path(output_dir, "MELHOR_RESULTADO_GLOBAL.csv"), row.names = FALSE)
    utils::write.csv(best_pso, file.path(output_dir, "MELHOR_RESULTADO_PSO.csv"), row.names = FALSE)
    utils::write.csv(best_nsga, file.path(output_dir, "MELHOR_RESULTADO_NSGA.csv"), row.names = FALSE)

    p_general <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = nsga_df,
        ggplot2::aes(x = RH_Total, y = Lucro),
        color = "steelblue",
        alpha = 0.6,
        size = 2
      ) +
      ggplot2::geom_point(
        data = pso_df,
        ggplot2::aes(x = RH_Total, y = Lucro, color = factor(omega)),
        alpha = 0.7,
        size = 2
      ) +
      ggplot2::facet_wrap(~ GW_Iteration) +
      ggplot2::labs(
        title = "Comparacao Geral - PSO vs NSGA-II com Growing Window",
        subtitle = "Azul = NSGA-II | Cores = PSO",
        x = "RH Total",
        y = "Lucro",
        color = "omega"
      ) +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = file.path(output_dir, "COMPARACAO_GERAL_PSO_NSGA_GW.png"),
      plot = p_general,
      width = 14,
      height = 8
    )

    txt_file <- file.path(output_dir, "ANALISE_FINAL_O3.txt")
    sink(txt_file)
    cat("=============================================\n")
    cat("ANALISE FINAL - O3\n")
    cat("Comparacao PSO vs NSGA-II com Growing Window\n")
    cat("=============================================\n\n")
    cat("Configuracao:\n")
    cat("Growing Window iterations:", n_gw, "\n")
    cat("Horizonte:", h, "dias\n")
    cat("Initial train:", initial_train, "\n")
    cat("PSO omegas:", paste(omegas, collapse = ", "), "\n")
    cat("RH limits:", paste(rh_limits, collapse = ", "), "\n\n")
    cat("=============================================\n")
    cat("MELHOR RESULTADO GLOBAL\n")
    cat("=============================================\n")
    print(best_global)
    cat("\n=============================================\n")
    cat("MELHOR RESULTADO PSO\n")
    cat("=============================================\n")
    print(best_pso)
    cat("\n=============================================\n")
    cat("MELHOR RESULTADO NSGA-II\n")
    cat("=============================================\n")
    print(best_nsga)
    sink()
  }

  list(
    pso_df = pso_df,
    pso_solutions_df = pso_solutions_df,
    nsga_df = nsga_df,
    nsga_solutions_df = nsga_solutions_df,
    all_pareto = all_pareto,
    best_global = best_global,
    best_pso = best_pso,
    best_nsga = best_nsga,
    output_dir = output_dir
  )
}
