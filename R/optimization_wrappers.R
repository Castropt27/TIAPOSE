# ============================================================
# optimization_wrappers.R
# Wrappers para os objetivos de otimização O1, O2 e O3
# ============================================================

if (file.exists(file.path("R", "data_helpers.R"))) {
  source(file.path("R", "data_helpers.R"))
}

if (file.exists(file.path("R", "legacy_optimization.R"))) {
  source(file.path("R", "legacy_optimization.R"))
}

# is_weekend: domingo=TRUE, segunda a sexta=FALSE, sábado=TRUE
IS_WEEKEND <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

STORE_NAMES <- c("baltimore", "lancaster", "philadelphia", "richmond")
DAY_NAMES   <- c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sáb")

# ============================================================
# eval_solution e repair_solution — implementação local
# (substitui Funcao_eval.R e repair.R quando não disponíveis)
# ============================================================

# Parâmetros do negócio
PRICE_PER_UNIT    <- 25.0    # preço de venda por unidade
COST_J            <- 80.0    # custo diário junior
COST_X            <- 100.0   # custo diário outro tipo de RH
COST_PR_UNIT      <- 2.0     # custo por unidade em promoção
UNITS_PER_J       <- 6.0     # unidades que um J consegue servir
UNITS_PER_X       <- 7.0     # unidades que um X consegue servir
PR_BOOST_FACTOR   <- 0.10    # boost por unidade de PR (%)
MAX_PROFIT        <- 10000   # teto de lucro para O2/O3

# -------------------------------------------------------
# Calcular unidades vendidas num dia
# -------------------------------------------------------
calc_units_day <- function(J, X, PR, C, is_weekend_day) {
  J  <- max(0, round(J))
  X  <- max(0, round(X))
  PR <- max(0, round(PR))
  C  <- max(0, C)
  
  # Capacidade base de servir clientes
  capacity <- J * UNITS_PER_J + X * UNITS_PER_X
  
  # Boost de promoção
  boost <- 1 + PR_BOOST_FACTOR * PR
  
  # Demanda efetiva com boost
  demand  <- C * boost
  
  # Vendas = mínimo entre capacidade e demanda
  units_sold <- min(capacity, demand)
  
  # Multiplicador fim de semana
  if (is_weekend_day) units_sold <- units_sold * 1.10
  
  return(floor(units_sold))
}

# -------------------------------------------------------
# Avaliar uma solução completa
# -------------------------------------------------------
eval_solution_local <- function(s, C_pred, is_weekend, objective = "O1") {
  
  total_profit   <- 0
  total_hr       <- 0
  total_units    <- 0
  total_revenue  <- 0
  total_cost     <- 0
  
  for (store in 1:4) {
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      
      J  <- max(0, round(s[idx + 1]))
      X  <- max(0, round(s[idx + 2]))
      PR <- max(0, round(s[idx + 3]))
      C  <- C_pred[store, day]
      
      units   <- calc_units_day(J, X, PR, C, is_weekend[day])
      revenue <- units * PRICE_PER_UNIT
      cost    <- J * COST_J + X * COST_X + PR * COST_PR_UNIT
      profit  <- revenue - cost
      
      total_profit  <- total_profit  + profit
      total_hr      <- total_hr      + J + X
      total_units   <- total_units   + units
      total_revenue <- total_revenue + revenue
      total_cost    <- total_cost    + cost
    }
  }
  
  if (objective == "O1") return(total_profit)
  if (objective == "O2") return(min(total_profit, MAX_PROFIT) / max(1, total_hr))
  if (objective == "O3") return(min(total_profit, MAX_PROFIT))   # multi-obj usa lucro capado + RH
  
  return(total_profit)
}

# -------------------------------------------------------
# Repair de solução
# -------------------------------------------------------
repair_solution_local <- function(s, C_pred, max_units_week = 10000) {
  s_rep <- round(pmax(0, s))
  
  for (store in 1:4) {
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      C   <- C_pred[store, day]
      
      max_J  <- ceiling(C / UNITS_PER_J)  + 2
      max_X  <- ceiling(C / UNITS_PER_X)  + 2
      max_PR <- 30
      
      s_rep[idx + 1] <- min(s_rep[idx + 1], max_J)
      s_rep[idx + 2] <- min(s_rep[idx + 2], max_X)
      s_rep[idx + 3] <- min(s_rep[idx + 3], max_PR)
    }
  }
  
  return(s_rep)
}

# -------------------------------------------------------
# Usar funções externas se disponíveis, caso contrário locais
# -------------------------------------------------------
eval_solution <- function(s, C_pred, is_weekend, objective = "O1") {
  eval_solution_local(s, C_pred, is_weekend, objective)
}

repair_solution <- function(s, C_pred, max_units_week = 10000) {
  repair_solution_local(s, C_pred, max_units_week)
}

# -------------------------------------------------------
# Limites superiores
# -------------------------------------------------------
calc_upper <- function(C_pred) {
  upper <- numeric(84)
  for (store in 1:4) {
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      C   <- C_pred[store, day]
      upper[idx + 1] <- ceiling(C / UNITS_PER_J)  + 2
      upper[idx + 2] <- ceiling(C / UNITS_PER_X)  + 2
      upper[idx + 3] <- 30
    }
  }
  return(upper)
}

# -------------------------------------------------------
# RH total
# -------------------------------------------------------
calc_rh_total <- function(s) {
  J_idx <- seq(1, 84, by = 3)
  X_idx <- seq(2, 84, by = 3)
  sum(round(s[J_idx])) + sum(round(s[X_idx]))
}

# ============================================================
# PSO interno (não precisa de pacote pso)
# ============================================================
run_pso_internal <- function(C_pred, objective, N_particles = 30,
                              N_iter = 100, w = 0.7, c1 = 1.5, c2 = 1.5,
                              progress_fn = NULL) {
  
  lower <- rep(0, 84)
  upper <- calc_upper(C_pred)
  D     <- 84
  
  pos <- matrix(0, nrow = N_particles, ncol = D)
  vel <- matrix(0, nrow = N_particles, ncol = D)
  
  for (i in 1:N_particles) {
    pos[i, ] <- round(runif(D, min = lower, max = upper))
    vel[i, ] <- runif(D, -1, 1)
  }
  
  pbest       <- pos
  pbest_score <- rep(-Inf, N_particles)
  gbest       <- pos[1, ]
  gbest_score <- -Inf
  
  for (i in 1:N_particles) {
    s_rep <- repair_solution(pos[i, ], C_pred)
    score <- eval_solution(s_rep, C_pred, IS_WEEKEND, objective)
    if (is.finite(score)) {
      pbest_score[i] <- score
      if (score > gbest_score) { gbest_score <- score; gbest <- s_rep }
    }
  }
  
  history <- numeric(N_iter)
  
  for (iter in 1:N_iter) {
    for (i in 1:N_particles) {
      r1 <- runif(D); r2 <- runif(D)
      vel[i, ] <- w * vel[i, ] +
                  c1 * r1 * (pbest[i, ] - pos[i, ]) +
                  c2 * r2 * (gbest - pos[i, ])
      pos[i, ] <- round(pos[i, ] + vel[i, ])
      pos[i, ] <- pmax(lower, pmin(upper, pos[i, ]))
      
      s_rep <- repair_solution(pos[i, ], C_pred)
      score <- eval_solution(s_rep, C_pred, IS_WEEKEND, objective)
      
      if (is.finite(score)) {
        if (score > pbest_score[i]) { pbest_score[i] <- score; pbest[i, ] <- s_rep }
        if (score > gbest_score)    { gbest_score <- score; gbest <- s_rep }
      }
    }
    history[iter] <- ifelse(is.finite(gbest_score), gbest_score, NA)
    
    if (!is.null(progress_fn)) progress_fn(iter / N_iter)
  }
  
  list(best_s = repair_solution(gbest, C_pred),
       best_score = gbest_score,
       history = history)
}

# ============================================================
# Construir tabela de plano semanal a partir de solução
# ============================================================
build_plan_table <- function(s, C_pred) {
  rows <- list()
  
  for (store in 1:4) {
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      
      J  <- max(0, round(s[idx + 1]))
      X  <- max(0, round(s[idx + 2]))
      PR <- max(0, round(s[idx + 3]))
      C  <- C_pred[store, day]
      
      units   <- calc_units_day(J, X, PR, C, IS_WEEKEND[day])
      revenue <- units * PRICE_PER_UNIT
      cost    <- J * COST_J + X * COST_X + PR * COST_PR_UNIT
      profit  <- revenue - cost
      
      rows[[length(rows) + 1]] <- data.frame(
        Dia      = DAY_NAMES[day],
        Cidade   = STORE_NAMES[store],
        Clientes = C,
        J        = J,
        X        = X,
        PR       = PR,
        Unidades = units,
        Vendas   = revenue,
        Custos   = cost,
        Lucro    = profit,
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, rows)
}

# ============================================================
# Resumo de uma solução
# ============================================================
build_summary <- function(s, C_pred, objective, city, week_label) {
  plan  <- build_plan_table(s, C_pred)
  rh    <- calc_rh_total(s)
  score <- eval_solution(s, C_pred, IS_WEEKEND, objective)
  
  data.frame(
    Objetivo         = objective,
    Cidade           = city,
    Semana           = week_label,
    Lucro_Total      = sum(plan$Lucro),
    RH_Total         = rh,
    Unidades_Vendidas= sum(plan$Unidades),
    Valor_Objetivo   = round(score, 2),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# O1 — Maximizar Lucro  [IMPLEMENTADO]
# ============================================================
run_O1 <- function(C_pred, N_particles = 30, N_iter = 100,
                   city = "todas", week_label = "Semana") {
  set.seed(42)
  result <- run_pso_internal(C_pred, "O1", N_particles, N_iter)
  
  plan    <- build_plan_table(result$best_s, C_pred)
  summary <- build_summary(result$best_s, C_pred, "O1", city, week_label)
  
  list(
    status    = "ok",
    objective = "O1",
    score     = result$best_score,
    summary   = summary,
    plan      = plan,
    history   = result$history,
    solution  = result$best_s
  )
}

# ============================================================
# O2 — Maximizar Lucro / RH  [IMPLEMENTADO]
# ============================================================
run_O2 <- function(C_pred, N_particles = 30, N_iter = 100,
                   city = "todas", week_label = "Semana") {
  set.seed(42)
  result <- run_pso_internal(C_pred, "O2", N_particles, N_iter)
  
  plan    <- build_plan_table(result$best_s, C_pred)
  summary <- build_summary(result$best_s, C_pred, "O2", city, week_label)
  
  list(
    status    = "ok",
    objective = "O2",
    score     = result$best_score,
    summary   = summary,
    plan      = plan,
    history   = result$history,
    solution  = result$best_s
  )
}

# ============================================================
# O3 — Multi-objetivo: Lucro vs RH (NSGA-II via mco)  [IMPLEMENTADO se mco disponível]
# ============================================================
run_O3 <- function(C_pred, city = "todas", week_label = "Semana",
                   popsize = 40, generations = 60) {
  
  if (!requireNamespace("mco", quietly = TRUE)) {
    return(list(
      status  = "not_implemented",
      message = "O objetivo O3 requer o pacote 'mco'. Instale com: install.packages('mco')"
    ))
  }
  
  lower <- rep(0, 84)
  upper <- calc_upper(C_pred)
  
  nsga_obj <- function(s) {
    s_rep  <- repair_solution(s, C_pred)
    rh     <- calc_rh_total(s_rep)
    lucro  <- eval_solution(s_rep, C_pred, IS_WEEKEND, "O3")
    c(rh, -lucro)
  }
  
  set.seed(42)
  
  tryCatch({
    res <- mco::nsga2(
      fn           = nsga_obj,
      idim         = 84,
      odim         = 2,
      lower.bounds = lower,
      upper.bounds = upper,
      popsize      = popsize,
      generations  = generations
    )
    
    pareto_idx  <- which(res$pareto.optimal)
    par_values  <- res$value[pareto_idx, ]
    par_pars    <- res$par[pareto_idx, ]
    
    # Garantir que é matrix
    if (is.null(dim(par_values))) par_values <- matrix(par_values, nrow = 1)
    if (is.null(dim(par_pars)))   par_pars   <- matrix(par_pars,   nrow = 1)
    
    pareto_df <- data.frame(
      solution_id      = seq_len(nrow(par_values)),
      Lucro            = round(-par_values[, 2], 2),
      Total_HR         = round( par_values[, 1], 0),
      stringsAsFactors = FALSE
    )
    pareto_df <- pareto_df[order(pareto_df$Total_HR), ]
    pareto_df$Unidades_Vendidas <- 0
    
    for (i in seq_len(nrow(par_pars))) {
      s_rep <- repair_solution(par_pars[i, ], C_pred)
      plan  <- build_plan_table(s_rep, C_pred)
      pareto_df$Unidades_Vendidas[i] <- sum(plan$Unidades)
    }
    
    # Solução recomendada: perto da mediana de HR
    med_hr <- median(pareto_df$Total_HR)
    rec_id <- which.min(abs(pareto_df$Total_HR - med_hr))
    
    # Plano da solução recomendada
    rec_s    <- repair_solution(par_pars[rec_id, ], C_pred)
    rec_plan <- build_plan_table(rec_s, C_pred)
    rec_sum  <- build_summary(rec_s, C_pred, "O3", city, week_label)
    
    list(
      status          = "ok",
      objective       = "O3",
      pareto_df       = pareto_df,
      recommended_id  = rec_id,
      recommended_summary = rec_sum,
      recommended_plan    = rec_plan
    )
    
  }, error = function(e) {
    list(
      status  = "error",
      message = paste("Erro no NSGA-II:", conditionMessage(e))
    )
  })
}

# ============================================================
# Dispatcher principal de otimização
# ============================================================
run_optimization <- function(objective, city, week_choice,
                              N_particles = 30, N_iter = 100,
                              use_legacy = FALSE,
                              rh_limit = NA_real_,
                              omega = 0.7,
                              popsize = 60,
                              generations = 100) {
  
  tryCatch({
    C_pred <- build_C_pred(week_choice)
    
    idx     <- resolve_week_index(load_city_data("baltimore"), week_choice)
    week_label <- paste("Semana", idx)
    
    if (objective == "O1") {
      return(run_O1(C_pred, N_particles, N_iter, city, week_label))
    } else if (objective == "O2") {
      if (isTRUE(use_legacy)) {
        if (!exists("legacy_run_pso_week", mode = "function")) {
          stop("Modulo legado nao esta disponivel.")
        }

        legacy_res <- legacy_run_pso_week(
          C_pred = C_pred,
          gw_iter = idx,
          omega = omega,
          rh_limit = rh_limit,
          objective_name = "O2",
          maxit = N_iter,
          swarm_size = N_particles
        )

        legacy_res$summary$Cidade <- city
        legacy_res$summary$Semana <- week_label
        return(legacy_res)
      }
      return(run_O2(C_pred, N_particles, N_iter, city, week_label))
    } else if (objective == "O3") {
      if (isTRUE(use_legacy)) {
        if (!exists("legacy_run_nsga2_week", mode = "function")) {
          stop("Modulo legado nao esta disponivel.")
        }

        legacy_res <- legacy_run_nsga2_week(
          C_pred = C_pred,
          gw_iter = idx,
          objective_name = "O3",
          popsize = popsize,
          generations = generations
        )

        legacy_res$recommended_summary$Cidade <- city
        legacy_res$recommended_summary$Semana <- week_label
        return(legacy_res)
      }
      return(run_O3(C_pred, city, week_label))
    } else {
      return(list(
        status  = "not_implemented",
        message = paste0("O objetivo '", objective, "' ainda não está implementado.")
      ))
    }
    
  }, error = function(e) {
    list(
      status  = "error",
      message = paste("Erro na otimização:", conditionMessage(e))
    )
  })
}
