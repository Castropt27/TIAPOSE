# ============================================================
# O3 — PSO vs NSGA-II com Growing Window
# ============================================================

setwd("C:/Users/rodri/Desktop/Tiapose")

source("Funcao_eval.R")
source("repair.R")

library(pso)
library(mco)
library(forecast)
library(ggplot2)
library(dplyr)

# ============================================================
# Pasta de resultados
# ============================================================

output_dir <- "C:/Users/rodri/Desktop/Tiapose/o3valorfinal"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================
# Dados
# ============================================================

files  <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
stores <- c("baltimore", "lancaster", "philadelphia", "richmond")

is_weekend <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

load_data <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]
  
  df$Num_Customers[df$Num_Customers < 0] <- NA
  df$Num_Customers <- as.numeric(na.interp(ts(df$Num_Customers, frequency = 7)))
  
  return(df)
}

data_list <- lapply(files, load_data)
names(data_list) <- stores

# ============================================================
# Growing Window — criar C_pred
# ============================================================

create_C_pred <- function(gw_iter, initial_train = 574, h = 7) {
  start_idx <- initial_train + (gw_iter - 1) * h + 1
  end_idx   <- start_idx + h - 1
  
  C_pred <- matrix(0, nrow = 4, ncol = 7)
  
  for (s in 1:4) {
    df <- data_list[[s]]
    
    if (end_idx > nrow(df)) {
      stop("Não há dados suficientes para esta iteração growing window.")
    }
    
    C_pred[s, ] <- df$Num_Customers[start_idx:end_idx]
  }
  
  rownames(C_pred) <- stores
  colnames(C_pred) <- c("su", "mo", "tu", "we", "th", "fr", "sa")
  
  return(C_pred)
}

# ============================================================
# Upper bounds
# ============================================================

calcupper <- function(prev) {
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
  
  return(upper)
}

# ============================================================
# RH total
# ============================================================

calc_rh_total <- function(s) {
  J_idx <- seq(1, 84, by = 3)
  X_idx <- seq(2, 84, by = 3)
  
  sum(round(s[J_idx])) + sum(round(s[X_idx]))
}

# ============================================================
# PSO com limite de RH
# ============================================================

run_pso_rh_limit <- function(C_pred, rh_limit, omega, gw_iter, objective_name = "O3") {
  
  lower <- rep(0, 84)
  upper <- calcupper(C_pred)
  
  objective_pso <- function(s) {
    
    s_repaired <- repair_solution(
      s = s,
      C_pred = C_pred,
      max_units_week = 10000
    )
    
    rh_total <- calc_rh_total(s_repaired)
    
    score <- eval_solution(
      s = s_repaired,
      C_pred = C_pred,
      is_weekend = is_weekend,
      objective = objective_name
    )
    
    if (rh_total > rh_limit) {
      penalty <- 10000 * (rh_total - rh_limit)
      score <- score - penalty
    }
    
    return(-score)
  }
  
  result <- psoptim(
    par = runif(84, lower, upper),
    fn = objective_pso,
    lower = lower,
    upper = upper,
    control = list(
      maxit = 150,
      s = 60,
      w = omega,
      c.p = 1.5,
      c.g = 1.5
    )
  )
  
  best_s <- repair_solution(
    s = result$par,
    C_pred = C_pred,
    max_units_week = 10000
  )
  
  rh_total <- calc_rh_total(best_s)
  
  best_score <- eval_solution(
    s = best_s,
    C_pred = C_pred,
    is_weekend = is_weekend,
    objective = objective_name
  )
  
  result_df <- data.frame(
    Metodo = "PSO",
    GW_Iteration = gw_iter,
    omega = omega,
    RH_Limit = rh_limit,
    RH_Total = rh_total,
    Lucro = best_score
  )
  
  solution_df <- data.frame(
    Metodo = "PSO",
    GW_Iteration = gw_iter,
    omega = omega,
    RH_Limit = rh_limit,
    Variable = paste0("x", 1:84),
    Value = round(best_s)
  )
  
  return(list(
    result = result_df,
    solution = solution_df
  ))
}

# ============================================================
# NSGA-II
# Objetivo 1: minimizar RH_Total
# Objetivo 2: maximizar Lucro -> minimizar -Lucro
# ============================================================

run_nsga2_week <- function(C_pred, gw_iter, objective_name = "O3") {
  
  lower <- rep(0, 84)
  upper <- calcupper(C_pred)
  
  nsga_objective <- function(s) {
    
    s_repaired <- repair_solution(
      s = s,
      C_pred = C_pred,
      max_units_week = 10000
    )
    
    rh_total <- calc_rh_total(s_repaired)
    
    lucro <- eval_solution(
      s = s_repaired,
      C_pred = C_pred,
      is_weekend = is_weekend,
      objective = objective_name
    )
    
    return(c(rh_total, -lucro))
  }
  
  set.seed(100 + gw_iter)
  
  nsga_res <- mco::nsga2(
    fn = nsga_objective,
    idim = 84,
    odim = 2,
    lower.bounds = lower,
    upper.bounds = upper,
    popsize = 60,
    generations = 100
  )
  
  pareto_idx <- which(nsga_res$pareto.optimal)
  
  pareto_values <- nsga_res$value[pareto_idx, ]
  pareto_pars   <- nsga_res$par[pareto_idx, ]
  
  nsga_df <- data.frame(
    Metodo = "NSGA-II",
    GW_Iteration = gw_iter,
    omega = NA,
    RH_Limit = NA,
    RH_Total = pareto_values[, 1],
    Lucro = -pareto_values[, 2]
  )
  
  nsga_df <- nsga_df %>%
    arrange(RH_Total)
  
  solution_df <- data.frame()
  
  for (i in 1:nrow(pareto_pars)) {
    sol_rep <- repair_solution(
      s = pareto_pars[i, ],
      C_pred = C_pred,
      max_units_week = 10000
    )
    
    temp <- data.frame(
      Metodo = "NSGA-II",
      GW_Iteration = gw_iter,
      Solution_ID = i,
      Variable = paste0("x", 1:84),
      Value = round(sol_rep)
    )
    
    solution_df <- rbind(solution_df, temp)
  }
  
  return(list(
    result = nsga_df,
    solution = solution_df
  ))
}

# ============================================================
# Configurações
# ============================================================

initial_train <- 574
h <- 7
n_gw <- 10

rh_limits <- seq(45, 95, by = 5)
omegas <- c(0.1, 0.5, 0.7)

pso_df <- data.frame()
pso_solutions_df <- data.frame()

nsga_df <- data.frame()
nsga_solutions_df <- data.frame()

# ============================================================
# Correr Growing Window
# ============================================================

for (gw in 1:n_gw) {
  
  cat("\n=============================\n")
  cat("GROWING WINDOW:", gw, "\n")
  cat("=============================\n")
  
  C_pred <- create_C_pred(
    gw_iter = gw,
    initial_train = initial_train,
    h = h
  )
  
  # -------------------------
  # PSO
  # -------------------------
  
  for (omega in omegas) {
    for (rh in rh_limits) {
      
      cat("PSO | GW:", gw, "| Omega:", omega, "| RH limit:", rh, "\n")
      
      temp <- run_pso_rh_limit(
        C_pred = C_pred,
        rh_limit = rh,
        omega = omega,
        gw_iter = gw,
        objective_name = "O3"
      )
      
      pso_df <- rbind(pso_df, temp$result)
      pso_solutions_df <- rbind(pso_solutions_df, temp$solution)
    }
  }
  
  # -------------------------
  # NSGA-II
  # -------------------------
  
  cat("NSGA-II | GW:", gw, "\n")
  
  nsga_temp <- run_nsga2_week(
    C_pred = C_pred,
    gw_iter = gw,
    objective_name = "O3"
  )
  
  nsga_df <- rbind(nsga_df, nsga_temp$result)
  nsga_solutions_df <- rbind(nsga_solutions_df, nsga_temp$solution)
  
  # -------------------------
  # Gráfico semanal
  # -------------------------
  
  pso_week <- pso_df %>%
    filter(GW_Iteration == gw)
  
  nsga_week <- nsga_df %>%
    filter(GW_Iteration == gw)
  
  p_week <- ggplot() +
    geom_point(
      data = nsga_week,
      aes(x = RH_Total, y = Lucro),
      color = "steelblue",
      size = 2
    ) +
    geom_line(
      data = nsga_week,
      aes(x = RH_Total, y = Lucro),
      color = "steelblue",
      linewidth = 0.8
    ) +
    geom_point(
      data = pso_week,
      aes(x = RH_Total, y = Lucro, color = factor(omega)),
      size = 2
    ) +
    geom_line(
      data = pso_week,
      aes(x = RH_Total, y = Lucro, color = factor(omega), group = omega),
      linewidth = 0.7
    ) +
    labs(
      title = paste("Comparação Pareto — PSO vs NSGA-II | GW", gw),
      subtitle = "Azul = NSGA-II | Cores = PSO",
      x = "RH Total",
      y = "Lucro",
      color = "ω"
    ) +
    theme_minimal()
  
  print(p_week)
  
  ggsave(
    filename = file.path(output_dir, paste0("GW_", gw, "_PSO_vs_NSGA.png")),
    plot = p_week,
    width = 10,
    height = 6
  )
}

# ============================================================
# Juntar resultados
# ============================================================

all_pareto <- rbind(
  nsga_df %>% select(Metodo, GW_Iteration, omega, RH_Limit, RH_Total, Lucro),
  pso_df %>% select(Metodo, GW_Iteration, omega, RH_Limit, RH_Total, Lucro)
)

# ============================================================
# Guardar CSVs
# ============================================================

write.csv(pso_df, file.path(output_dir, "PSO_resultados_completos.csv"), row.names = FALSE)
write.csv(nsga_df, file.path(output_dir, "NSGA_resultados_completos.csv"), row.names = FALSE)
write.csv(all_pareto, file.path(output_dir, "Comparacao_PSO_NSGA_completa.csv"), row.names = FALSE)

write.csv(pso_solutions_df, file.path(output_dir, "PSO_solucoes_completas.csv"), row.names = FALSE)
write.csv(nsga_solutions_df, file.path(output_dir, "NSGA_solucoes_completas.csv"), row.names = FALSE)

# ============================================================
# Melhor resultado global
# ============================================================

best_global <- all_pareto %>%
  arrange(desc(Lucro)) %>%
  slice(1)

best_pso <- pso_df %>%
  arrange(desc(Lucro)) %>%
  slice(1)

best_nsga <- nsga_df %>%
  arrange(desc(Lucro)) %>%
  slice(1)

write.csv(best_global, file.path(output_dir, "MELHOR_RESULTADO_GLOBAL.csv"), row.names = FALSE)
write.csv(best_pso, file.path(output_dir, "MELHOR_RESULTADO_PSO.csv"), row.names = FALSE)
write.csv(best_nsga, file.path(output_dir, "MELHOR_RESULTADO_NSGA.csv"), row.names = FALSE)

# ============================================================
# Gráfico geral
# ============================================================

p_general <- ggplot() +
  geom_point(
    data = nsga_df,
    aes(x = RH_Total, y = Lucro),
    color = "steelblue",
    alpha = 0.6,
    size = 2
  ) +
  geom_point(
    data = pso_df,
    aes(x = RH_Total, y = Lucro, color = factor(omega)),
    alpha = 0.7,
    size = 2
  ) +
  facet_wrap(~ GW_Iteration) +
  labs(
    title = "Comparação Geral — PSO vs NSGA-II com Growing Window",
    subtitle = "Azul = NSGA-II | Cores = PSO",
    x = "RH Total",
    y = "Lucro",
    color = "ω"
  ) +
  theme_minimal()

print(p_general)

ggsave(
  filename = file.path(output_dir, "COMPARACAO_GERAL_PSO_NSGA_GW.png"),
  plot = p_general,
  width = 14,
  height = 8
)

# ============================================================
# TXT com análise
# ============================================================

txt_file <- file.path(output_dir, "ANALISE_FINAL_O3.txt")

sink(txt_file)

cat("=============================================\n")
cat("ANÁLISE FINAL — O3\n")
cat("Comparação PSO vs NSGA-II com Growing Window\n")
cat("=============================================\n\n")

cat("Configuração:\n")
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

cat("\n=============================================\n")
cat("RESUMO MÉDIO POR MÉTODO\n")
cat("=============================================\n")

summary_method <- all_pareto %>%
  group_by(Metodo) %>%
  summarise(
    Mean_Lucro = mean(Lucro),
    Median_Lucro = median(Lucro),
    Max_Lucro = max(Lucro),
    Mean_RH = mean(RH_Total),
    Min_RH = min(RH_Total),
    Max_RH = max(RH_Total),
    .groups = "drop"
  )

print(summary_method)

cat("\n=============================================\n")
cat("ANÁLISE\n")
cat("=============================================\n\n")

if (best_global$Metodo == "PSO") {
  cat("O melhor resultado global foi obtido pelo PSO.\n")
} else {
  cat("O melhor resultado global foi obtido pelo NSGA-II.\n")
}

cat("\nO NSGA-II gera uma fronteira de Pareto completa, permitindo observar várias soluções de compromisso entre RH total e lucro.\n")
cat("O PSO foi executado várias vezes com diferentes valores de omega e limites de RH, aproximando uma fronteira de Pareto através de várias execuções.\n")
cat("A comparação em growing window permite avaliar a estabilidade dos métodos ao longo de várias semanas, em vez de depender apenas de uma única semana.\n")
cat("Os ficheiros CSV e gráficos guardados nesta pasta permitem analisar separadamente cada semana e comparar os métodos globalmente.\n")

sink()

cat("\nTudo guardado em:\n")
cat(output_dir, "\n")