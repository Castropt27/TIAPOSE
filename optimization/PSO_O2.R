
source("Funcao_eval.R")
source("repair.R")

install.packages("pso")
install.packages("forecast")

library(pso)
library(ggplot2)
library(forecast)

# -----------------------------
# Pastas
# -----------------------------
base_dir <- "C:/Users/Utilizador/Documents/eda_outputs"
output_dir <- file.path(base_dir, "Particle Swarm Growing Window")

if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# Dados
# -----------------------------
files  <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
stores <- c("baltimore", "lancaster", "philadelphia", "richmond")

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

is_weekend <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)



# -----------------------------
# Upper bounds
# -----------------------------
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

# -----------------------------
# Criar C_pred para cada semana
# -----------------------------
create_C_pred <- function(iter, initial_train = 574, h = 7) {
  start_idx <- initial_train + (iter - 1) * h + 1
  end_idx   <- start_idx + h - 1
  
  C_pred <- matrix(0, nrow = 4, ncol = 7)
  
  for (s in 1:4) {
    df <- data_list[[s]]
    C_pred[s, ] <- df$Num_Customers[start_idx:end_idx]
  }
  
  rownames(C_pred) <- stores
  colnames(C_pred) <- c("su", "mo", "tu", "we", "th", "fr", "sa")
  
  return(C_pred)
}

# -----------------------------
# Mostrar plano
# -----------------------------
show_plan <- function(s) {
  s <- round(s)
  days <- c("su", "mo", "tu", "we", "th", "fr", "sa")
  
  for (store in 1:4) {
    plan <- matrix(
      0,
      nrow = 3,
      ncol = 7,
      dimnames = list(c("J", "X", "PR"), days)
    )
    
    for (day in 1:7) {
      idx <- (store - 1) * 21 + (day - 1) * 3
      plan["J", day]  <- s[idx + 1]
      plan["X", day]  <- s[idx + 2]
      plan["PR", day] <- s[idx + 3]
    }
    
    cat("\n>>", stores[store], "\n")
    print(plan)
  }
}

# -----------------------------
# PSO Growing Window
# -----------------------------
run_pso_for_week <- function(C_pred, iter) {
  
  best_history <- c()
  
  lower <- rep(0, 84)
  upper <- calcupper(C_pred)
  
  objective_pso <- function(s) {
    s_repaired <- repair_solution(
      s = s,
      C_pred = C_pred,
      max_units_week = 10000
    )
    
    profit <- eval_solution(
      s = s_repaired,
      C_pred = C_pred,
      is_weekend = is_weekend,
      objective = "O2"
    )
    
    best_history <<- c(best_history, profit)
    
    return(-profit)
  }
  
  set.seed(123 + iter)
  
  pso_result <- psoptim(
    par = runif(84, lower, upper),
    fn = objective_pso,
    lower = lower,
    upper = upper,
    control = list(
      maxit = 100,
      s = 80
    )
  )
  
  best_s <- repair_solution(
    s = pso_result$par,
    C_pred = C_pred,
    max_units_week = 10000
  )
  
  best_score <- eval_solution(
    s = best_s,
    C_pred = C_pred,
    is_weekend = is_weekend,
    objective = "O2"
  )
  
  best_so_far <- cummax(best_history)
  
  conv_df <- data.frame(
    GW_Iteration = iter,
    Evaluation = 1:length(best_so_far),
    Best_Profit = best_so_far
  )
  
  return(list(
    iteration = iter,
    C_pred = C_pred,
    best_s = best_s,
    best_score = best_score,
    convergence = conv_df
  ))
}

# -----------------------------
# Correr 20 iterações
# -----------------------------
n_iter <- 20
h <- 7
initial_train <- 574

results_list <- list()

for (i in 1:n_iter) {
  cat("\n==============================\n")
  cat("Growing Window Iteration:", i, "\n")
  cat("==============================\n")
  
  C_pred_i <- create_C_pred(
    iter = i,
    initial_train = initial_train,
    h = h
  )
  
  results_list[[i]] <- run_pso_for_week(C_pred_i, i)
  
  cat("Melhor score:", results_list[[i]]$best_score, "\n")
}

# -----------------------------
# Resumo final
# -----------------------------
summary_df <- data.frame(
  Iteration = 1:n_iter,
  Best_Score = sapply(results_list, function(x) x$best_score)
)

final_summary <- data.frame(
  Method = "Particle Swarm Optimization",
  Objective = "O2",
  Score_Mean = mean(summary_df$Best_Score),
  Score_Median = median(summary_df$Best_Score),
  Score_Min = min(summary_df$Best_Score),
  Score_Max = max(summary_df$Best_Score)
)

print(summary_df)
print(final_summary)

# -----------------------------
# Convergência geral
# -----------------------------
all_convergence <- do.call(
  rbind,
  lapply(results_list, function(x) x$convergence)
)

p_conv <- ggplot(all_convergence, aes(x = Evaluation, y = Best_Profit, group = GW_Iteration)) +
  geom_line(alpha = 0.35) +
  labs(
    title = "Convergência - PSO com Growing Window",
    x = "Avaliações da função",
    y = "Melhor lucro encontrado"
  ) +
  theme_minimal()

print(p_conv)

ggsave(
  file.path(output_dir, "pso_growingwindow_convergencia.png"),
  plot = p_conv,
  width = 10,
  height = 6
)

# -----------------------------
# Guardar CSVs
# -----------------------------
write.csv(
  summary_df,
  file.path(output_dir, "pso_growingwindow_scores.csv"),
  row.names = FALSE
)

write.csv(
  final_summary,
  file.path(output_dir, "pso_growingwindow_summary.csv"),
  row.names = FALSE
)

write.csv(
  all_convergence,
  file.path(output_dir, "pso_growingwindow_convergencia.csv"),
  row.names = FALSE
)

# guardar melhor solução de cada iteração
best_solutions_df <- data.frame()

for (i in 1:n_iter) {
  temp <- data.frame(
    Iteration = i,
    Variable = paste0("x", 1:84),
    Value = round(results_list[[i]]$best_s)
  )
  
  best_solutions_df <- rbind(best_solutions_df, temp)
}

write.csv(
  best_solutions_df,
  file.path(output_dir, "pso_growingwindow_best_solutions.csv"),
  row.names = FALSE
)

# -----------------------------
# Guardar TXT
# -----------------------------
txt_file <- file.path(output_dir, "pso_growingwindow_resultados.txt")

sink(txt_file)

cat("=============================================\n")
cat("Particle Swarm Optimization - Growing Window\n")
cat("=============================================\n\n")

cat("Resumo por iteração:\n")
print(summary_df)

cat("\nResumo final:\n")
print(final_summary)

cat("\n\nPlanos finais por iteração:\n")

for (i in 1:n_iter) {
  cat("\n=============================================\n")
  cat("ITERATION:", i, "\n")
  cat("Best score:", results_list[[i]]$best_score, "\n")
  cat("=============================================\n")
  show_plan(results_list[[i]]$best_s)
}

sink()

cat("Tudo guardado em:\n", output_dir, "\n")