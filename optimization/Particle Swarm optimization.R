setwd("C:/Users/rodri/Desktop/Tiapose")

source("Funcao_eval.R")

library(ggplot2)

set.seed(123)

# -------------------------------------------------------
# 1. Carregar dados reais dos últimos 7 dias
# -------------------------------------------------------
files  <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
stores <- c("baltimore", "lancaster", "philadelphia", "richmond")

load_last_week <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]
  tail(df$Num_Customers, 7)
}

C_real <- t(sapply(files, load_last_week))
rownames(C_real) <- stores
colnames(C_real) <- c("su","mo","tu","we","th","fr","sa")

cat("Clientes reais dos últimos 7 dias:\n")
print(C_real)

is_weekend <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

# -------------------------------------------------------
# 2. Limites
# -------------------------------------------------------
calcupper <- function(prev) {
  upper <- numeric(84)
  
  for (store in 1:4) {
    for (day in 1:7) {
      
      idx <- (store - 1) * 21 + (day - 1) * 3
      C   <- prev[store, day]
      
      upper[idx + 1] <- ceiling(C / 6) + 2   # J
      upper[idx + 2] <- ceiling(C / 7) + 2   # X
      upper[idx + 3] <- 30                   # PR
    }
  }
  
  upper
}

lower <- rep(0, 84)
upper <- calcupper(C_real)

cat("\nLimites por loja (max dia):\n")

for (store in 1:4) {
  
  max_idx_J <- seq((store - 1) * 21 + 1,
                   (store - 1) * 21 + 19,
                   by = 3)
  
  max_idx_X <- seq((store - 1) * 21 + 2,
                   (store - 1) * 21 + 20,
                   by = 3)
  
  cat("  ", stores[store],
      "— max J:", max(upper[max_idx_J]),
      "| max X:", max(upper[max_idx_X]),
      "| max clientes:", max(C_real[store, ]),
      "\n")
}

# -------------------------------------------------------
# 3. PSO
# -------------------------------------------------------
pso <- function(N_particles, N_iter,
                C_pred, is_weekend,
                objective,
                lower, upper,
                w = 0.7,
                c1 = 1.5,
                c2 = 1.5) {
  
  D <- length(lower)
  
  # ---------------------------
  # Inicialização
  # ---------------------------
  
  pos <- matrix(0, nrow = N_particles, ncol = D)
  vel <- matrix(0, nrow = N_particles, ncol = D)
  
  for (i in 1:N_particles) {
    
    pos[i, ] <- round(runif(D, min = lower, max = upper))
    vel[i, ] <- runif(D, min = -1, max = 1)
  }
  
  pbest       <- pos
  pbest_score <- rep(-Inf, N_particles)
  
  gbest       <- pos[1, ]
  gbest_score <- -Inf
  
  # ---------------------------
  # Avaliar população inicial
  # ---------------------------
  
  for (i in 1:N_particles) {
    
    score <- eval_solution(
      pos[i, ],
      C_pred,
      is_weekend,
      objective
    )
    
    if (is.finite(score)) {
      
      pbest_score[i] <- score
      
      if (score > gbest_score) {
        gbest_score <- score
        gbest       <- pos[i, ]
      }
    }
  }
  
  history    <- numeric(N_iter)
  history[1] <- ifelse(is.finite(gbest_score), gbest_score, NA)
  
  # ---------------------------
  # Loop principal PSO
  # ---------------------------
  
  for (iter in 2:N_iter) {
    
    for (i in 1:N_particles) {
      
      r1 <- runif(D)
      r2 <- runif(D)
      
      # velocidade
      vel[i, ] <- w  * vel[i, ] +
        c1 * r1 * (pbest[i, ] - pos[i, ]) +
        c2 * r2 * (gbest       - pos[i, ])
      
      # posição
      pos[i, ] <- round(pos[i, ] + vel[i, ])
      
      # limites
      pos[i, ] <- pmax(lower, pmin(upper, pos[i, ]))
      
      # score
      score <- eval_solution(
        pos[i, ],
        C_pred,
        is_weekend,
        objective
      )
      
      if (is.finite(score)) {
        
        # melhor pessoal
        if (score > pbest_score[i]) {
          
          pbest_score[i] <- score
          pbest[i, ]     <- pos[i, ]
        }
        
        # melhor global
        if (score > gbest_score) {
          
          gbest_score <- score
          gbest       <- pos[i, ]
        }
      }
    }
    
    history[iter] <- ifelse(is.finite(gbest_score),
                            gbest_score,
                            NA)
    
    if (iter %% 10 == 0) {
      cat("Iter:", iter,
          "| Melhor score:", round(gbest_score, 2),
          "\n")
    }
  }
  
  list(
    best_s     = gbest,
    best_score = gbest_score,
    history    = history
  )
}

# -------------------------------------------------------
# 4. Executar APENAS O1
# -------------------------------------------------------
N_particles <- 30
N_iter      <- 200

cat("\n========================================\n")
cat("PSO — O1\n")
cat("========================================\n")

res <- pso(
  N_particles = N_particles,
  N_iter      = N_iter,
  C_pred      = C_real,
  is_weekend  = is_weekend,
  objective   = "O1",
  lower       = lower,
  upper       = upper
)

cat("\n========================================\n")
cat("MELHOR RESULTADO O1\n")
cat("========================================\n")

cat("Melhor score:", res$best_score, "\n")

# -------------------------------------------------------
# 5. Gráfico de convergência
# -------------------------------------------------------
df_conv <- data.frame(
  Iteracao = 1:N_iter,
  Score    = res$history
)

df_conv$Score[!is.finite(df_conv$Score)] <- NA

p <- ggplot(df_conv,
            aes(x = Iteracao,
                y = Score)) +
  
  geom_line(
    colour = "#378ADD",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "PSO — Convergência O1",
    
    subtitle = paste0(
      "Partículas = ", N_particles,
      " | Iterações = ", N_iter,
      " | w=0.7, c1=1.5, c2=1.5"
    ),
    
    x = "Iteração",
    y = "Melhor score"
  ) +
  
  theme_minimal(base_size = 12)

print(p)

# -------------------------------------------------------
# 6. Mostrar melhor plano
# -------------------------------------------------------
show_plan <- function(s,
                      store_name = NULL,
                      stores = c("baltimore",
                                 "lancaster",
                                 "philadelphia",
                                 "richmond")) {
  
  s    <- round(s)
  days <- c("su","mo","tu","we","th","fr","sa")
  
  if (is.null(store_name)) {
    
    for (st in 1:4) {
      show_plan(s, stores[st], stores)
    }
    
    return(invisible(NULL))
  }
  
  st_idx <- match(store_name, stores)
  
  plan <- matrix(
    0,
    nrow = 3,
    ncol = 7,
    dimnames = list(c("J","X","PR"), days)
  )
  
  for (day in 1:7) {
    
    idx <- (st_idx - 1) * 21 + (day - 1) * 3
    
    plan["J",  day] <- s[idx + 1]
    plan["X",  day] <- s[idx + 2]
    plan["PR", day] <- s[idx + 3]
  }
  
  cat("\n========================================\n")
  cat(store_name, "\n")
  cat("========================================\n")
  
  print(plan)
}

cat("\n========================================\n")
cat("MELHOR PLANO O1\n")
cat("========================================\n")

show_plan(res$best_s)