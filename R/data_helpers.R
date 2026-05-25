# ============================================================
# data_helpers.R
# Funções auxiliares para leitura e preparação dos dados
# ============================================================

# Cidades disponíveis
CITIES <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Caminho base dos dados (relativo ao app.R)
DATA_DIR <- "data"

# -------------------------------------------------------
# Carregar CSV de uma cidade
# -------------------------------------------------------
load_city_data <- function(city) {
  path <- file.path(DATA_DIR, paste0(city, ".csv"))
  
  if (!file.exists(path)) {
    stop(paste("Ficheiro não encontrado:", path))
  }
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]
  
  # Garantir coluna Num_Customers
  if (!"Num_Customers" %in% colnames(df)) {
    stop("Coluna 'Num_Customers' não encontrada no CSV.")
  }
  
  return(df)
}

# -------------------------------------------------------
# Obter semanas disponíveis nos dados
# -------------------------------------------------------
get_weeks <- function(df) {
  n <- nrow(df)
  n_weeks <- floor(n / 7)
  weeks <- seq_len(n_weeks)
  names(weeks) <- paste("Semana", seq_len(n_weeks))
  return(weeks)
}

# -------------------------------------------------------
# Extrair uma semana do dataframe (por índice)
# -------------------------------------------------------
get_week_slice <- function(df, week_index) {
  n <- nrow(df)
  n_weeks <- floor(n / 7)
  
  if (week_index < 1 || week_index > n_weeks) {
    stop(paste("Índice de semana inválido:", week_index,
               "— disponíveis: 1 a", n_weeks))
  }
  
  start <- (week_index - 1) * 7 + 1
  end   <- start + 6
  
  return(df[start:end, ])
}

# -------------------------------------------------------
# Extrair semana de treino (tudo antes) e teste (semana escolhida)
# -------------------------------------------------------
split_train_test <- function(df, week_index) {
  n_weeks <- floor(nrow(df) / 7)
  
  if (week_index < 2) {
    stop("Precisa de pelo menos 1 semana de treino antes da semana de teste.")
  }
  
  end_train <- (week_index - 1) * 7
  train_df  <- df[1:end_train, ]
  test_df   <- get_week_slice(df, week_index)
  
  return(list(train = train_df, test = test_df))
}

# -------------------------------------------------------
# Resolve "última semana", "penúltima", ou índice direto
# -------------------------------------------------------
resolve_week_index <- function(df, week_choice) {
  n_weeks <- floor(nrow(df) / 7)
  
  if (week_choice == "ultima") {
    return(n_weeks)
  } else if (week_choice == "penultima") {
    if (n_weeks < 2) stop("Não há semanas suficientes.")
    return(n_weeks - 1)
  } else {
    idx <- suppressWarnings(as.integer(week_choice))
    if (is.na(idx)) stop("Semana inválida.")
    return(idx)
  }
}

# -------------------------------------------------------
# Criar matriz C_pred (4 lojas x 7 dias) para otimização
# -------------------------------------------------------
build_C_pred <- function(week_choice = "ultima") {
  C_pred <- matrix(0, nrow = 4, ncol = 7)
  rownames(C_pred) <- CITIES
  colnames(C_pred) <- c("su", "mo", "tu", "we", "th", "fr", "sa")
  
  for (i in seq_along(CITIES)) {
    df  <- load_city_data(CITIES[i])
    idx <- resolve_week_index(df, week_choice)
    wk  <- get_week_slice(df, idx)
    C_pred[i, ] <- wk$Num_Customers
  }
  
  return(C_pred)
}
