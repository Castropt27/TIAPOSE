# ============================================================
# forecast_wrappers.R
# Wrappers para modelos de previsão
# ============================================================

# Modelos disponíveis
FORECAST_MODELS <- c(
  "Seasonal Naive"  = "snaive",
  "ARIMA"           = "arima",
  "ETS"             = "ets",
  "Random Forest"   = "rf"
)

# -------------------------------------------------------
# Métricas
# -------------------------------------------------------
calc_metrics <- function(real, predicted) {
  if (length(real) == 0 || length(predicted) == 0) return(NULL)
  
  err   <- real - predicted
  rmse  <- sqrt(mean(err^2, na.rm = TRUE))
  mae   <- mean(abs(err), na.rm = TRUE)
  nmae  <- ifelse(mean(real, na.rm = TRUE) == 0, NA,
                  mae / mean(real, na.rm = TRUE))
  
  ss_res <- sum(err^2, na.rm = TRUE)
  ss_tot <- sum((real - mean(real, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- ifelse(ss_tot == 0, NA, 1 - ss_res / ss_tot)
  
  list(RMSE = round(rmse, 4),
       MAE  = round(mae,  4),
       NMAE = round(nmae, 4),
       R2   = round(r2,   4))
}

# -------------------------------------------------------
# Seasonal Naive  [IMPLEMENTADO]
# -------------------------------------------------------
run_snaive <- function(train_df, h = 7) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Pacote 'forecast' não está instalado.")
  }
  
  ts_train <- ts(train_df$Num_Customers, frequency = 7)
  model    <- forecast::snaive(ts_train, h = h)
  pred     <- as.numeric(model$mean)
  
  return(pred)
}

# -------------------------------------------------------
# ARIMA  [IMPLEMENTADO]
# -------------------------------------------------------
run_arima <- function(train_df, h = 7) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Pacote 'forecast' não está instalado.")
  }
  
  ts_train <- ts(train_df$Num_Customers, frequency = 7)
  model    <- forecast::auto.arima(ts_train, seasonal = TRUE,
                                    stepwise = TRUE, approximation = TRUE)
  fcast    <- forecast::forecast(model, h = h)
  pred     <- as.numeric(fcast$mean)
  
  return(pred)
}

# -------------------------------------------------------
# ETS  [IMPLEMENTADO]
# -------------------------------------------------------
run_ets <- function(train_df, h = 7) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Pacote 'forecast' não está instalado.")
  }
  
  ts_train <- ts(train_df$Num_Customers, frequency = 7)
  model    <- forecast::ets(ts_train)
  fcast    <- forecast::forecast(model, h = h)
  pred     <- as.numeric(fcast$mean)
  
  return(pred)
}

# -------------------------------------------------------
# Random Forest  [IMPLEMENTADO]
# -------------------------------------------------------
run_rf_forecast <- function(train_df, test_df) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Pacote 'randomForest' não está instalado.")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Pacote 'zoo' não está instalado.")
  }
  
  # Prepara dados completos
  df_all <- rbind(train_df, test_df)
  df_all$TouristEvent <- ifelse(df_all$TouristEvent == "Yes", 1, 0)
  
  # Features
  df_all$DayOfWeek  <- as.integer(format(df_all$Date, "%u"))
  df_all$Month      <- as.integer(format(df_all$Date, "%m"))
  df_all$DayOfMonth <- as.integer(format(df_all$Date, "%d"))
  
  for (lag in c(1, 2, 3, 7, 14, 28)) {
    df_all[[paste0("lag", lag)]] <- c(rep(NA, lag),
                                       df_all$Num_Customers[1:(nrow(df_all) - lag)])
  }
  
  df_all$rolling_mean7 <- zoo::rollmean(df_all$Num_Customers,
                                         k = 7, fill = NA, align = "right")
  df_all$rolling_mean7 <- c(NA, df_all$rolling_mean7[-nrow(df_all)])
  
  features <- c("Num_Employees", "Pct_On_Sale", "TouristEvent",
                "DayOfWeek", "Month", "DayOfMonth",
                "lag1", "lag2", "lag3", "lag7", "lag14", "lag28",
                "rolling_mean7")
  
  n_train  <- nrow(train_df)
  n_test   <- nrow(test_df)
  
  # Índices válidos (sem NAs nos features)
  train_complete <- df_all[1:n_train, ]
  test_complete  <- df_all[(n_train + 1):(n_train + n_test), ]
  
  train_complete <- train_complete[complete.cases(train_complete[, features]), ]
  
  if (nrow(train_complete) < 10) {
    stop("Dados de treino insuficientes para Random Forest.")
  }
  
  formula_rf <- as.formula(
    paste("Num_Customers ~", paste(features, collapse = " + "))
  )
  
  model <- randomForest::randomForest(formula_rf, data = train_complete,
                                       ntree = 100)
  
  pred <- predict(model, newdata = test_complete)
  
  return(as.numeric(pred))
}

# -------------------------------------------------------
# Dispatcher principal
# -------------------------------------------------------
run_forecast <- function(model_id, city, week_choice) {
  
  # Modelos implementados
  implemented <- c("snaive", "arima", "ets", "rf")
  
  if (!model_id %in% implemented) {
    return(list(
      status  = "not_implemented",
      message = paste0("O modelo '", model_id, "' ainda não está implementado.")
    ))
  }
  
  tryCatch({
    df  <- load_city_data(city)
    idx <- resolve_week_index(df, week_choice)
    
    if (idx < 2) {
      return(list(
        status  = "error",
        message = "Precisa de pelo menos 1 semana de treino. Escolha a semana 2 ou posterior."
      ))
    }
    
    split  <- split_train_test(df, idx)
    train  <- split$train
    test   <- split$test
    real   <- test$Num_Customers
    dates  <- test$Date
    
    pred <- switch(model_id,
      "snaive" = run_snaive(train, h = 7),
      "arima"  = run_arima(train,  h = 7),
      "ets"    = run_ets(train,    h = 7),
      "rf"     = run_rf_forecast(train, test)
    )
    
    pred    <- pmax(0, round(pred))
    metrics <- calc_metrics(real, pred)
    
    result_table <- data.frame(
      Dia       = format(dates, "%a %d/%m"),
      Real      = real,
      Previsto  = pred,
      Erro      = real - pred
    )
    
    return(list(
      status       = "ok",
      city         = city,
      week_index   = idx,
      model        = model_id,
      table        = result_table,
      metrics      = metrics,
      dates        = dates,
      real         = real,
      predicted    = pred
    ))
    
  }, error = function(e) {
    return(list(
      status  = "error",
      message = paste("Erro na previsão:", conditionMessage(e))
    ))
  })
}
