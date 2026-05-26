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
# ARIMAX Growing Window  [REPORT-STYLE]
# -------------------------------------------------------
run_arimax_growing_window <- function(city, h = 7, n_iter = 20, initial_train = 574) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Pacote 'forecast' não está instalado.")
  }

  df <- load_city_data(city)
  df <- df[order(df$Date), ]

  df$Num_Customers[df$Num_Customers < 0] <- NA
  df$Num_Employees[df$Num_Employees < 0] <- NA
  df$Pct_On_Sale[df$Pct_On_Sale < 0] <- NA
  df$Sales[df$Sales < 0] <- NA
  df$TouristEvent_Bin <- ifelse(df$TouristEvent == "Yes", 1, 0)

  df$Num_Customers <- as.numeric(forecast::na.interp(ts(df$Num_Customers, frequency = 3)))
  df$Num_Employees <- as.numeric(forecast::na.interp(ts(df$Num_Employees, frequency = 3)))
  df$Pct_On_Sale   <- as.numeric(forecast::na.interp(ts(df$Pct_On_Sale, frequency = 3)))

  y <- ts(df$Num_Customers, frequency = 3)
  n <- length(y)

  xreg <- cbind(
    Num_Employees = df$Num_Employees,
    Pct_On_Sale   = df$Pct_On_Sale,
    TouristEvent  = df$TouristEvent_Bin
  )

  if (initial_train + n_iter * h > n) {
    stop(paste("Não há observações suficientes para:", city))
  }

  metrics_list <- list()
  all_preds <- data.frame()

  for (i in seq_len(n_iter)) {
    train_end  <- initial_train + (i - 1) * h
    test_start <- train_end + 1
    test_end   <- train_end + h

    y_train <- window(y, end = time(y)[train_end])
    y_test  <- window(y, start = time(y)[test_start], end = time(y)[test_end])
    x_train <- xreg[1:train_end, , drop = FALSE]
    x_test  <- xreg[test_start:test_end, , drop = FALSE]

    model <- forecast::auto.arima(y_train, xreg = x_train, seasonal = TRUE)
    fc <- forecast::forecast(model, xreg = x_test, h = h)

    pred <- as.numeric(fc$mean)
    real <- as.numeric(y_test)

    err <- real - pred
    rmse <- sqrt(mean(err^2, na.rm = TRUE))
    mae  <- mean(abs(err), na.rm = TRUE)
    nmae <- ifelse(mean(real, na.rm = TRUE) == 0, NA, mae / mean(real, na.rm = TRUE))
    ss_res <- sum(err^2, na.rm = TRUE)
    ss_tot <- sum((real - mean(real, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- ifelse(ss_tot == 0, NA, 1 - (ss_res / ss_tot))

    metrics_list[[i]] <- data.frame(
      Iteration = i,
      RMSE = rmse,
      MAE  = mae,
      NMAE = nmae,
      R2   = r2
    )

    all_preds <- rbind(all_preds, data.frame(
      Store = city,
      Iteration = i,
      Date = df$Date[test_start:test_end],
      Real = real,
      Predicted = pred
    ))
  }

  metrics_df <- do.call(rbind, metrics_list)
  summary_df <- data.frame(
    Store = city,
    Model = "ARIMAX",
    RMSE_Mean = mean(metrics_df$RMSE, na.rm = TRUE),
    MAE_Mean  = mean(metrics_df$MAE,  na.rm = TRUE),
    NMAE_Mean = mean(metrics_df$NMAE, na.rm = TRUE),
    R2_Mean   = mean(metrics_df$R2,   na.rm = TRUE),
    RMSE_Median = median(metrics_df$RMSE, na.rm = TRUE),
    MAE_Median  = median(metrics_df$MAE,  na.rm = TRUE),
    NMAE_Median = median(metrics_df$NMAE, na.rm = TRUE),
    R2_Median   = median(metrics_df$R2,   na.rm = TRUE)
  )

  list(
    status = "ok",
    model = "ARIMAX",
    city = city,
    metrics = metrics_df,
    summary = summary_df,
    predictions = all_preds
  )
}

# -------------------------------------------------------
# Random Forest Growing Window  [REPORT-STYLE]
# -------------------------------------------------------
run_rf_growing_window <- function(city, h = 7, runs = 20) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Pacote 'randomForest' não está instalado.")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Pacote 'zoo' não está instalado.")
  }

  df <- load_city_data(city)
  df <- df[order(df$Date), ]
  df <- df[!duplicated(df), ]

  df$Num_Customers <- zoo::na.approx(df$Num_Customers, na.rm = FALSE)
  df$Num_Employees[is.na(df$Num_Employees)] <- mean(df$Num_Employees, na.rm = TRUE)
  df$Pct_On_Sale[is.na(df$Pct_On_Sale)]     <- mean(df$Pct_On_Sale, na.rm = TRUE)
  df$Sales[is.na(df$Sales)]                 <- mean(df$Sales, na.rm = TRUE)
  df$TouristEvent <- ifelse(df$TouristEvent == "Yes", 1, 0)

  df$DayOfWeek  <- as.integer(format(df$Date, "%u"))
  df$Month      <- as.integer(format(df$Date, "%m"))
  df$DayOfMonth <- as.integer(format(df$Date, "%d"))

  for (lag in c(1, 2, 3, 7, 14, 28)) {
    df[[paste0("lag", lag)]] <- c(rep(NA, lag), df$Num_Customers[1:(nrow(df) - lag)])
  }

  df$rolling_mean7 <- zoo::rollmean(df$Num_Customers, k = 7, fill = NA, align = "right")
  df$rolling_mean7 <- c(NA, df$rolling_mean7[-nrow(df)])
  df <- df[complete.cases(df), ]

  features <- c(
    "Num_Employees", "Pct_On_Sale", "TouristEvent",
    "DayOfWeek", "Month", "DayOfMonth",
    "lag1", "lag2", "lag3", "lag7", "lag14", "lag28",
    "rolling_mean7"
  )

  n <- nrow(df)
  rmse_v <- mae_v <- nmae_v <- r2_v <- numeric(0)
  last_pred_table <- NULL

  if (n <= h + runs) {
    stop(paste("Não há observações suficientes para:", city))
  }

  for (b in seq_len(runs)) {
    train_end  <- n - h - runs + b
    test_start <- train_end + 1
    test_end   <- train_end + h

    train_df <- df[1:train_end, ]
    test_df  <- df[test_start:test_end, ]

    formula_rf <- as.formula(paste("Num_Customers ~", paste(features, collapse = " + ")))
    model <- randomForest::randomForest(formula_rf, data = train_df, ntree = 100)

    preds <- predict(model, newdata = test_df)
    reals <- test_df$Num_Customers

    err <- reals - preds
    rmse <- sqrt(mean(err^2, na.rm = TRUE))
    mae  <- mean(abs(err), na.rm = TRUE)
    nmae <- ifelse(mean(reals, na.rm = TRUE) == 0, NA, mae / mean(reals, na.rm = TRUE))
    ss_res <- sum(err^2, na.rm = TRUE)
    ss_tot <- sum((reals - mean(reals, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- ifelse(ss_tot == 0, NA, 1 - ss_res / ss_tot)

    rmse_v[b] <- rmse
    mae_v[b]  <- mae
    nmae_v[b] <- nmae
    r2_v[b]   <- r2

    if (b == runs) {
      last_pred_table <- data.frame(
        Date = test_df$Date,
        Real = reals,
        Predicted = round(as.numeric(preds), 2),
        Lower = round(0.84 * as.numeric(preds), 2),
        Upper = round(as.numeric(preds) * 1.20, 2)
      )
    }
  }

  metrics_df <- data.frame(
    Iteration = seq_len(runs),
    RMSE = rmse_v,
    MAE = mae_v,
    NMAE = nmae_v,
    R2 = r2_v
  )

  summary_df <- data.frame(
    Store = city,
    Model = "Random Forest",
    RMSE_Mean = mean(rmse_v, na.rm = TRUE),
    MAE_Mean  = mean(mae_v,  na.rm = TRUE),
    NMAE_Mean = mean(nmae_v, na.rm = TRUE),
    R2_Mean   = mean(r2_v,   na.rm = TRUE),
    RMSE_Median = median(rmse_v, na.rm = TRUE),
    MAE_Median  = median(mae_v,  na.rm = TRUE),
    NMAE_Median = median(nmae_v, na.rm = TRUE),
    R2_Median   = median(r2_v,   na.rm = TRUE)
  )

  list(
    status = "ok",
    model = "Random Forest",
    city = city,
    metrics = metrics_df,
    summary = summary_df,
    predictions = last_pred_table
  )
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
