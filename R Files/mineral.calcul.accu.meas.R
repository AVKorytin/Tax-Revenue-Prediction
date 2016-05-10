#8

accaggsarima.rev <- function(x=effective_production, h=12)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  tax_rate_w <- window(tax_rate, start=test.start)
  # Fit
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,0,0))
  production_forecast <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=12, start=test.start)
  revenues_forecast <- production_forecast * tax_rate_w / 1000
  fitted <- aggregate(revenues_forecast, 4)
  actual <- aggregate(window(x=revenues, start=test.start), 4)
  MAE <- sum(abs(fitted - actual))/h
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}


accaggmets.rev <- function(x=effective_production, h=12)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  duty_rate_w <- window(tax_rate, start=test.start)
  # Fit
  fit <- hw(train, seasonal="additive")
  export_forecast <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=12, start=test.start)
  revenues_forecast <- export_forecast * duty_rate_w / 1000
  fitted <- aggregate(revenues_forecast, 4)
  actual <- aggregate(window(x=revenues, start=test.start), 4)
  MAE <- sum(abs(fitted - actual))/h
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}

accqsarima.rev <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  duty_rate_w <- window(aggregate(tax_rate, 4)/3, start=test.start)
  # Fit
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,1,0))
  export_forecast_q <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=4, start=test.start)
  revenues_forecast_q <- export_forecast_q * duty_rate_w / 1000
  fitted <- aggregate(revenues_forecast_q, 4)
  actual <- window(aggregate(x=revenues, 4), start=test.start)
  MAE <- sum(abs(fitted - actual))/h
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}

accqets.rev <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  duty_rate_w <- window(aggregate(tax_rate, 4)/3, start=test.start)
  # Fit
  fit <- hw(train, seasonal="additive")
  export_forecast_q <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=4, start=test.start)
  revenues_forecast_q <- export_forecast_q * duty_rate_w / 1000
  fitted <- aggregate(revenues_forecast_q, 4)
  actual <- window(aggregate(x=revenues, 4), start=test.start)
  MAE <- sum(abs(fitted - actual))/h
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}

accaggsarima.rev()
accaggmets.rev()
accqsarima.rev()
accqets.rev()