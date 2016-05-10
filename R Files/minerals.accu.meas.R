# 5

# From oil.forecast.sarima and oil.forecast.ets benchmark models are
# SARIMA(0,0,0)(0,0,0)12 -- msarima_fit1
# SARIMA(0,0,0)(0,1,0)4 -- qsarima_fit2.1
# hw(effective_production, seasonal="additive") -- mets_fit1
# hw(effective_production_q, seasonal="additive") -- qets_fit1

# Let's first compare ets and sarima on same frequency:
# Monthly
## SARIMA
accmsarima <- function(x=effective_production, h=12)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,0,0))
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test))
}
accmets <- function(x=effective_production, h=12)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- hw(train, seasonal="additive")
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test))
}
accuracy(msarima_fit1)
accuracy(mets_fit1)
accmsarima()
accmets()


# Quarterly
accqsarima <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,1,0))
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test))
}
accqets <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- hw(train, seasonal="additive")
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test))
}
accuracy(qsarima_fit2.1)
accuracy(qets_fit1)
accqsarima()
accqets()


accqsarima2 <- function(x=effective_production_q, h=4)
{
  #' This is just to check if functions are right
  #' Results should be exactly the same as in accqsarima()
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,1,0))
  fitted <- data.frame(forecast(fit,h=h))["Point.Forecast"][[1]]
  actual <- tail(x, n=h)
  MAE <- sum(abs(fitted - actual))/h
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}
accqsarima2()


# Aggregated monthly SARIMA
accaggmsarima <- function(x=effective_production, h=12, k=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  fit <- Arima(train, order=c(0,0,0), seasonal=c(0,0,0))
  fitted <- data.frame(forecast(fit,h=h))["Point.Forecast"][[1]]
  fitted <- aggregate(ts(fitted, frequency=12), nfrequency=k)
  actual <- aggregate(ts(tail(x, n=h), frequency=12), nfrequency=k)
  MAE <- sum(abs(fitted - actual))/(h/3)
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}


# Aggregated monthly ETS
accaggmets <- function(x=effective_production, h=12, k=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  fit <- hw(train, seasonal="additive")
  fitted <- data.frame(forecast(fit,h=h))["Point.Forecast"][[1]]
  fitted <- aggregate(ts(fitted, frequency=12), nfrequency=k)
  actual <- aggregate(ts(tail(x, n=h), frequency=12), nfrequency=k)
  MAE <- sum(abs(fitted - actual))/(h/3)
  RMSE <- sqrt(mean((fitted - actual)^2))
  MAPE <- mean(abs(100*(fitted - actual)/actual))
  return(list(MAE, RMSE, MAPE))
}


accqsarima()
accaggmsarima()
accqets()
accaggmets()