#6

# We base the forecast on 

getForecast <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  tax_rate_w <- window(aggregate(tax_rate, 4)/3, start=test.start)
  # Fit
  fit <- hw(train, seasonal="additive")
  production_forecast <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=4, start=test.start)
  revenues_forecast <- production_forecast * tax_rate_w / 1000000
  fitted <- revenues_forecast
  rev.CI.Lo.95 <- ts(data.frame(forecast(fit, h=h))["Lo.95"][[1]], frequency=4, start=test.start)
  rev.CI.Lo.95 <- rev.CI.Lo.95 * tax_rate_w / 1000000
  rev.CI.Hi.95 <- ts(data.frame(forecast(fit, h=h))["Hi.95"][[1]], frequency=4, start=test.start)
  rev.CI.Hi.95 <- rev.CI.Hi.95 * tax_rate_w / 1000000
  revenues = window(x=revenues, start=c(2012,1))
  revenues = aggregate(revenues, 4)
  plot(revenues/1000, lwd=1, col="blue3", ylim=c(500,820),  xlab="", ylab="Поступления НДПИ, млрд. руб."); grid()
  lines(fitted, col="brown", lwd=1)
  lines(rev.CI.Lo.95, col="brown", lty="dashed")
  lines(rev.CI.Hi.95, col="brown", lty="dashed")
  legend("topleft", legend=c("Факт. поступления","Прогноз. поступления", "95% интервал"), col=c("blue3","brown","brown"), lwd=c(2,2,2), lty=c(1,1,2), cex=0.8, bty="n")
  plot(diff(aggregate(revenues/1000, nfrequency=4)), ylim=c(-60, 130), col="blue3", xlab="", ylab="Первая разность поступлений, млрд. руб."); grid()
  lines(diff(fitted), col="brown", lwd=1)
  legend("topleft", legend=c("Факт. поступления","Прогноз. поступления"), col=c("blue3","brown"), lwd=c(2,2), lty=c(1,1), cex=0.8, bty="n")
  #legend("bottomleft", legend=c("Факт. поступления","Прогноз. поступления"), col=c("blue3","brown"), lwd=c(2,2), cex=0.8, bty="n")
  return(fitted)
  #return(rev.CI.Lo.95)
}
df <- getForecast()


### REdundant:::

getForecast <- function(x=effective_production, h=12)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  tax_rate <- window(tax_rate, start=test.start)
  # Fit
  #fit <- Arima(train, order=c(0,0,0), seasonal=c(0,0,0))
  fit <- hw(train, seasonal="additive")
  export_forecast <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=12, start=test.start)
  revenues_forecast <- export_forecast * tax_rate / 1000000
  fitted <- aggregate(revenues_forecast, nfrequency=4)
  revenues = window(x=revenues, start=c(2011,1))
  plot(aggregate(revenues/1000, nfrequency=4), col="blue3", xlab="", ylab="Поступления пошлин на нефть, млрд. руб."); grid()
  lines(fitted, col="brown")
  legend("bottomleft", legend=c("Факт","Прогноз"), col=c("blue3","brown"), lwd=c(2,2), cex=0.8, bty="n")
  plot(diff(aggregate(revenues/1000, nfrequency=4)), ylim=c(-55,155), col="blue3", xlab="", ylab="Поступления после взятия разности, млрд. руб."); grid()
  lines(diff(fitted), col="brown")
  legend("bottomleft", legend=c("Факт. поступления","Прогноз. поступления"), col=c("blue3","brown"), lwd=c(2,2), cex=0.8, bty="n")
  return(fitted)
}
df <- getForecast()