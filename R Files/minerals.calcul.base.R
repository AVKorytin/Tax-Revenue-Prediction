getForecastm <- function(x=effective_production_q, h=4)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x, end=train.end)
  test <- window(x, start=test.start)
  # Fit M
  effective_production_q = window(effective_production_q, start=c(2012, 1))
  fit <- hw(train, seasonal="additive")
  production_forecast <- ts(data.frame(forecast(fit, h=h))["Point.Forecast"][[1]], frequency=4, start=test.start)
  fitted <- production_forecast
  CI.Lo95 <- ts(data.frame(forecast(fit, h=h))["Lo.95"][[1]], frequency=4, start=test.start)
  CI.Hi95 <- ts(data.frame(forecast(fit, h=h))["Hi.95"][[1]], frequency=4, start=test.start)
  ts.plot(effective_production_q/1000, col="blue3", xlab="", ylim=c(90,120), xlim=c(2012, 2015.3), ylab="Расчетная добыча, млн. т")
  lines(fitted/1000, col="brown")
  lines(CI.Lo95/1000, col="brown", lty="dashed")
  lines(CI.Hi95/1000, col="brown", lty="dashed")
  legend("bottomleft", legend=c("Расч. добыча", "Прогноз. добыча", "95% интервал"), col=c("blue3", "brown", "brown"), lwd=c(2, 2, 2), lty=c(1,1,2), cex=0.8, bty="n")
  return(fitted)
}
df <- getForecastm()