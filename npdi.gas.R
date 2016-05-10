# Settings
#path = "C:/Users/Administrator/Dropbox/IPython/TaxesForecast/! НДПИ/"
path = "./"
setwd(path)

library(forecast)
library(tseries)

twoyaxisplot <- function(x1, x2, title=NULL, ylab=NULL){
  par(mar=c(5,4,4,5)+.1)
  plot(x1, col="blue3", xlab="", ylab=ylab); grid()
  par(new=TRUE)
  plot(x2, xlab="", ylab="", col="green3", plot.type="single", yaxt='n', ann=FALSE)
  axis(4)
  mtext(title, side=4, line=3)
}

# Read data
data = read.csv("data.gas.csv", sep = ";")
gas = ts(data[,"gas"], freq=4, start=c(2008, 1))
ndpi = ts(data[,"ndpi"], freq=4, start=c(2008, 1))
oil = ts(data[,"oil"], freq=4, start=c(2008, 1))
twoyaxisplot(gas, ndpi)

twoyaxisplot(ndpi/1000, oil, "НДПИ на газ, млрд. руб.", "Цена нефти, руб./барр.")
legend("topleft", legend=c("НДПИ на газ", "Цена нефти"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")

twoyaxisplot(ndpi/1000, gas/1000, "НДПИ на газ, млрд. руб.", "Добыча газа, трлн. куб. м")
legend("topleft", legend=c("НДПИ на газ", "Добыча газа"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")

fit <- auto.arima(ndpi, xreg=cbind(gas, oil), stepwise=FALSE, approximation=FALSE)
fit
ts.plot(fit$residuals)
ts.plot(ndpi/1000, fit$fitted/1000, col=c("blue3", "green3"), xlab="", ylab="НДПИ на газ, млрд. руб."); grid()
legend("topleft", legend=c("actual", "fitted"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")


twoyaxisplot(esn/defl, fot/defl, "ФОТ, млрд. руб.", "ЕСН, млрд. руб.")
legend("topleft", legend=c("ЕСН", "ФОТ"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")
twoyaxisplot(esn/defl, gdp/defl, "ВВП, млрд. руб.", "ЕСН, млрд. руб.")
legend("topleft", legend=c("ЕСН", "ВВП"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")