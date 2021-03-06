#IMPORTANT!!!
#This script almost just the same as the beginning of the file ndfl.auto.arima.2.R

# Settings
#path = "C:/Users/Administrator/Dropbox/IPython/TaxesForecast/! ���/"
path = "C:/Users/Alekz/Dropbox/IPython/TaxesForecast/! ���/"
setwd(path)

library(forecast)
library(tseries)

# Read data
data = read.csv("data.csv", sep = ";")
gdp = ts(data[,"gdp"], freq=4, start=c(2007, 1))
fot = ts(data[,"fot"], freq=4, start=c(2007, 1))
esn = ts(data[,"esn"], freq=4, start=c(2007, 1))
defl = ts(data[,"defl"], freq=4, start=c(2007, 1))

twoyaxisplot(esn/defl, fot/defl, "���, ����. ���.", "���, ����. ���.")
legend("topleft", legend=c("���", "���"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")
twoyaxisplot(esn/defl, gdp/defl, "���, ����. ���.", "���, ����. ���.")
legend("topleft", legend=c("���", "���"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")