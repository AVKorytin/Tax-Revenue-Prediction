#IMPORTANT!!!
#This script almost just the same as the beginning of the file ndfl.auto.arima.2.R

# Settings
#path = "C:/Users/Administrator/Dropbox/IPython/TaxesForecast/! ÅÑÍ/"
path = "C:/Users/Alekz/Dropbox/IPython/TaxesForecast/! ÅÑÍ/"
setwd(path)

library(forecast)
library(tseries)

# Read data
data = read.csv("data.csv", sep = ";")
gdp = ts(data[,"gdp"], freq=4, start=c(2007, 1))
fot = ts(data[,"fot"], freq=4, start=c(2007, 1))
esn = ts(data[,"esn"], freq=4, start=c(2007, 1))
defl = ts(data[,"defl"], freq=4, start=c(2007, 1))

twoyaxisplot(esn/defl, fot/defl, "ÔÎÒ, ìëğä. ğóá.", "ÅÑÍ, ìëğä. ğóá.")
legend("topleft", legend=c("ÅÑÍ", "ÔÎÒ"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")
twoyaxisplot(esn/defl, gdp/defl, "ÂÂÏ, ìëğä. ğóá.", "ÅÑÍ, ìëğä. ğóá.")
legend("topleft", legend=c("ÅÑÍ", "ÂÂÏ"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")