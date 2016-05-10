# Some changes was done. See accuracyMeasures() function

# Settings
#path = "C:/Users/Administrator/Dropbox/IPython/TaxesForecast/! НДФЛ/"
path = "~/R/TaxesIEP"
setwd(path)

library(forecast)
library(tseries)

# Read data
data = read.csv("data2.csv", sep = ";")
gdp = ts(data[,"gdp"], freq=4, start=c(2003, 1))
fot = ts(data[,"fot"], freq=4, start=c(2003, 1))
ndfl = ts(data[,"ndfl"], freq=4, start=c(2003, 1))
defl = ts(data[,"defl"], freq=4, start=c(2003, 1))
ipc1 = ts(data[,"ipc1"], freq=4, start=c(2003, 1))
ipc2 = ts(data[,"ipc2"], freq=4, start=c(2003, 1))

#defl = ipc2
# Transform data
gdp.log <- log(gdp/defl)              #Real GDP log
tsoutliers(diff(gdp.log))             #Detect ts outliers in the first differences (there aren't)
fot.log <- log(fot/defl)              #Real FOT log                    WHY?
tsoutliers(diff(fot.log))
ndfl.log <- log(ndfl/defl)            #Real NDFL log
tsoutliers(diff(ndfl.log))
ndfl.log.clean <- diffinv(tsclean( diff(ndfl.log)) ) + ndfl.log[1]
                                      #Clean original ts from 1-diff ts outliers

# Preliminary stuff: Exploratory analisys
ts.plot(exp(ndfl.log.clean), col="blue3", xlab="", ylab="НДФЛ, млрд. руб."); grid()
twoyaxisplot(exp(ndfl.log.clean), fot/defl, "ФОТ, млрд. руб.", "НДФЛ, млрд. руб.")
legend("topleft", legend=c("НДФЛ", "ФОТ"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")
twoyaxisplot(exp(ndfl.log.clean), gdp/defl, "ВВП, млрд. руб.", "НДФЛ, млрд. руб.")
legend("topleft", legend=c("НДФЛ", "ВВП"), col=c("blue3", "green3"), lwd=c(2, 2), lty=c(1,1), cex=0.8, bty="n")

# Estimation
insample.with.reg <- function(y, x, h=4)
{
# Description:
# y - ts outcome
# x - ts predictor
# h - period (4 for quater, 12 for month)
        t <- time(y)          #Time series of time
        # We want to predict one last period
        test.start <- t[length(y) - h + 1]
        # Sampling training and test data subseries
        train.y <- window(y, end = test.start - 1)
        train.x <- window(x, end = test.start - 1)
        test.y  <- window(y, start = test.start)
        test.x  <- window(x, start = test.start)
        
        fit <- auto.arima(train.y, xreg = train.x, stepwise = F, approximation = F)
        print(fit)
        
        # Testing residuals
        print( Box.test(fit$residuals, fitdf=3, lag=10, type="Ljung") )  #WHY?
        print( Box.test(fit$residuals, fitdf=4, lag=10, type="Ljung") )  #WHY?
        print( jarque.bera.test(fit$residuals) )
        ts.plot(fit$residuals, col="blue3", ylab="Остатки модели SARIMAX(1,0,0)(0,1,1)[4]"); grid()
        
        # Check accuracy measures for Test data
        fitted0 <- data.frame(forecast(fit, xreg = test.x, h = h))["Point.Forecast"][[1]]
        actual <- tail(y, n = h)
        print( exp(fitted0) )
        print( exp(actual) )
        accuracyMeasures(actual, fitted)
        
        # Automatic accuracy calculations
        print( accuracy(forecast(fit, xreg = test.x), test.y) )
        
        # Plot forecast in logs
        plot(forecast(fit, xreg=test.x), col="blue3"); grid()
        lines(ndfl.log.clean, col="green3")
        
        # Retrieve forecast data
        fitted <- ts(fitted0, frequency = 4, start = test.start)
        
        # Confident interval
        CI.Lo95 <- ts(data.frame(forecast(fit, xreg=test.x, h=h))["Lo.95"][[1]], frequency=4, start=test.start)
        CI.Hi95 <- ts(data.frame(forecast(fit, xreg=test.x, h=h))["Hi.95"][[1]], frequency=4, start=test.start)
        
        col.gamma <- cbind("blue3", "brown", "coral1", "coral1")
        line.types <- cbind("solid", "solid", "dashed", "dashed")
        
        ts.plot(cbind(y, fitted, CI.Hi95, CI.Lo95), col = col.gamma, lty = line.types); grid()
        ts.plot(cbind(window(y, freq=4, start=c(2010,1)), fitted, CI.Hi95, CI.Lo95),
                col = col.gamma, lty = line.types, xlab = ""); grid()
        ts.plot(cbind(exp(y), exp(fitted), exp(CI.Hi95), exp(CI.Lo95)), col=col.gamma, lty=line.types); grid()
        
        par(mar=c(2,4,1,1))
        ts.plot(cbind(exp(window(y, freq=4, start=c(2010,1))), exp(fitted), exp(CI.Hi95), exp(CI.Lo95)), col=col.gamma, lty=line.types, xlab="", ylab="НДФЛ, млрд. руб."); grid()
        legend("topleft", legend=c("НДФЛ", "Прогноз", "95% интервал"), col=c("blue3", "brown", "brown"), lwd=c(2, 2, 2), lty=c(1,1,2), cex=0.8, bty="n")
        
        # Construct some prediction intervals for differenced data
        fitted <- c(forecast(fit, xreg = test.x, h = h)$fitted, fitted)
        fitted <- ts(exp(fitted), frequency = 4, start = c(2003, 1))
        fitted.undiff <- fitted
        fitted <- diff(fitted)
        
        ## Lower bound
        CI.Lo95.2 <- character()
        for(i in 1:h)
        {
                fitted.end <- time(fitted)[length(y)-h-1+i]
                stdev <- sd(window(fitted, end = fitted.end))
                CI.Lo95.2 <- c( CI.Lo95.2, fitted[length(y)-h-1+i] + qnorm(0.975)*stdev )
        }
        CI.Lo95.2 <- ts(CI.Lo95.2, frequency = 4, start = test.start)
        
        ## Upper bound
        CI.Hi95.2 <- character()
        for(i in 1:h)
        {
                fitted.end <- time(fitted)[length(y)-h-1+i]
                stdev <- sd(window(fitted, end=fitted.end))
                CI.Hi95.2 <- c( CI.Hi95.2, fitted[length(y)-h-1+i] - qnorm(0.975)*stdev )
        }
        CI.Hi95.2 <- ts(CI.Hi95.2, frequency = 4, start = test.start)
        
        #ARIMA first?
        fitted <- ts(tail(fitted, n=h), frequency = 4, start = test.start)
        ts.plot(cbind(diff(exp(y)), fitted, CI.Hi95.2, CI.Lo95.2), col=col.gamma, lty=line.types, xlab=""); grid()
        par(mar=c(2,4,1,1))
        ts.plot(cbind(diff(exp(window(y, freq=4, start=c(2010,1)))), fitted, CI.Hi95.2, CI.Lo95.2), col=col.gamma, lty=line.types, xlab="", ylab="НДФЛ, первые разности, млрд. руб."); grid()
        legend("topleft", legend=c("НДФЛ", "Прогноз", "95% интервал"), col=c("blue3", "brown", "brown"), lwd=c(2, 2, 2), lty=c(1,1,2), cex=0.8, bty="n")
        
        print( "Accuracy for unlogged differenced series" )
        actual <- tail(diff(exp(y)), n=h)
        accuracyMeasures(actual, fitted)
        
        print( "Accuracy for unlogged series" )
        fitted <- tail(fitted.undiff, n=h)
        actual <- tail(exp(y), n=h)
        accuracyMeasures(actual, fitted)
        
        print( "Check if unlogged auto arima is better" )
        fit <- auto.arima(exp(train.y), xreg = exp(train.x), stepwise = F, approximation = F)
        print( accuracy(forecast(fit, xreg=exp(test.x)), exp(test.y)) )
        
        # SARIMA
        print( "SARIMA calculations" )
        fit <- auto.arima(train.y, stepwise=FALSE, approximation=FALSE)
        print(fit)
        print( Box.test(fit$residuals, fitdf=3, lag=10, type="Ljung") )
        print( jarque.bera.test(fit$residuals) )
        ts.plot(fit$residuals, col="blue3", ylab="Остатки модели SARIMAX(1,0,0)(0,1,1)[4]"); grid()
        
        fitted <- data.frame(forecast(fit, h=h))["Point.Forecast"][[1]]
        fitted <- ts(fitted, frequency=4, start=test.start)
        ts.plot(cbind(y, fitted), col=cbind("blue3", "brown"), lty=cbind("solid", "solid")); grid()
        ts.plot(cbind(diff(y), diff(fitted)), col=cbind("blue3", "brown"), lty=cbind("solid", "solid")); grid()
        fitted <- data.frame(forecast(fit, h=h))["Point.Forecast"][[1]]
        
        print( "Accuracy for unlogged series" )
        fitted <- tail(exp(fitted), n=h)
        actual <- tail(exp(y), n=h)
        accuracyMeasures(actual, fitted)
        
        # ETS
        print( "ETS calculations" )
        fit <- ets(train.y)
        print(fit)
        print( Box.test(fit$residuals, fitdf=3, lag=10, type="Ljung") )
        print( jarque.bera.test(fit$residuals) )
        ts.plot(fit$residuals, col="blue3", ylab="Остатки модели SARIMAX(1,0,0)(0,1,1)[4]"); grid()
        fitted <- data.frame(forecast(fit, h=h))["Point.Forecast"][[1]]
        fitted <- ts(fitted, frequency=4, start=test.start)
        ts.plot(cbind(y, fitted), col=cbind("blue3", "brown"), lty=cbind("solid", "solid")); grid()
        ts.plot(cbind(diff(y), diff(fitted)), col=cbind("blue3", "brown"), lty=cbind("solid", "solid")); grid()
        fitted <- data.frame(forecast(fit, h=h))["Point.Forecast"][[1]]
        
        print( "Accuracy for unlogged series" )
        fitted <- tail(exp(fitted), n=h)
        actual <- tail(exp(y), n=h)
        accuracyMeasures(actual, fitted)
}

accuracyMeasures <- function(actual, fitted) {
        h <- length(actual)
        RMSE <- c( "RMSE", sqrt(mean((fitted - actual)^2)) )
        MAE <- c( "MAE", sum(abs(fitted - actual))/h )
        MAPE <- c( "MAPE", mean(abs(100*(fitted - actual)/actual)) )
        print( list(RMSE, MAE, MAPE) )
}


insample.with.reg(ndfl.log.clean, fot.log, h=4)
insample.with.reg(ndfl.log.clean, gdp.log, h=4)

# BACKLOG: write unclean function
(0.001832597 - (-0.3611811)) + (-0.8243357 - (-0.3069227)) + (0.04654134 - (-0.3151799))