# 2

# Plot production
plot(production, xlab="", ylab="Эффективная добыча", col="blue3"); grid()

# Plot exemption and administration index:
plot(exemption_index, xlab="", ylab="Отношение эффективного к базовому", col="blue3"); grid()

# Check if there are outliers present:
tsoutliers(as.numeric(effective_production))
plot(tsclean(as.numeric(effective_production)), type="l")
lines(as.numeric(effective_production), type="l", col="blue3")

# Recalculate effective_export and revenues:
effective_production1 = effective_production
effective_production = ts(tsclean(as.numeric(effective_production)), frequency=12, start=c(2009,1))
ts.plot(effective_production, xlab="", ylab="Эффективная добыча", col="blue3"); grid()

revenues0 = revenues
revenues = effective_production * tax_rate / 1000
ts.plot(revenues, revenues0, col=c("blue3", "green3"))


# Check for unit roots:
# It is not obvious whether series is stationary. We better do some tests
k = trunc((length(effective_production)-1)^(1/3)) # default calculated value in tseries package
k = ceiling(12*(length(effective_production)/100)^0.25) # see, Ng
# Ndiffs based on tests
ndiffs(effective_production, alpha=0.05, test=c("kpss","adf", "pp"), max.d=12)
ndiffs(effective_production, alpha=0.05, test="adf", max.d=12)
ndiffs(effective_production, alpha=0.05, test="kpss", max.d=12)
ndiffs(effective_production, alpha=0.05, test="pp", max.d=12)
# NSdiffs based on tests
nsdiffs(effective_production, m=12, test=c("ocsb","ch"), max.D=12)
nsdiffs(effective_production, m=12, test="ocsb", max.D=12)
nsdiffs(effective_production, m=12, test="ch", max.D=12)
# ADF tests:
adf.test(effective_production, alternative="s", k=k)
adfTest(effective_production, lags=k, type=c("nc", "c", "ct"))
adfTest(effective_production, lags=k, type="nc")
adfTest(effective_production, lags=k, type="c")
adfTest(effective_production, lags=k, type="ct")

# Make plots for presentation:
production = window(production, c(2009,1))
effective_production = window(effective_production, c(2009,1))
# 1) total_export & effective_export quarterly
par(mar=c(4,4,2,2))
ts.plot(aggregate(production/1000, nfrequency=4), aggregate(effective_production/1000, nfrequency=4), col=c("blue3", "springgreen4"), lwd=1, xlab="", ylab="Добыча нефти, млн. т"); grid()
legend("bottomleft", legend=c("Общая добыча","Расчетная добыча"), col=c("blue3","springgreen4"), lwd=c(2,2), cex=0.8, bty="n")


# 2) effective_export monthly
par(mar=c(4,4,2,2))
plot(effective_production, xlab="", ylab="Расчетная добыча, млн. т", col="blue3"); grid()


##
plot(duty_rate, xlab="", ylab="Расчетный экспорт, млн. т", col="blue3"); grid()
par(new=TRUE)
plot(ex_rate, xlab="", ylab="Расчетный экспорт, млн. т", col="blue3"); grid()