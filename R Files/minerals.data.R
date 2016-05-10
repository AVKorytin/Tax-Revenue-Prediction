# 1
library(MAPA)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggplot2)
library(astsa)
library(tsoutliers)
library(fpp)

path = "C:/Users/Alekz/Dropbox/IPython/TaxesForecast/! ÍÄÏÈ/"
#path = "C:/Users/Administrator/Dropbox/IPython/TaxesForecast/! ÍÄÏÈ/"
setwd(path)

# Read data:
data = read.csv("data.csv", sep = ";")
production = ts(data["total_production"], frequency=12, start=c(2006,1))
revenues = ts(data["revenues"], frequency=12, start=c(2006,1))
ex_rate = ts(data["ex_rate"], frequency=12, start=c(2006,1))
kc = ts(data["Kc"], frequency=12, start=c(2006,1))
tax_rate = ts(data["tax_rate"], frequency=12, start=c(2006,1))
oil_price = ts(data["oil_price"], frequency=12, start=c(2006,1))

# Calculate additional variables:
effective_production = revenues*1000/tax_rate
exemption_index = effective_production/production

# Different behavior in 2008 => use data since 2009:
effective_production0 = effective_production
effective_production = window(x=effective_production, start=c(2009,1))

# Aggregate data to quarterly:
effective_production_q = aggregate(x=effective_production, nfrequency=4)