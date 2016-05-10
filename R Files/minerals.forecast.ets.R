# 4

# Benchmark models are
# hw(effective_production, seasonal="additive")
# hw(effective_production, seasonal="additive")

# *********** #
# ETS monthly #
# *********** #
# Summary: 114 obs.

# Plot diagnostics TS plots
tsdisplay(effective_production/1000, main="Ёффективн", col="blue3")
# No difference in seasonal behavior on different levels => it is likely to be additive seasonality

# As long as we expect seasonality to be present, use Holt-Winters seasonal method
mets_fit1 <- hw(effective_production, seasonal="additive")
accuracy(mets_fit1)
plot(forecast(mets_fit1))

# Let's see what automatic optimization will suggest:
mets_fit2 <- ets(effective_production)
mets_fit2 # ETS(ANN)
#AIC     AICc      BIC 
#1554.386 1554.546 1559.099 
accuracy(mets_fit2)
plot(mets_fit2)
# -- higher accuracy measures than in previous case

mets_fit3 <- ets(effective_production, model="ANN")
accuracy(mets_fit3)


# benchmark is hw(effective_production, seasonal="additive")


# *********** #
# ETS quarter #
# *********** #
# Summary: 34 obs.

# Plot diagnostics TS plots
tsdisplay(effective_production_q/1000, main="Ёффективный экспорт нефти, тыс. т", col="blue3")
# No difference in seasonal behavior on different levels => it is likely to be additive seasonality

# As long as we expect seasonality to be present, use Holt-Winters seasonal method
qets_fit1 <- hw(effective_production_q, seasonal="additive")
accuracy(qets_fit1)

# Let's see what automatic optimization will suggest:
qets_fit2 <- ets(effective_production_q)
qets_fit2 # ETS(MAA) => suggests multiplicative level, additive trend, and additive seasonal component
#AIC     AICc      BIC 
#526.1133 526.6351 528.6295 
accuracy(qets_fit2)
plot(qets_fit2)
# -- higher accuracy measures than in previous case

# benchmark is hw(effective_production, seasonal="additive")