library(urca)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(COINT)
library(dplyr)
library(dplyrAssist)
library(quantmod)
library(readxl)
data <- read_excel("DATAGUIA1B.xlsx")
data <- data.frame(data)
amzn <- data$AMAZON

amzn_arima <- arima(amzn, order = c(2,0,1))
amzn_fit <- fitted(amzn_arima)

amzn_arima_mu <- coef(amzn_arima)["intercept"]
amzn_arima_a1 <- coef(amzn_arima)["ar1"]
amzn_arima_a2 <- coef(amzn_arima)["ar2"]
amzn_arima_b1 <- coef(amzn_arima)["ma1"]

plot(amzn, type = "l", main = "Amazon Observados y ARMA(2,1)", xlab = "Tiempo", ylab = "Precio", col = "black")
lines(amzn_fit, col = "blue")
legend("topleft", legend = c("Observed", "ARMA(2,1)"), col = c("black", "blue"))
amzn_resid <- residuals(amzn_arima)
hist(amzn_resid)

acf(amzn, plot = TRUE)
