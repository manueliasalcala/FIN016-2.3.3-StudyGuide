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
hsbc <- data$HSBC

hsbc_ar <- arima(hsbc, order = c(1,0,0))
hsbc_ar1_mu <- coef(hsbc_ar)["intercept"]
hsbc_ar1_a1 <- coef(hsbc_ar)["ar1"]

hsbc_fitted <- fitted(hsbc_ar)
plot(hsbc, type = "l", xlab ="Time", ylab = "Price", main = "HSBC AR1", col = "black")
lines(hsbc_fitted, col = "red")
legend("topleft", legend = c("Observed", "AR1"), col = c("black", "red"))

acf(hsbc, plot = TRUE)
hsbc_res <- residuals(hsbc_ar)
plot(hsbc_res, type = "l", main = "Residuales HSBC AR1")
hist(hsbc_res, main = "Residuales HSBC AR1")

forecast_hsbc <- predict(hsbc_ar, n.ahead = 1)