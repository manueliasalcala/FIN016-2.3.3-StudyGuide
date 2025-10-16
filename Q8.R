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

hsbc_ar2 <- arima(hsbc, order = c(2,0,0))
hsbc_ar2_mu <- coef(hsbc_ar2)["intercept"]
hsbc_ar2_a1 <- coef(hsbc_ar2)["ar1"]
hsbc_ar2_a2 <- coef(hsbc_ar2)["ar2"]

hsbc_ar <- arima(hsbc, order = c(1,0,0))
hsbc_ar1_mu <- coef(hsbc_ar)["intercept"]
hsbc_ar1_a1 <- coef(hsbc_ar)["ar1"]

acf(hsbc, plot = TRUE)
hsbc_res2 <- residuals(hsbc_ar2)
plot(hsbc_res, type = "l", main = "Residuales HSBC AR2")
hist(hsbc_res, main = "Residuales HSBC AR2")

forecast_hsbc <- predict(hsbc_ar, n.head = 1)
forecast_hsbc2 <- predict(hsbc_ar2, n.head = 1)