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
tsla <- data$TESLA
x <- 1

tsla_ret <- diff(log(tsla))
spy_ret <- diff(log(spy))

tsla_model <- lm(tsla ~ spy)

tsla_beta0 <- coef(tsla_model)[1]
tsla_beta1 <- coef(tsla_model)[2]
tsla_r2 <- summary(tsla_model)$r.squared

tsla_fit <- fitted(tsla_model)
forecast(tsla_fit, h = 4)