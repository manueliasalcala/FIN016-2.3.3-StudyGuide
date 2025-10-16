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
am <- data$AM
spy <- data$SPY

am_ret <- diff(log(am))
spy_ret <- diff(log(spy))

am_model <- lm(am_ret ~ spy_ret)
beta0_am <- coef(am_model)[1]
beta1_am <- coef(am_model)[2]

plot(spy_ret, am_ret, main = "Retornos AM vs SPY", xlab = "SPY", ylab = "AM", col = "darkgreen", pch = 19)
abline(am_model, col = "red")

am_ret_mean <- mean(am_ret)
am_ret_var <- var(am_ret)
am_ret_sd <- sd(am_ret)
am_cv <- am_ret_sd / am_ret_mean
cov_matrix_am <- cov(cbind(am_ret, spy_ret))
cor_matrix_am <- cor(cbind(am_ret, spy_ret))
