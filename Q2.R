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
f1 <- data$F1
spy <- data$SPY

f1_ret <- diff(log(f1))
spy_ret <- diff(log(spy))

ts.plot(f1, main="F1 Prices", col="red")
ts.plot(spy, main="SPY Prices", col="blue")

f1_model <- lm(f1_ret ~ spy_ret)
beta0_f1 <- coef(f1_model)[1]
beta1_f1 <- coef(f1_model)[2]

plot(spy_ret, f1_ret, xlab = "F1", ylab = "SPY")
abline(f1_model, col="red", lwd=2)

f1_ret_mean <- mean(f1_ret)
f1_ret_var <- var(f1_ret)
f1_ret_sd <- sd(f1_ret)
f1_ret_cov_m <- cov(cbind(f1_ret, spy_ret))
f1_ret_cor_m <- cor(cbind(f1_ret, spy_ret))
f1_cv <- f1_ret_sd / f1_ret_mean