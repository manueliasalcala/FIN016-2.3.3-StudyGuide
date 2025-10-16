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
spy <- data$SPY
x <- 1:length(spy)
x2 <- x ** 2

spy_model <- lm(spy ~ x + x2)

spy_p <- summary(spy_model)$coefficients[, 4]
spy_r2 <- summary(spy_model)$r.squared

adf.test(spy)
acf(spy, plot = TRUE)