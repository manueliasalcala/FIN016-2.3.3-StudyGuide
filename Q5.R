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
bbva <- data$BBVA
x <- 1:length(bbva)
x2 <- x ** 2

bbva_model <- lm(bbva ~ x + x2)

bbva_p <- summary(bbva_model)$coefficients[, 4]
bbva_r2 <- summary(bbva_model)$r.squared

adf.test(bbva)
acf(bbva, plot = TRUE)