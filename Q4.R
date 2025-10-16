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
x <- 1:length(tsla)

tsla_ret <- diff(log(tsla))
spy_ret <- diff(log(spy))

tsla_model <- lm(tsla ~ x)

tsla_beta0 <- coef(tsla_model)[1]
tsla_beta1 <- coef(tsla_model)[2]
tsla_r2 <- summary(tsla_model)$r.squared
tsla_p <- summary(tsla_model)$coefficients[, 4]

tsla_fit <- fitted(tsla_model)
max_t <- max(x)
forecast_x <- c(max_t +1,max_t +2,max_t +3,max_t +4)
forecast_y <- tsla_beta0 + tsla_beta1 * forecast_x
plot(x, tsla, type = "l", main = "Precios de TSLA y Estimados", xlab = "Tiempo", ylab = "Precio")
lines(x, tsla_fit, col = "blue")
points(forecast_x, forecast_y, col = "red")