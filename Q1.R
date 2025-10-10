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
# Get nvdia data.
nvdia <- data$NVIDIA
#Get summary statistics.
nvdia_mean <- mean(nvdia)
nvdia_var <- var(nvdia)
#Calculate returns.
nvdia_ret <- diff(log(nvdia))
#Graph returns.
hist(nvdia_ret)
ts.plot(nvdia_ret, main = "Rendimientos NVDIA", xlab = "Días", ylab = "Precios")
#Get nabla
nvdia_nabla <- diff(nvdia)
ts.plot(nvdia_nabla, main = "Diferencial de NVDIA", xlab = "Días",
        ylab = "Precios")
nvdia_cor<- sum(head(nvdia - mean(nvdia), -1) * tail(nvdia - mean(nvdia), -1)) / sum((nvdia - mean(nvdia))^2)
acf(nvdia, lag.max = 3)
acf(nvdia, main = "Correlograma NVDIA")

nvdia_prod_dif <- head(nvdia - mean(nvdia), -1) * tail(nvdia - mean(nvdia))

ts.plot(nvdia_prod_dif, main = "Producto Difeencias NVDIA")