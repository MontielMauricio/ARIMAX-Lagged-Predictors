################################################
# Con este código se pueden pronósticar series de tiempo multivariadas
#
## Funciones de transferencia para pronosticar el IPC utilizando el precio del petróleo
#
# Hecho por Mauricio Montiel el 14/04/2020
###############################################

# Preliminares
remove(list = ls())
setwd("~/GitHub/Forecasting/ARIMAX-Lagged-Predictors/")

#librerías
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(forecast)

# Importar datos
data_x <- read_excel("data.xlsx")
data_x <- ts(data_x, start= c(2001,1), frequency = 251)

# Grafico de datos en niveles
ts.plot(scale(data_x), col = c(1, 2),  lwd = c(1, 2)) #la roja es el petroleo

# Prueba de estacionariedad para el ipc
ur.kpss(data_x[,1]) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(data_x[,1])) %>% summary() # los datos diferenciados si son estacionarios

# Prueba de estacionariedad para el petróleo
ur.kpss(data_x[,2]) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(data_x[,2])) %>% summary() # los datos diferenciados si son estacionarios

##
# Modelo ARIMAX
##

# AR representa una regresion de la variable contra valores pasados de ella misma
# MA representa la media movil ponderada de los errores pasados del pronóstico.
# X representa una variable o un conjunto de variables explicativas

# Lagged predictors.
oil_lag <- cbind(
  Lag0 = data_x[,"oil"],
  Lag1 = stats::lag(data_x[,"oil"],-1),
  Lag2 = stats::lag(data_x[,"oil"],-2),
  Lag3 = stats::lag(data_x[,"oil"],-3)) 

# Restrict data so models use same fitting period
fit1 <- auto.arima(data_x[4:4848,1], xreg=oil_lag[4:4848,1], stationary=TRUE)
fit2 <- auto.arima(data_x[4:4848,1], xreg=oil_lag[4:4848,1:2], stationary=TRUE)
fit3 <- auto.arima(data_x[4:4848,1], xreg=oil_lag[4:4848,1:3], stationary=TRUE)
fit4 <- auto.arima(data_x[4:4848,1], xreg=oil_lag[4:4848,1:4], stationary=TRUE)

# chose the best
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

# Selección automatica del modelo ARIMAX
modelo_x <- auto.arima(data_x[4:4848,1], xreg = oil_lag[4:4848,1:4], seasonal=T, stepwise=T, approximation=T)
summary(modelo_x)
checkresiduals(modelo_x) # Los residuales estan correlacionados

# Pronóstico
autoplot(forecast(modelo_x, h=10,
         xreg = cbind(Lag0=oil_lag[4838:4848,1],
                      Lag1=oil_lag[4838:4848,2],
                      Lag2=oil_lag[4838:4848,3],
                      Lag3=oil_lag[4838:4848,4])), include = 100)
       
