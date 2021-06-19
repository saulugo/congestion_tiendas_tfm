library(prophet)
library(tidyverse)
library(lubridate)
library(plotly)
source('~/master/TFM/codigo/funciones_auxiliares.R', encoding = 'UTF-8')

################################################################################
###### PRE-PROCESADO DE LOS DATOS###############################################
################################################################################

setwd("C:/Users/sll017es/Documents/master/TFM/codigo")

data <- read.csv("data/data.csv")

data$DIA_SEMANA <- weekdays(ymd_hms(as.character(data$FECHA_TRAMO)))



data_1290 <- read_csv("data/data_tienda_1290.csv") #tienda DIA
data_2266 <- read_csv("data/data_tienda_2266.csv") #tienda DIA
data_2701 <- read_csv("data/data_tienda_2701.csv") #tienda DIA
data_6467 <- read_csv("data/data_tienda_6467.csv") #tienda DIA
data_10061 <- read_csv("data/data_tienda_10061.csv") #tienda DIA
data_16139 <- read_csv("data/data_tienda_16139.csv") #tienda DIA
data_52288 <- read_csv("data/data_tienda_52288.csv") #tienda LA PLAZA
data_53116 <- read_csv("data/data_tienda_53116.csv") #tienda DIA y luego LA PLAZA
data_60850 <- read_csv("data/data_tienda_60850.csv") #tienda CLAREL
data_75223 <- read_csv("data/data_tienda_75223.csv") #tienda CLAREL


# se limpian los datos quitando venta negativa o venta mayor a 2000€
# se crean (agrupadas) las 3 variables a predecir: N_TICKETS, TICKET_MEDIO, UNIDADES


data_indice_congestion <- data %>% 
  group_by(TIENDA_ID,FECHA_TRAMO,CAJA) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),FECHA=FECHA) %>%
  group_by(TIENDA_ID,FECHA_TRAMO) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),N_CAJAS=n(),FECHA=FECHA) %>%
  distinct_all() %>%
  filter(VENTA_SIN_IVA>=0 & UNIDADES>=0,VENTA_SIN_IVA<=2000) %>%
  mutate(TICKET_MEDIO=VENTA_SIN_IVA/N_TICKETS) 


#PRUEBA DE LOS DATOS
data_indice_congestion_52288 <- filter(data_indice_congestion,FECHA>="2021-02-22" & FECHA<="2021-02-28",TIENDA_ID==52288)


################################################################################
###### MODELOS PREDICTIVOS #####################################################
################################################################################


################################################################################
# 3 modelos: tienda 52288
# Algoritmo: Prophet
# Datos de entrenamiento: datos desde el 25/01/21 al 21/02/21 (4 semanas)
# Predicción: datos desde el 22/02/21 al 28/02/21 (1 semana)
# variables a predecir: N_TICKETS, UNIDADES, TICKET_MEDIO
################################################################################

tienda_id <- 52288

# Datos de entrenamiento VARIABLE: N_TICKETS
train_data_tickets <- data_indice_congestion %>% 
  filter(FECHA>="2021-01-25" & FECHA<="2021-02-21",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,N_TICKETS) %>% 
  rename(ds=FECHA_TRAMO,y=N_TICKETS)

train_data_tickets <- genera_df_tickets_tramos("2021-01-25","2021-02-21",train_data_tickets)

# Datos de entrenamiento VARIABLE: TICKET_MEDIO
train_data_ticket_medio <- data_indice_congestion %>% 
  filter(FECHA>="2021-01-25" & FECHA<="2021-02-21",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,TICKET_MEDIO) %>% 
  rename(ds=FECHA_TRAMO,y=TICKET_MEDIO)

train_data_ticket_medio <- genera_df_tickets_tramos("2021-01-25","2021-02-21",train_data_ticket_medio)


# Datos de entrenamiento VARIABLE: UNIDADES
train_data_unidades <- data_indice_congestion %>% 
  filter(FECHA>="2021-01-25" & FECHA<="2021-02-21",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,UNIDADES) %>% 
  rename(ds=FECHA_TRAMO,y=UNIDADES)

train_data_unidades <- genera_df_tickets_tramos("2021-01-25","2021-02-21",train_data_unidades)

fig11 <- plot_ly(
  x = ymd_hms(as.character(train_data_tickets$ds)),
  y = train_data_tickets$y,
  name = "Número de Tickets",
  type = "bar"
)

fig11 <- fig11 %>% layout(
  title = "Datos de Entrenamiento Variable N_TICKETS - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig11

fig12 <- plot_ly(
  x = ymd_hms(as.character(train_data_ticket_medio$ds)),
  y = train_data_ticket_medio$y,
  name = "Ticket Medio",
  type = "bar"
)

fig12 <- fig12 %>% layout(
  title = "Datos de Entrenamiento Variable TICKET_MEDIO - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Valor medio (€) del ticket por tramo de 15 minutos")
)

fig12

fig13 <- plot_ly(
  x = ymd_hms(as.character(train_data_unidades$ds)),
  y = train_data_unidades$y,
  name = "Unidades",
  type = "bar"
)

fig13 <- fig13 %>% layout(
  title = "Datos de Entrenamiento Variable UNIDADES - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cantidad de artículos por tramo de 15 minutos")
)

fig13

# Datos de prueba del modelo

# VARIABLE N_TICKETS

test_data_tickets <- data_indice_congestion %>% 
  filter(FECHA>="2021-02-22" & FECHA<="2021-02-28",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,N_TICKETS) %>% 
  rename(ds=FECHA_TRAMO,y=N_TICKETS)

test_data_tickets <- genera_df_tickets_tramos("2021-02-22","2021-02-28",test_data_tickets)


# VARIABLE TICKET_MEDIO

test_data_ticket_medio <- data_indice_congestion %>% 
  filter(FECHA>="2021-02-22" & FECHA<="2021-02-28",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,TICKET_MEDIO) %>% 
  rename(ds=FECHA_TRAMO,y=TICKET_MEDIO)

test_data_ticket_medio <- genera_df_tickets_tramos("2021-02-22","2021-02-28",test_data_ticket_medio)

# VARIABLE UNIDADES

test_data_unidades <- data_indice_congestion %>% 
  filter(FECHA>="2021-02-22" & FECHA<="2021-02-28",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,UNIDADES) %>% 
  rename(ds=FECHA_TRAMO,y=UNIDADES)

test_data_unidades <- genera_df_tickets_tramos("2021-02-22","2021-02-28",test_data_unidades)

#gráficas de datos de validación del modelo

fig14 <- plot_ly(
  x = ymd_hms(as.character(test_data_tickets$ds)),
  y = test_data_tickets$y,
  name = "Tickets",
  type = "bar"
)

fig14 <- fig14 %>% layout(
  title = "Datos de Validación Variable N_TICKETS - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig14

fig15 <- plot_ly(
  x = ymd_hms(as.character(test_data_ticket_medio$ds)),
  y = test_data_ticket_medio$y,
  name = "Ticket Medio",
  type = "bar"
)

fig15 <- fig15 %>% layout(
  title = "Datos de Validación Variable TICKET_MEDIO - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Valor medio (€) del ticket por tramo de 15 minutos")
)

fig15

fig16 <- plot_ly(
  x = ymd_hms(as.character(test_data_unidades$ds)),
  y = test_data_unidades$y,
  name = "Unidades",
  type = "bar"
)

fig16 <- fig16 %>% layout(
  title = "Datos de Validación Variable UNIDADES - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cantidad de artículos por tramo de 15 minutos")
)

fig16


# creando los tres modelos para predecir N_TICKETS, TICKET_MEDIO, UNIDADES

# MODELO N_TICKETS

m_tickets <- prophet(train_data_tickets)
future_tickets <- make_future_dataframe(m, periods=96*7, freq = 15*60)

#verificando el DF future
nrow(future_tickets)-nrow(train_data_tickets)

# haciendo la predicción

forecast_tickets <- predict(m_tickets, future_tickets)
tail(forecast_tickets[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
forecast_tickets <- select(forecast_tickets,ds,yhat,yhat_lower,yhat_upper)
forecast_tickets$ts <- ymd_hms(forecast_tickets$ds)

#para evaluar el modelo me quedo solo con las fechas de la semana de validación
forecast_tickets <- filter(forecast_tickets,ts>=ymd_hms("2021-02-22 00:00:00"))


# evaluando el modelo

forecast_tickets$y <- test_data_tickets$y

# calculo el RSS para cada observación predicha
forecast_tickets$RSS <- (forecast_tickets$y - forecast_tickets$yhat)^2

forecast_tickets <- mutate(forecast_tickets,hora=hour(ts))

#lo siguiente es para calcular el error del modelo solamente en el horario comercial de las tiendas
forecast_tickets$RSS[forecast_tickets$hora<8 | forecast_tickets$hora>20] <- 0

#y también quito el domingo que no abre la tienda
forecast_tickets<- mutate(forecast_tickets,dia=weekdays(ts))
forecast_tickets <- filter(forecast_tickets,dia!="domingo")

n_no_cero <- nrow(filter(forecast_tickets,y>0))

rmse_tickets <- sqrt(sum(forecast_tickets$RSS))/n_no_cero

rmse_tickets
#0.2767499

#elimino el domingo que no abre la tienda
test_data_tickets <- mutate(test_data_tickets,ts=ymd_hms(ds))
test_data_tickets <- mutate(test_data_tickets,dia=weekdays(ts))
test_data_tickets <- filter(test_data_tickets,dia!="domingo")

#media de tickets por cada 15 min, sin contar las horas en las que está cerrada la tienda
test_data_tickets <- mutate(test_data_tickets,hora=hour(ts))
test_data_tickets <- filter(test_data_tickets,hora>=8 & hora<=20)


#media de tickets
mean(test_data_tickets$y)
#10.59295
rmse_tickets/mean(test_data_tickets$y)*100
#[1] 2.612586 - 2,61% de error relativo

#grafico predicción vs datos de validación

fig17 <- plot_ly(
  x = ymd_hms(as.character(test_data_tickets$ts)),
  y = test_data_tickets$y,
  name = "Tickets - Real",
  type = "bar"
)

fig17 <- fig17 %>% add_trace(
  x = ymd_hms(as.character(forecast_tickets$ts)),
  y = forecast_tickets$yhat,
  name = "Tickets - Predicción",
  type = "bar"
)

fig17 <- fig17 %>% layout(
  title = "Predicción vs Dato Real Variable N_TICKETS - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig17

# MODELO TICKET_MEDIO

m_ticket_medio <- prophet(train_data_ticket_medio)
future_ticket_medio <- make_future_dataframe(m, periods=96*7, freq = 15*60)

#verificando el DF future
nrow(future_ticket_medio)-nrow(train_data_ticket_medio)

# haciendo la predicción

forecast_ticket_medio <- predict(m_ticket_medio, future_ticket_medio)
tail(forecast_ticket_medio[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
forecast_ticket_medio <- select(forecast_ticket_medio,ds,yhat,yhat_lower,yhat_upper)
forecast_ticket_medio$ts <- ymd_hms(forecast_ticket_medio$ds)

#para evaluar el modelo me quedo solo con las fechas de la semana de validación
forecast_ticket_medio <- filter(forecast_ticket_medio,ts>=ymd_hms("2021-02-22 00:00:00"))


# evaluando el modelo

forecast_ticket_medio$y <- test_data_ticket_medio$y

# calculo el RSS para cada observación predicha
forecast_ticket_medio$RSS <- (forecast_ticket_medio$y - forecast_ticket_medio$yhat)^2

forecast_ticket_medio <- mutate(forecast_ticket_medio,hora=hour(ts))

#lo siguiente es para calcular el error del modelo solamente en el horario comercial de las tiendas
forecast_ticket_medio$RSS[forecast_ticket_medio$hora<8 | forecast_ticket_medio$hora>20] <- 0

#y también quito el domingo que no abre la tienda
forecast_ticket_medio<- mutate(forecast_ticket_medio,dia=weekdays(ts))
forecast_ticket_medio <- filter(forecast_ticket_medio,dia!="domingo")

n_no_cero <- nrow(filter(forecast_ticket_medio,y>0))

rmse_ticket_medio <- sqrt(sum(forecast_ticket_medio$RSS))/n_no_cero

rmse_ticket_medio
#0.2816793

#elimino el domingo que no abre la tienda
test_data_ticket_medio <- mutate(test_data_ticket_medio,ts=ymd_hms(ds))
test_data_ticket_medio <- mutate(test_data_ticket_medio,dia=weekdays(ts))
test_data_ticket_medio <- filter(test_data_ticket_medio,dia!="domingo")


#media de ticket_medio por cada 15 min, sin contar las horas en las que está cerrada la tienda
test_data_ticket_medio <- mutate(test_data_ticket_medio,hora=hour(ts))
test_data_ticket_medio <- filter(test_data_ticket_medio,hora>=8 & hora<=20)


#media de ticket_medio
mean(test_data_ticket_medio$y)
#9.290253
rmse_ticket_medio/mean(test_data_ticket_medio$y)*100
#[1] x1987 - 3,03% de error relativo

#grafico predicción vs datos de validación

fig18 <- plot_ly(
  x = ymd_hms(as.character(test_data_ticket_medio$ts)),
  y = test_data_ticket_medio$y,
  name = "Ticket Medio - Real",
  type = "bar"
)

fig18 <- fig18 %>% add_trace(
  x = ymd_hms(as.character(forecast_ticket_medio$ts)),
  y = forecast_ticket_medio$yhat,
  name = "Ticket Medio - Predicción",
  type = "bar"
)

fig18 <- fig18 %>% layout(
  title = "Predicción vs Dato Real Variable TICKET_MEDIO - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Valor medio (€) del ticket por tramo de 15 minutos")
)

fig18

# MODELO unidades

m_unidades <- prophet(train_data_unidades)
future_unidades <- make_future_dataframe(m, periods=96*7, freq = 15*60)

#verificando el DF future
nrow(future_unidades)-nrow(train_data_unidades)

# haciendo la predicción

forecast_unidades <- predict(m_unidades, future_unidades)
tail(forecast_unidades[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
forecast_unidades <- select(forecast_unidades,ds,yhat,yhat_lower,yhat_upper)
forecast_unidades$ts <- ymd_hms(forecast_unidades$ds)

#para evaluar el modelo me quedo solo con las fechas de la semana de validación
forecast_unidades <- filter(forecast_unidades,ts>=ymd_hms("2021-02-22 00:00:00"))


# evaluando el modelo

forecast_unidades$y <- test_data_unidades$y

# calculo el RSS para cada observación predicha
forecast_unidades$RSS <- (forecast_unidades$y - forecast_unidades$yhat)^2

forecast_unidades <- mutate(forecast_unidades,hora=hour(ts))

#lo siguiente es para calcular el error del modelo solamente en el horario comercial de las tiendas
forecast_unidades$RSS[forecast_unidades$hora<8 | forecast_unidades$hora>20] <- 0

#y también quito el domingo que no abre la tienda
forecast_unidades<- mutate(forecast_unidades,dia=weekdays(ts))
forecast_unidades <- filter(forecast_unidades,dia!="domingo")

n_no_cero <- nrow(filter(forecast_unidades,y>0))

rmse_unidades <- sqrt(sum(forecast_unidades$RSS))/n_no_cero

rmse_unidades
#2.392518

#elimino el domingo que no abre la tienda
test_data_unidades <- mutate(test_data_unidades,ts=ymd_hms(ds))
test_data_unidades <- mutate(test_data_unidades,dia=weekdays(ts))
test_data_unidades <- filter(test_data_unidades,dia!="domingo")

#media de unidades por cada 15 min, sin contar las horas en las que está cerrada la tienda
test_data_unidades <- mutate(test_data_unidades,hora=hour(ts))
test_data_unidades <- filter(test_data_unidades,hora>=8 & hora<=20)


#media de unidades
mean(test_data_unidades$y)
#79.54808
rmse_unidades/mean(test_data_unidades$y)*100
#[1] 3.007638 - 3% de error relativo

#grafico predicción vs datos de validación

fig19 <- plot_ly(
  x = ymd_hms(as.character(test_data_unidades$ts)),
  y = test_data_unidades$y,
  name = "Ticket Medio - Real",
  type = "bar"
)

fig19 <- fig19 %>% add_trace(
  x = ymd_hms(as.character(forecast_unidades$ts)),
  y = forecast_unidades$yhat,
  name = "Ticket Medio - Predicción",
  type = "bar"
)

fig19 <- fig19 %>% layout(
  title = "Predicción vs Dato Real Variable UNIDADES - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cantidad de artículos por tramo de 15 minutos")
)

fig19

### CÁLCULO DEL INDICE DE CONGESTIÓN PARA LA SEMANA DE PREDICCIÓN

# Estandarización de las 3 variables

# N_TICKETS predicha

#llevo a 0 la predicción en los tramos fuera del horario comercial
forecast_tickets$yhat_clean <- if_else(forecast_tickets$yhat>=0,forecast_tickets$yhat,0)
forecast_tickets$yhat_clean[forecast_tickets$hora<8 | forecast_tickets$hora>20] <- 0

# Estandarización de la variable tickets predicha
forecast_tickets$yhat_clean_std <- forecast_tickets$yhat_clean/max(forecast_tickets$yhat_clean)
forecast_tickets$y_std <- forecast_tickets$y/max(forecast_tickets$y)

summary(forecast_tickets)

# TICKET_MEDIO predicha

#llevo a 0 la predicción en los tramos fuera del horario comercial
forecast_ticket_medio$yhat_clean <- if_else(forecast_ticket_medio$yhat>=0,forecast_ticket_medio$yhat,0)
forecast_ticket_medio$yhat_clean[forecast_ticket_medio$hora<8 | forecast_ticket_medio$hora>20] <- 0

# Estandarización de la variable ticket_medio predicha
forecast_ticket_medio$yhat_clean_std <- forecast_ticket_medio$yhat_clean/max(forecast_ticket_medio$yhat_clean)
forecast_ticket_medio$y_std <- forecast_ticket_medio$y/max(forecast_ticket_medio$y)

summary(forecast_ticket_medio)

# UNIDADES predicha

#llevo a 0 la predicción en los tramos fuera del horario comercial
forecast_unidades$yhat_clean <- if_else(forecast_unidades$yhat>=0,forecast_unidades$yhat,0)
forecast_unidades$yhat_clean[forecast_unidades$hora<8 | forecast_unidades$hora>20] <- 0

# Estandarización de la variable unidades predicha
forecast_unidades$yhat_clean_std <- forecast_unidades$yhat_clean/max(forecast_unidades$yhat_clean)
forecast_unidades$y_std <- forecast_unidades$y/max(forecast_unidades$y)


summary(forecast_unidades)

# Calculo del indice de congestión

indice_congestion <- data.frame(ds=forecast_tickets$ds, 
                                tickets_hat_std=forecast_tickets$yhat_clean_std,
                                ticket_medio_hat_std=forecast_ticket_medio$yhat_clean_std,
                                unidades_hat_std=forecast_unidades$yhat_clean_std)
summary(indice_congestion)

indice_congestion <- mutate(indice_congestion,indice_congestion_hat=tickets_hat_std*ticket_medio_hat_std*unidades_hat_std)
summary(indice_congestion)

# evaluación de la predicción del índice de congestión


indice_congestion$indice_congestion_real <- forecast_tickets$y_std * forecast_ticket_medio$y_std * forecast_unidades$y_std

# calculo el RSS para cada observación predicha
indice_congestion$RSS <- (indice_congestion$indice_congestion_real - indice_congestion$indice_congestion_hat)^2

summary(indice_congestion)



#tomando en cuenta solo las predicciones donde el valor real es distinto de cero para el cálculo del RMSE

n_no_cero <- nrow(filter(indice_congestion,indice_congestion_real>0))
rmse_ind_congestion <- sqrt(sum(indice_congestion$RSS))/n_no_cero
rmse_ind_congestion
#0.01753313

#error relativo
mean(indice_congestion$indice_congestion_real)
#0.01681047

rmse_ind_congestion/mean(indice_congestion$indice_congestion_real)*100

#gráfica del índice de congestión real vs predicho

fig20 <- plot_ly(
  x = ymd_hms(as.character(indice_congestion$ds)),
  y = indice_congestion$indice_congestion_real,
  name = "Índice de Congestión - Real",
  type = "scatter",
  mode = "line"
)

fig20 <- fig20 %>% add_trace(
  x = ymd_hms(as.character(indice_congestion$ds)),
  y = indice_congestion$indice_congestion_hat,
  name = "Índice de Congestión - Predicción",
  type = "scatter",
  mode = "line"
)

fig20 <- fig20 %>% layout(
  title = "Predicción vs Real del Índice de Congestión - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Índice de Congestión")
)

fig20

