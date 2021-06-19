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
#data_por_tramo_total_cajas <- data %>% 
#  group_by(TIENDA_ID,FECHA,HORA,TRAMO,FECHA_TRAMO) %>% 
#  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),KILOS=sum(KILOS)) %>%
#  filter(VENTA_SIN_IVA>=0 & UNIDADES>=0 & KILOS>=0,VENTA_SIN_IVA<=2000)

data_por_tramo_total_cajas <- data %>% 
  group_by(TIENDA_ID,FECHA,HORA,TRAMO,FECHA_TRAMO) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),KILOS=sum(KILOS)) %>%
  filter(VENTA_SIN_IVA>=0 & UNIDADES>=0 & KILOS>=0,VENTA_SIN_IVA<=2000) %>%
  select(TIENDA_ID,FECHA_TRAMO,N_TICKETS)


test3 <- filter(data_por_tramo_total_cajas,FECHA=="2020-01-02",TIENDA_ID==52288,HORA==13)
# test3 el número de tickets tiene que dar 31, 16, 26, 17 para un total de 90

################################################################################
###### MODELOS PREDICTIVOS #####################################################
################################################################################


################################################################################
# 1er modelo: tienda 52288
# Algoritmo: Prophet
# Datos de entrenamiento: datos desde el 25/01/21 al 21/02/21 (4 semanas)
# Predicción: datos desde el 22/02/21 al 28/02/21 (1 semana)
################################################################################

tienda_id <- 52288

# Datos de entrenamiento
train_data <- data_por_tramo_total_cajas %>% 
  filter(FECHA>="2021-01-25" & FECHA<="2021-02-21",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,N_TICKETS) %>% 
  rename(ds=FECHA_TRAMO,y=N_TICKETS)

train_data <- genera_df_tickets_tramos("2021-01-25","2021-02-21",train_data)

fig5 <- plot_ly(
  x = ymd_hms(as.character(train_data$ds)),
  y = train_data$y,
  name = "Tickets",
  type = "bar"
)

fig5 <- fig5 %>% layout(
  title = "Datos de Entrenamiento - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig5

# Datos de prueba del modelo

test_data <- data_por_tramo_total_cajas %>% 
  filter(FECHA>="2021-02-22" & FECHA<="2021-02-28",TIENDA_ID==tienda_id) %>%
  as.data.frame() %>% 
  select(FECHA_TRAMO,N_TICKETS) %>% 
  rename(ds=FECHA_TRAMO,y=N_TICKETS)

test_data <- genera_df_tickets_tramos("2021-02-22","2021-02-28",test_data)

#elimino el domingo que no abre la tienda
test_data <- mutate(test_data,ts=ymd_hms(ds))
test_data <- mutate(test_data,dia=weekdays(ts))
test_data <- filter(test_data,dia!="domingo")

fig6 <- plot_ly(
  x = ymd_hms(as.character(test_data$ds)),
  y = test_data$y,
  name = "Tickets",
  type = "bar"
)

fig6 <- fig6 %>% layout(
  title = "Datos de Validación - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig6
# creando el modelo


m <- prophet(train_data)
future <- make_future_dataframe(m, periods=96*7, freq = 15*60)

#verificando el DF future
nrow(future)-nrow(train_data)

# haciendo la predicción

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
my_forecast <- select(forecast,ds,yhat,yhat_lower,yhat_upper)
my_forecast$ts <- ymd_hms(my_forecast$ds)

#para evaluar el modelo me quedo solo con las fechas de la semana de validación
my_forecast <- filter(my_forecast,ts>=ymd_hms("2021-02-22 00:00:00"))


# evaluando el modelo

my_forecast$y <- test_data$y

# calculo el RSS para cada observación predicha
my_forecast$RSS <- (my_forecast$y - my_forecast$yhat)^2

my_forecast <- mutate(my_forecast,hora=hour(ts))

#lo siguiente es para calcular el error del modelo solamente en el horario comercial de las tiendas
my_forecast$RSS[my_forecast$hora<8 | my_forecast$hora>20] <- 0

#y también quito el domingo que no abre la tienda
my_forecast<- mutate(my_forecast,dia=weekdays(ts))
my_forecast <- filter(my_forecast,dia!="domingo")

rmse <- sqrt(sum(my_forecast$RSS))/nrow(my_forecast)

rmse
#0.1388554

#media de tickets por cada 15 min, sin contar las horas en las que está cerrada la tienda
test_data <- mutate(test_data,hora=hour(ts))
test_data2 <- filter(test_data,hora>=8 & hora<=20)

#media de tickets
mean(test_data2$y)
#10,59
#0.14/10.59*100
#[1] 1.322002

#grafico predicción vs datos de validación

fig7 <- plot_ly(
  x = ymd_hms(as.character(test_data$ts)),
  y = test_data$y,
  name = "Tickets - Real",
  type = "bar"
)

fig7 <- fig7 %>% add_trace(
  x = ymd_hms(as.character(my_forecast$ts)),
  y = my_forecast$yhat,
  name = "Tickets - Predicción",
  type = "bar"
)

fig7 <- fig7 %>% layout(
  title = "Predicción vs Dato Real - Tienda 52288",
  xaxis = list(title="Fecha"),
  yaxis = list(title="Número de tickets por tramo de 15 minutos")
)

fig7

# plot the forecast
plot(m, forecast)

# view the forecast componets

prophet_plot_components(m, forecast)  



