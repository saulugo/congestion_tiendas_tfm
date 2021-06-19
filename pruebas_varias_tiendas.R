library(prophet)
library(tidyverse)
library(lubridate)
library(plotly)
source('~/master/TFM/codigo/funciones_auxiliares.R', encoding = 'UTF-8')

#graficando la predicción del índice de congestión, varias tiendas, distintos períodos


#feriado del 12 de octubbre en los datos a predecir. Tienda DIA
fecha_ini_train <- "2020-09-14"
fecha_fin_train <- "2020-10-11"
fecha_ini_test <- "2020-10-12"
fecha_fin_test <- "2020-10-18"
tienda_id <- 10061


prediccion_tienda <- predice_indice_congestion(tienda_id,fecha_ini_train,fecha_fin_train,data)

dat_plot_cajas_tramo <- data %>% 
  filter(TIENDA_ID==tienda_id) %>% 
  group_by(FECHA_TRAMO,CAJA) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),FECHA=FECHA) %>% 

  group_by(FECHA_TRAMO) %>% summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),N_CAJAS=n(),FECHA=FECHA) %>%
  distinct_all() %>%
  filter(VENTA_SIN_IVA>0) %>%
  mutate(TICKET_MEDIO=VENTA_SIN_IVA/N_TICKETS) %>%
  filter(FECHA>=fecha_ini_test & FECHA<=fecha_fin_test)



fig20 <- plot_ly(
  x = ymd_hms(as.character(dat_plot_cajas_tramo$FECHA_TRAMO)),
  y = dat_plot_cajas_tramo$N_CAJAS,
  name = "CAJAS ABIERTAS",
  type = "bar"
)

fig20 <- fig20 %>% add_trace(
  x = ymd_hms(as.character(prediccion_tienda$ds)),
  y = prediccion_tienda$indice_congestion_hat,
  name = "IND CONGESTION",
  type = "scatter",
  mode = "line",
  line = list(color ="red",width=2),
  yaxis = "y2"
)

fig20 <- fig20 %>% layout(
  yaxis2 = list(overlaying = "y", side = "right",title="Índice de Congestión de Caja (0 a 1)"),
  title = paste("Índice de Congestión - Tienda",tienda_id),
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cajas abiertas")
)

fig20





#sin feriados, tienda DIA
fecha_ini_train <- "2021-02-01"
fecha_fin_train <- "2021-02-28"
fecha_ini_test <- "2021-03-01"
fecha_fin_test <- "2021-03-07"
tienda_id <- 2266


prediccion_tienda <- predice_indice_congestion(tienda_id,fecha_ini_train,fecha_fin_train,data)

dat_plot_cajas_tramo <- data %>% 
  filter(TIENDA_ID==tienda_id) %>% 
  group_by(FECHA_TRAMO,CAJA) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),FECHA=FECHA) %>% 
  
  group_by(FECHA_TRAMO) %>% summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),N_CAJAS=n(),FECHA=FECHA) %>%
  distinct_all() %>%
  filter(VENTA_SIN_IVA>0) %>%
  mutate(TICKET_MEDIO=VENTA_SIN_IVA/N_TICKETS) %>%
  filter(FECHA>=fecha_ini_test & FECHA<=fecha_fin_test)



fig20 <- plot_ly(
  x = ymd_hms(as.character(dat_plot_cajas_tramo$FECHA_TRAMO)),
  y = dat_plot_cajas_tramo$N_CAJAS,
  name = "CAJAS ABIERTAS",
  type = "bar"
)

fig20 <- fig20 %>% add_trace(
  x = ymd_hms(as.character(prediccion_tienda$ds)),
  y = prediccion_tienda$indice_congestion_hat,
  name = "IND CONGESTION",
  type = "scatter",
  mode = "line",
  line = list(color ="red",width=2),
  yaxis = "y2"
)

fig20 <- fig20 %>% layout(
  yaxis2 = list(overlaying = "y", side = "right",title="Índice de Congestión de Caja (0 a 1)"),
  title = paste("Índice de Congestión - Tienda",tienda_id),
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cajas abiertas")
)

fig20


#varios feriados en el entrenamiento y feriados en la predicción, tienda La Plaza
fecha_ini_train <- "2020-03-30"
fecha_fin_train <- "2020-04-26"
fecha_ini_test <- "2020-04-27"
fecha_fin_test <- "2020-05-03"
tienda_id <- 53116 


prediccion_tienda <- predice_indice_congestion(tienda_id,fecha_ini_train,fecha_fin_train,data)

dat_plot_cajas_tramo <- data %>% 
  filter(TIENDA_ID==tienda_id) %>% 
  group_by(FECHA_TRAMO,CAJA) %>% 
  summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),FECHA=FECHA) %>% 
  
  group_by(FECHA_TRAMO) %>% summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),N_CAJAS=n(),FECHA=FECHA) %>%
  distinct_all() %>%
  filter(VENTA_SIN_IVA>0) %>%
  mutate(TICKET_MEDIO=VENTA_SIN_IVA/N_TICKETS) %>%
  filter(FECHA>=fecha_ini_test & FECHA<=fecha_fin_test)



fig20 <- plot_ly(
  x = ymd_hms(as.character(dat_plot_cajas_tramo$FECHA_TRAMO)),
  y = dat_plot_cajas_tramo$N_CAJAS,
  name = "CAJAS ABIERTAS",
  type = "bar"
)

fig20 <- fig20 %>% add_trace(
  x = ymd_hms(as.character(prediccion_tienda$ds)),
  y = prediccion_tienda$indice_congestion_hat,
  name = "IND CONGESTION",
  type = "scatter",
  mode = "line",
  line = list(color ="red",width=2),
  yaxis = "y2"
)

fig20 <- fig20 %>% layout(
  yaxis2 = list(overlaying = "y", side = "right",title="Índice de Congestión de Caja (0 a 1)"),
  title = paste("Índice de Congestión - Tienda",tienda_id),
  xaxis = list(title="Fecha"),
  yaxis = list(title="Cajas abiertas")
)

fig20


