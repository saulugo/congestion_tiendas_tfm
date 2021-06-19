library(tidyverse)
library(lubridate)
library(prophet)
library(logging)

# Configuro el logging para nivel básico: muestra info, warning y error.
logging::basicConfig()

################################################################################
# devuelve el valor de "y" que corresponde con el timestamp tiempo_actual
# si no consigue ningún valor que corresponda con tiempo_actual, devuelve 0
################################################################################

obtiene_y <- function(tiempo_actual,data){
  mydata <- filter(data,ds==as.character(tiempo_actual))
  
  if (nrow(mydata)>0) {
    valor_y <- mydata$y
  } 
  else {
    valor_y <- 0
  }
    
  valor_y
}

################################################################################
# formatea un objeto fecha-hora en forma de character y
# agrega la hora 00:00:00
################################################################################

formatea_ts <- function(tiempo_actual){
  if(hour(tiempo_actual)==0 & minute(tiempo_actual)==0 & second(tiempo_actual)==0){
    mi_hora <- "00:00:00"
    ts_c <- paste(as.character(tiempo_actual),mi_hora)
  } else{
    ts_c <- as.character(tiempo_actual)
  }
  ts_c
}


################################################################################
# toma un rango de 2 fechas y un dataframe que contiene el número
# de tickets por tramos de 15 minutos y devuelve un dataframe
# con el valor de tickets en todos los tramos de 15 minutos posibles
# entre las dos fechas
# 1 día (24 horas) tiene 96 observaciones de tramos de 15 minutos.
################################################################################

genera_df_tickets_tramos <- function(fecha_ini,fecha_fin,data){
  fecha_ini <- paste(as.character(fecha_ini),"00:00:00")
  fecha_fin <- paste(as.character(fecha_fin),"23:45:00")
  
  tiempo_actual <- ymd_hms(fecha_ini)
  tiempo_final <- ymd_hms(fecha_fin)
  
  #vector para almacenar el eje de tiempos del df final
  ds <- formatea_ts(tiempo_actual)
  ds <- as.vector(ds)
  
  #vector para almacenar el número de tickets en cada tramo horario para el 
  #df final
  
  y <- obtiene_y(tiempo_actual,data)
  y <- as.vector(y)
  
  while(tiempo_actual < tiempo_final){
      tiempo_actual <- tiempo_actual + minutes(15)
      ds <- append(ds,formatea_ts(tiempo_actual))
      y <- append(y,obtiene_y(tiempo_actual,data))
  }
  
  df <- data.frame(ds=ds,y=y)
  df
  
}


################################################################################
# Predice el índice de congestión para una lista de tiendas
################################################################################


predice_indice_congestion <- function(l_tiendas,fecha_ini_train, fecha_fin_train,data){
  todos_ind_congestion <- data.frame(ds=vector(),tienda_id=vector(),ind_congestion_hat=vector())
  
  "Predicción de las tiendas ${l_tiendas}" %>% 
    str_interp() %>% 
    loginfo()
  
  "Datos de entrenamiento desde ${fecha_ini_train} hasta ${fecha_fin_train} " %>% 
    str_interp() %>% 
    loginfo()
  
  for (tienda_id in l_tiendas) {
    
    "Preparando datos de entrenamiento de la tienda ${tienda_id}" %>% 
      str_interp() %>% 
      loginfo()
    
    data_indice_congestion <- data %>% 
      group_by(TIENDA_ID,FECHA_TRAMO,CAJA) %>% 
      summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),FECHA=FECHA) %>%
      group_by(TIENDA_ID,FECHA_TRAMO) %>% 
      summarise(N_TICKETS=sum(N_TICKETS),VENTA_SIN_IVA=sum(VENTA_SIN_IVA),UNIDADES=sum(UNIDADES),N_CAJAS=n(),FECHA=FECHA) %>%
      distinct_all() %>%
      filter(VENTA_SIN_IVA>=0 & UNIDADES>=0,VENTA_SIN_IVA<=2000) %>%
      mutate(TICKET_MEDIO=VENTA_SIN_IVA/N_TICKETS) 
    
    # Datos de entrenamiento VARIABLE: N_TICKETS
    
    "Datos de entrenamiento, variable N_TICKETS" %>% 
      str_interp() %>% 
      loginfo()
    
    train_data_tickets <- data_indice_congestion %>% 
      filter(FECHA>=fecha_ini_train & FECHA<=fecha_fin_train,TIENDA_ID==tienda_id) %>%
      as.data.frame() %>% 
      select(FECHA_TRAMO,N_TICKETS) %>% 
      rename(ds=FECHA_TRAMO,y=N_TICKETS)
    
    train_data_tickets <- genera_df_tickets_tramos(fecha_ini_train,fecha_fin_train,train_data_tickets)
    
    # Datos de entrenamiento VARIABLE: TICKET_MEDIO
    
    "Datos de entrenamiento, variable TICKET_MEDIO" %>% 
      str_interp() %>% 
      loginfo()
    
    train_data_ticket_medio <- data_indice_congestion %>% 
      filter(FECHA>=fecha_ini_train & FECHA<=fecha_fin_train,TIENDA_ID==tienda_id) %>%
      as.data.frame() %>% 
      select(FECHA_TRAMO,TICKET_MEDIO) %>% 
      rename(ds=FECHA_TRAMO,y=TICKET_MEDIO)
    
    train_data_ticket_medio <- genera_df_tickets_tramos(fecha_ini_train,fecha_fin_train,train_data_ticket_medio)
    
    # Datos de entrenamiento VARIABLE: UNIDADES
    
    "Datos de entrenamiento, variable UNIDADES" %>% 
      str_interp() %>% 
      loginfo()
    
    train_data_unidades <- data_indice_congestion %>% 
      filter(FECHA>=fecha_ini_train & FECHA<=fecha_fin_train,TIENDA_ID==tienda_id) %>%
      as.data.frame() %>% 
      select(FECHA_TRAMO,UNIDADES) %>% 
      rename(ds=FECHA_TRAMO,y=UNIDADES)
    
    train_data_unidades <- genera_df_tickets_tramos(fecha_ini_train,fecha_fin_train,train_data_unidades)
    
    # creando los tres modelos para predecir N_TICKETS, TICKET_MEDIO, UNIDADES
    
    # MODELO N_TICKETS
    
    "Creando modelo: N_TICKETS" %>% 
      str_interp() %>% 
      loginfo()
    
    m_tickets <- prophet(train_data_tickets)
    future_tickets <- make_future_dataframe(m_tickets, periods=96*7, freq = 15*60)
    
    # haciendo la predicción
    
    "Prediciendo variable: N_TICKETS" %>% 
      str_interp() %>% 
      loginfo()
    
    forecast_tickets <- predict(m_tickets, future_tickets)
    forecast_tickets <- select(forecast_tickets,ds,yhat,yhat_lower,yhat_upper)
    forecast_tickets$ts <- ymd_hms(forecast_tickets$ds)
    
    #para evaluar el modelo me quedo solo con las fechas de la semana de validación
    forecast_tickets <- filter(forecast_tickets,ts>=(ymd_hms(paste(fecha_fin_train,"00:00:00"))+3600*24))
    
    # MODELO TICKET_MEDIO
    
    "Creando modelo: TICKET_MEDIO" %>% 
      str_interp() %>% 
      loginfo()
    
    m_ticket_medio <- prophet(train_data_ticket_medio)
    future_ticket_medio <- make_future_dataframe(m_ticket_medio, periods=96*7, freq = 15*60)
    
    # haciendo la predicción
    
    "Prediciendo variable: TICKET_MEDIO" %>% 
      str_interp() %>% 
      loginfo()
    
    forecast_ticket_medio <- predict(m_ticket_medio, future_ticket_medio)
    forecast_ticket_medio <- select(forecast_ticket_medio,ds,yhat,yhat_lower,yhat_upper)
    forecast_ticket_medio$ts <- ymd_hms(forecast_ticket_medio$ds)
    
    #para evaluar el modelo me quedo solo con las fechas de la semana de validación
    forecast_ticket_medio <- filter(forecast_ticket_medio,ts>=(ymd_hms(paste(fecha_fin_train,"00:00:00"))+3600*24))
    
    # MODELO unidades
    
    "Creando modelo: UNIDADES" %>% 
      str_interp() %>% 
      loginfo()
    
    m_unidades <- prophet(train_data_unidades)
    future_unidades <- make_future_dataframe(m_unidades, periods=96*7, freq = 15*60)
    
    
    # haciendo la predicción
    
    "Prediciendo variable: UNIDADES" %>% 
      str_interp() %>% 
      loginfo()
    
    forecast_unidades <- predict(m_unidades, future_unidades)
    tail(forecast_unidades[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    forecast_unidades <- select(forecast_unidades,ds,yhat,yhat_lower,yhat_upper)
    forecast_unidades$ts <- ymd_hms(forecast_unidades$ds)
    
    #para evaluar el modelo me quedo solo con las fechas de la semana de validación
    forecast_unidades <- filter(forecast_unidades,ts>=(ymd_hms(paste(fecha_fin_train,"00:00:00"))+3600*24))
    
    ### CÁLCULO DEL INDICE DE CONGESTIÓN PARA LA SEMANA DE PREDICCIÓN
    
    "Calculando índice de congestión de la tienda ${tienda_id} " %>% 
      str_interp() %>% 
      loginfo()
    
    # Estandarización de las 3 variables
    
    # N_TICKETS predicha
    
    #llevo a 0 la predicción en los tramos fuera del horario comercial
    forecast_tickets$yhat_clean <- if_else(forecast_tickets$yhat>=0,forecast_tickets$yhat,0)
    forecast_tickets$yhat_clean[forecast_tickets$hora<8 | forecast_tickets$hora>20] <- 0
    
    # Estandarización de la variable tickets predicha
    forecast_tickets$yhat_clean_std <- forecast_tickets$yhat_clean/max(forecast_tickets$yhat_clean)
    
    # TICKET_MEDIO predicha
    
    #llevo a 0 la predicción en los tramos fuera del horario comercial
    forecast_ticket_medio$yhat_clean <- if_else(forecast_ticket_medio$yhat>=0,forecast_ticket_medio$yhat,0)
    forecast_ticket_medio$yhat_clean[forecast_ticket_medio$hora<8 | forecast_ticket_medio$hora>20] <- 0
    
    # Estandarización de la variable ticket_medio predicha
    forecast_ticket_medio$yhat_clean_std <- forecast_ticket_medio$yhat_clean/max(forecast_ticket_medio$yhat_clean)
    
    # UNIDADES predicha
    
    #llevo a 0 la predicción en los tramos fuera del horario comercial
    forecast_unidades$yhat_clean <- if_else(forecast_unidades$yhat>=0,forecast_unidades$yhat,0)
    forecast_unidades$yhat_clean[forecast_unidades$hora<8 | forecast_unidades$hora>20] <- 0
    
    # Estandarización de la variable unidades predicha
    forecast_unidades$yhat_clean_std <- forecast_unidades$yhat_clean/max(forecast_unidades$yhat_clean)
    
    # Calculo del indice de congestión
    
    indice_congestion <- data.frame(ds=forecast_tickets$ds, 
                                    tickets_hat_std=forecast_tickets$yhat_clean_std,
                                    ticket_medio_hat_std=forecast_ticket_medio$yhat_clean_std,
                                    unidades_hat_std=forecast_unidades$yhat_clean_std)
    
    indice_congestion <- mutate(indice_congestion,indice_congestion_hat=tickets_hat_std*ticket_medio_hat_std*unidades_hat_std)
    indice_congestion$tienda_id <- tienda_id
    
    todos_ind_congestion <- rbind(todos_ind_congestion,select(indice_congestion,ds,tienda_id,indice_congestion_hat))
    
    "Predicción completada para la tienda ${tienda_id} " %>% 
      str_interp() %>% 
      loginfo()

  }
  
  
  "Predicción completada para todas las tiendas, bye!" %>% 
    str_interp() %>% 
    loginfo()
  todos_ind_congestion$ts <- ymd_hms(todos_ind_congestion$ds)
  
  #lo siguiente es para calcular el error del modelo solamente en el horario comercial de las tiendas
  todos_ind_congestion <- mutate(todos_ind_congestion,hora=hour(ts))
  todos_ind_congestion$indice_congestion_hat[todos_ind_congestion$hora<8 | todos_ind_congestion$hora>20] <- 0
  
  todos_ind_congestion
}



