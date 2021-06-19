# Preprocesado de los datos
# Objetivo: separar los datos para cada tienda y obtener 10 datasets para las
# 10 tiendas de la muestra.
################################################################################


library(tidyverse)
library(logging)

# Configuro el logging para nivel básico: muestra info, warning y error.
logging::basicConfig()

# lectura de los datos

data <- read.csv("data/data.csv")
head(data)
summary(data)

# pre-procesado de los datos de las 10 tiendas de la muestra

lista_tiendas <- unique(data$TIENDA_ID)

for (tienda_id in lista_tiendas) {
  data_tienda <- data %>% filter(TIENDA_ID==tienda_id)
  write.csv(data_tienda,file=paste0("data/data_tienda_",tienda_id,".csv"),row.names = FALSE)
  "Almacenando datos de la tienda ${tienda_id}" %>% 
    str_interp() %>% 
    loginfo()
  
  cantidad_cajas <- length(unique(data_tienda$CAJA))
  
  "La tienda ${tienda_id} tiene ${cantidad_cajas} cajas " %>% 
    str_interp() %>% 
    loginfo()
}


"Finalizado el pre-procesado de datos" %>% 
  str_interp() %>% 
  loginfo()


# Lectura de los datos para AED

library(tidyverse)

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

