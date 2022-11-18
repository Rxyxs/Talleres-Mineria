library(readr)
library(tibble)
# Datos 
agvAPI <- readRDS("C:/Rmdtos/data/data_agvAPI.rds")
agrometAPI <- readRDS("C:/Rmdtos/data/data_estaciones_agrometAPI.rds") 
meta_agromet <- readRDS("C:/Rmdtos/data/metadata_estaciones_agrometAPI.rds")
meta_agv <- readRDS("C:/Rmdtos/data/metadata_estaciones_agvAPI(1).rds")  

#Mascaras 
mascara_agromet <- readRDS("C:/Rmdtos/data/station_id_agromet_Pablo.rds")
mascara_agv <- readRDS("C:/Rmdtos/data/station_id_agv_Pablo.rds")

#Aplicación de las mascaras 
## datos station de agromet y agv
meta_agromet <- meta_agromet[meta_agromet$ema %in% mascara_agromet,]
meta_agv <- meta_agv[meta_agv$serial %in% mascara_agv,]

agrometAPI <- agrometAPI[agrometAPI$station_id %in% mascara_agromet,]
agvAPI_rows <- as.numeric(rownames(meta_agv[meta_agv$serial %in% mascara_agv,]))
agvAPI <- agvAPI[c(agvAPI_rows)]
#1
class(meta_agromet)
class(meta_agv)
class(agrometAPI)
class(agvAPI)


#punto 2 (falta la mitad)
#agvAPI <- readRDS("data_agvAPI.rds")

sapply(agvAPI, dim.data.frame)


#punto 3
class(agrometAPI)
apply(agrometAPI,agrometAPI$precipitacion_horaria, sum)
apply(agrometAPI, agrometAPI$temp_promedio_aire, mean)
apply(agrometAPI, agrometAPI$temp_minima, mean)
apply(agrometAPI, agrometAPI$temp_maxima, mean)
#punto 4


#6
summary(tibble_tagrometAPI)
sum(is.na(agrometAPI))

summary(agvAPI)
sum(is.na(agvAPI))






#apply(data_agromet_Pablo, 2 , sum)
dia <- format(agrometAPI$dia)
diagrom <- tapply(agrometAPI['dia'], 'precipitacion_horaria' ,sum)

data_anidad <- 
  agrometAPI |> nest(neted_col = 2:13)

data_anidad
