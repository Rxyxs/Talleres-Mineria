#Taller2  
library(readr)
library(tidyr)
library(dplyr)
#Datos
agroAPI <- readRDS("C:/Rmdtos/data_raw/data_estaciones_agrometAPI.rds") 
meta_agro <- readRDS("C:/Rmdtos/data_raw/metadata_estaciones_agrometAPI.rds")

#Mascaras 
mascara_agromet <- readRDS("C:/Rmdtos/data_raw/station_id_agromet_Pablo.rds")
mascara_agv <- readRDS("C:/Rmdtos/data_raw/station_id_agv_Pablo.rds")

#Mascaras

#Sets de datos
agv_meta <- readRDS("C:/Rmdtos/data_raw/metadata_estaciones_agvAPI(1).rds")
api_agv <- readRDS("C:/Rmdtos/data_raw/data_agvAPI.rds")

#Aplicación de máscaras
agv_meta <- agv_meta[agv_meta$serial %in% mascara_agv,]
api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$serial %in% mascara_agv,]))

api_agv_rows <- as.numeric(rownames(agv_meta[agv_meta$tipo == "Humedad_Suelo",]))
api_agv_rows <- api_agv_rows[!is.na(api_agv_rows)]
data <- api_agv[c(api_agv_rows)]

#Filtros

#145 filas estaciones 
#cantidad de variables
cols <- sapply(data,ncol)
vars <- sapply(data,nrow)
# varibles unicas 
vars_un<- unique(vars)
ind785 <- vars%in%c(7,8,5)
data785 <- data[ind785]
# los indicadores de las estaciones que tienen 7 variables

serial_est<- agv_meta$serial[ind785]

# separar para cada caso 
data_sep <- lapply(vars_un,function(x){
  data785[x == vars]
})

#agrupa en una lista las estaciones que tienen la misma cantidad de variables 
#separa los valores unicos

#nombres de variables 
names <-  sapply(data_sep[[1]], purrr::pluck,1)

##primer caso
#seleccionar las tres primeras filas de profundidad
a <- lapply(data_sep[[1]],function(l){
  out <- l |>
    slice(1:3) #selleciona las filas por indice o por nombre
  out$z <- c(90,60,30)
  return(out)
})

# desanidar lo datos
r <- lapply(seq_along(a), function(i){
  out <- tryCatch(
    a[[i]] |>
      tidyr::unnest('data') |>
      tidyr::hoist(3,'value')|>
      dplyr::select(2,3,5) ,
  error = function(e) NULL)
  
  if (!is.null(out)) out$serial <- serial_est[i]
  if (is.list(out)) out <- out |> unnest('value')
  return(out)
})

# cuales son nulos

data_final<- do.call(rbind,r)

data_final<-  rename(data_final, Fecha_Hora = timestamp, valores = value, profundidad =  z)
data_final
write_rds(data_final, 'C:/Rmdtos/data_proceseed/data_procesada')
### El set de datos obtenido en el punto anterior lo debe ordenar de mayor a menor utilizando la función dplyr::arrange.
#ahora 

mam <- dplyr::arrange(data_final,desc(valores))

### Utilice la función tidyr::separate para crear una columna de fecha y otra de hora, a partir de la columnas fecha_hora.


h <- c('Fecha', 'Hora')
fecha_hora <- separate(data_final,'Fecha_Hora',h,sep=10)

#Haga un análisis de los valores faltantes (NAs) explicitos e implicitos. Utilice el paquete {tidyr}. Lo importante es saber si se encuentran valores para todas las fechas y en caso que no saber cuantos datos faltan.



















