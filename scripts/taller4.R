library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


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

data_final<- do.call(rbind,r)

data_final<-  rename(data_final, Fecha_Hora = timestamp, valores = value, profundidad =  z)
data_final




data_graf <- meta_agro |> 
  mutate(dia = as_date( timestamp)) |> 
  group_by(station_id, dia = as_date(Fecha_Hora)) |> 
  summarise(across(temp_promedio_aire:temp_maxima,
                   .fns = \(x) sum(is.na(x)),
                   .names = '{.col}_nas'))

data_graf_longer <- data_graf |> 
  pivot_longer(temp_promedio_aire_nas:temp_maxima_nas)

ggplot(data_graf_longer, aes(dia, as.factor(station_id), fill = value))+
  geom_tile() +
  facet_grid(.~name)


  agv_graf <- data_final |> 
    mutate(fecha_hora = ymd_hm(Fecha_Hora)) |> 
    select(everything()) |> 
    group_by(dia = as_date(fecha_hora), serial, profundidad) |> 
    summarise(n = n(),
              NAs = sum(is.na(valores)),
              NAs_prop = NAs/n)
  
  ggplot(agv_graf, aes(dia, serial, fill = NAs)) +
    geom_tile() +
    facet_grid(.~profundidad) +
    scale_x_date(limits = c(ymd(20220831), ymd(20220905)), expand = c(0,0))





################## Crear una funcion para eliminar valores anomalos 07 taller 4 #################
  set.seed(123)
  x <- c(10,-10,rnorm(100))
  hist(x)
  
  lim_inf <- quantile(x, .25) - 1.5*IQR(x)
  lim_sup <- quantile(x, .75) + 1.5*IQR(x)
  x[x< lim_inf] <- NA
  x[x> lim_sup] <- NA
  x
  x > lim_inf
  x < lim_sup
  
  #IQR(x)
  #quantile(x, .25)
  #quantile(x, .75)
#ahora se cra la funcion
  
elim_anomal <- function(x,...){
  if (!is.numeric(x)) stop('suministre el valor numerico')
 lim_inf <-  quantile(x, .25,...) - 1.5*IQR(x,...)
 lim_sup <- quantile(x, .75,...) + 1.5*IQR(x,...)
 x[x> lim_inf| x < lim_sup] <- NA
 return(x)
  
  
}

elim_anomal(x,na.rm = TRUE)

elim_anomal <- function(x,na.rm){
  if (!is.numeric(x)) stop('suministre el valor numerico')
  lim_inf <-  quantile(x, .25,na.rm) - 1.5*IQR(x,na.rm)
  lim_sup <- quantile(x, .75,na.rm) + 1.5*IQR(x,na.rm)
  return(c(lim_inf,lim_sup))
  
  
}

elim_anomal(x,na.rm = TRUE)


library(dplyr)
iris|>
  mutate(across(is.numeric,\(x) elim_anomal(x))) 
#  glimpse()|>
 # summary()


















