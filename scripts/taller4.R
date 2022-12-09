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



################## Crear una funcion para eliminar valores anomalos 07 taller 4 #################


#1.1 Eliminar valores anómalos
#1.1.1
set.seed(123)
x <- c(10,-10,rnorm(100))
hist(x)

lim_inf <- quantile(x,.25) - 1.5*IQR(x)
lim_sup <- quantile(x,.75) + 1.5*IQR(x)

IQR(x)#rango interquartil
quantile(x)


elimi_anomal <- function(x){
  lim_inf <- quantile(x,.25) - 1.5*IQR(x)
  lim_sup <- quantile(x,.75) + 1.5*IQR(x)
  x[x < lim_inf | x > lim_sup] <- NA
  return(x)
}

elimi_anomal(x)

#1.1.2
vector1 <- c(rnorm(100, mean = 0, sd = 1))
elimi_anomal(vector1)

#1.1.3
iris |>   
  mutate(across(is.numeric, elimi_anomal,na.rm = TRUE))

#1.1.4

#1.1.5
#yearapi_agromet <- agroAPI |> 
 # mutate(across(is.numeric, elimi_anomal,na.rm = TRUE))

###############################################################################
#1.2 Funcion con multiples columnas como argumentos
#1.2.1
rqmean <- mean(sqrt(iris$Sepal.Length))
iris |> 
  mutate(across(is.numeric))/rqmean

#1.2.2
iris |> 
  group_by(Species) |>
  mutate(across(where(is.numeric),.fns = \(x) x/rqmean))

#1.2.3
rqmeanfun <- function(x,y){group_by(y) |>
    mutate(across(where(is.numeric))) |> 
    mutate(rqmean_ = x/rqmean)}

rqmeanfun(iris$Sepal.Width, iris[iris$Species,])

###############################################################################
#1.3 Resumir por rangos tipo categórica
#1.3.1
x <- 1:100
clasifica <- function(x,...){
  cut(x,3, labels=c('Low', 'Med', 'High'))}

#1.3.2
clasifica(vector1)

#1.3.3
iris |>
  mutate(across(where(is.numeric),.fns=clasifica))  

#1.3.4
iris |> 
  group_by(Species) |>
  mutate(across(where(is.numeric),.fns = \(x) cut(x, 3, labels = c('Bajo', 'Medio', 'Alto'))))

#1.3.5
meta_agro |>
  group_by(mes = floor_date(fecha_de_alta, '1 month')) |>
  mutate(across(6:8,.fns=clasifica)) 

###############################################################################
#1.4 Coeficiente de Variación
#1.4



cvar <- function(x){
  if(is.numeric(x)){
    x <- na.omit(x)
    cv<- sd(x)/mean(x)
    out <- cv < 0.3
  } else out <- FALSE
  return(out)
}
x <- runif(10)
y <- rnorm(200)
apply(iris[,1:4],2,FUN =cvar)
apply(select(iris, where(is.numeric)),2,FUN=cvar)
cvar(x)
cvar(y)
any(c(TRUE,FALSE,NA),na.rm = TRUE)
iris |>
  select(where(cvar))
#para datos agromet

#meta_agro |> mutate(across(everything(),elimi_anomal, na.rm = TRUE)) |>
#  drop_na() |>
#  select(where(cvar))


#### 2 EJERCICIOS DE TRANSFORMACIÓN Y VISUALIZACIÓN


#2.1. Comparar Distribuciones
#2.1.1
cols <- sapply(api_agv,ncol)
vars <- sapply(api_agv,nrow)
vars_un<- unique(vars)
ind7 <- vars %in% 7
serial_est<- agv_meta$serial[ind7]
data_sep <- lapply(vars_un,function(x){
  api_agv[x == vars]
})
a <- lapply(data_sep[[1]],function(l){
  out <- l |>
    slice(1:3) #seleciona filas por indice o nombre
  out$z <- c(90,60,30)
  return(out)
})


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


agv_final<- do.call(rbind,r)
names <- c('timestamp', 'Valor', 'Profundidad', 'Serial')
names -> colnames(agv_final)

agv_final <- tibble(agv_final)



agv_graf <- agv_final |> 
  mutate(fecha_hora = ymd_hm(timestamp)) |> 
  select(everything()) |> 
  group_by(dia = as_date(fecha_hora), Serial, Profundidad) |> 
  summarise(n = n(),
            NAs = sum(is.na(Valor)),
            NAs_prop = NAs/n)

ggplot(agv_graf, aes(dia, Serial, fill = NAs)) +
  geom_tile() +
  facet_grid(.~Profundidad) +
  scale_x_date(limits = c(ymd(20220831), ymd(20220905)), expand = c(0,0))


#2.3.1 
regionA <- agroAPI |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(temp_prom = mean(temp_promedio_aire, na.rm = TRUE)) |> 
  left_join(meta_agro, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))

regionA |> 
  ggplot(aes(region, temp_prom)) +
  geom_boxplot() +
  coord_flip()
#2.3.2
region <- agroAPI |> 
  group_by(station_id, dia = as_date(fecha_hora)) |> 
  summarise(temp_prom = mean(temp_promedio_aire, na.rm = TRUE)) |> 
  mutate(mes = floor_date(dia, '1 month')) |> 
  left_join(meta_agro, by = c('station_id' = 'ema')) |> 
  arrange(desc(latitud))

region <- region[,-2] |> 
  group_by(mes)

region |> 
  ggplot(aes(region, temp_prom, dia)) +
  geom_point()

# 2.3.3
#radsolar <- api_agromet[c('station_id', 'fecha_hora', "radiacion_solar_max")]

#radsolar <- api_agromet |> 
 # group_by(station_id, dia = as_date(fecha_hora)) |> 
  #summarise(rad_solar = mean(radiacion_solar_max, na.rm = TRUE)) |> 
  #mutate(mes = floor_date(dia, '1 month')) |> 
  #left_join(agromet_meta, by = c('station_id' = 'ema')) |> 
 # arrange(desc(latitud))



#radsolar <- radsolar |> 
  #group_by(station_id, dia = as_date(fecha_hora)) |> 
  #summarise(rad_solar = mean(radiacion_solar_max), na.rm = TRUE) |> 
  #mutate(mes = floor_date(dia, '1 month')) |> 
 # left_join(agromet_meta, by = c('station_id' = 'ema'))

#radsolar |> 
  #ggplot(aes(radsolar, rad_solar))+
 # geom_point()
#ggplot(agroAPI, aes())+
# geom_bo
