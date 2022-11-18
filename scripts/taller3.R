library(readr)
library(tidyr)
library(dplyr)

meta_agromet <- readRDS("C:/Rmdtos/data_raw/metadata_estaciones_agrometAPI.rds")
meta_agv <- readRDS("C:/Rmdtos/data_raw/metadata_estaciones_agvAPI(1).rds")
station_agromet <- readRDS("C:/Rmdtos/data_raw/station_id_agromet_Pablo.rds")
station_agv <- readRDS("C:/Rmdtos/data_raw/station_id_agv_Pablo.rds")
data_estaciones_agvAPI <- readRDS("C:/Rmdtos/data_raw/data_agvAPI.rds")
data_estaciones_agrometAPI <- readRDS("C:/Rmdtos/data_raw/data_estaciones_agrometAPI.rds")


met_agromet <- meta_agromet[meta_agromet$ema %in% station_agromet,]
meta_agv <- meta_agv[meta_agv$serial %in% station_agv,]


data_agro <- data_estaciones_agrometAPI[data_estaciones_agrometAPI$station_id %in% station_agromet,]

#1.con los datos de agromet cree datos anidados por estación (station_id) considerando todas las variables climáticas.

##data_estaciones_agrometAPI
data_nest <- data_agro |>
  nest(data=2:13)
data_nest |> glimpse()
##guarda todos los datos en data y solo deja por fuera la station id que es po rla que agrupa

##otra opcion
data_nest2 <- data_agro |>
  group_by (station_id)|>
  nest()

data_nest2 |> glimpse()

#2.de la data anidada extraiga el primer valor de la variable humedad relativa de la estación que se encuetra en la posición 5.


  
data_nest$data[5][[1]][1,4] |> 
  print()
  purrr::pluck(data_nest$data, 5,4,1)

#3.agregue la variable precipitación extraida de la columna anidada como una variable adicional


data_nest |> 
  hoist(data, 'precipitacion_horaria') |>
  unnest(precipitacion_horaria) |> #4
  select(-data)#elimina la columna data pero puede ser seleccionando todo menos esa (1:2)
##entra a ala columna data


#5.Haga explicitos los valores NA implicitos de precipitación. Comparé la cantidad de observaciones con la data original.

data_agro |>
  select(station_id, fecha_hora, precipitacion_horaria) |>
  complete(fecha_hora, station_id) |>
  glimpse() 


#6.relicé el rellenado de los valores NA de precipitación horaria tomando el valor anterior.


data_agro |>
  fill(precipitacion_horaria, .direction = "down") 
##2
#2.Filtrar los datos para los meses de Mayo a Julio en las estaciones asignadas.

library(lubridate)
data_agro |>
  mutate(fecha_hora  = as_date(data_agro$fecha_hora))
  filter(data_agro,fecha_hora  >= ymd(20210501) & fecha_hora < ymd(20210801))
#3.Tome una muestra de 1000 filas de forma aleatoria sin reemplazo.
slice_sample(data_agro, n=1000)
#4.Para cada estación seleccione los valores máximos de precipitación horaria.
data_agro |>
  group_by(station_id) |>
  arrange(desc(precipitacion_horaria)) |>
  slice(n=1) |>
  select(station_id,fecha_hora,precipitacion_horaria)
#5.Seleccione las columnas que tienen temperatura, además de station_id y fecha_hora.

data_agro |>
select(contains('temp'),station_id, fecha_hora)


#6.Seleccione las columnas que tienen valores no numéricos.
data_agro|>
select(where(\(x) !is.numeric(x))) |> 
  glimpse()
##3
#1.Agrupe los datos de las climáticos de agromet por estación (group_by)
data_agro |>
  group_by(station_id)
#2.Haga un sumarizado de promedio mensual de las variables de temperatura para cada estación (group_by, summarize y across).
data_agro_promm <-
  data_agro |> 
  group_by(station_id) |>
  summarise(across(contains("temp"),\(x) mean( x, na.rm = T), .names = "{.col}_avg"))

data_agro_promm
#3.Renombre y reordene las variables como temp_prom, temp_max y Temp_min (rename_with y `relocate``)
data_agro_prom_names <- data_agro_promm |> 
  rename_with(contains("temp"),.fn = \(x) substr(x, start =1,stop =8))

data_agro_prom_names

data_agro_prom_names |>
  relocate(temp_max, .after = temp_pro) |>
  relocate(temp_min, .after = last_col())
#4Cree las columnas var_temp y temp, en formato largo que contenga las variables de temp_prom, temp_max y Temp_min y sus valores. (pivot_longer)
#4
data_pivot_longer <- data_agro_prom_names|>
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) 

    

#5.Ordene los datos anteriores de mayor a menor.
data_pivot_longer <- data_agro_prom_names|>
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) |>
  arrange(desc(temp))


#6.Vuelva a formato ancho los datos del punto anterior.
data_pivot_wider <- data_pivot_longer |>
  pivot_wider(names_from = var_temp, values_from = temp)  
#7.Caclule cuantos datos no faltantes (!is.na) tiene cada estacion para cada una de las variables.
data_agro |>
  select(station_id, contains('temp')) |>
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) |>
  group_by(station_id, var_temp) |>
  summarize(nas = sum(is.na(temp)))|>
  glimpse()


##4. Ejercicios {dplyr} {parte 3}
#1.Con los datos de agromet, calcule el promedio de temperatura (media, máxima, mínima) por día y estación. Utilice summarize y across.
data_agro_prom_4 <-
  data_agro |>
  group_by(fecha_hora, station_id) |>
  summarise(across(contains("temp"),\(x) mean( x, na.rm = T), .names = "{.col}_avg"))
data_agro_prom_4
    

