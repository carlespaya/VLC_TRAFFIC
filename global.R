library(leaflet)
library(lubridate)
library(dplyr)
library(XML)
library(curl)
library(sf)
library(crs)
library(rgdal)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rvest)
library(stringr)
library(readr)
library(geosphere)
library(osrm)
library(osmdata)
library(leaflet.extras)
library(ggmap)
library(tidyr)
library(purrr)
register_google(key="AIzaSyB1rg4nWUhPWM-_k1Sdj0VFpEo_Zb0Dp5I")

Sys.setenv(TZ="Europe/Paris")

# Crear un dataframe vacío para almacenar los resultados
resultados_df <- data.frame()

# Guardar la fecha de la última descarga
fecha_anterior <- format(Sys.time(), "%Y-%m-%d")

# ---------------------------- Iconos para mapa -----------------------------
bus_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/128/4002/4002863.png",
                     iconWidth = 40, iconHeight = 40)

localitation_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/9924/9924455.png",
                              iconWidth = 40, iconHeight = 40)

vbs_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/3005/3005583.png",
                     iconWidth = 40, iconHeight = 40)

bus_icon_cercano <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/829/829276.png",
                             iconWidth = 40, iconHeight = 40)

vbs_icon_cercano <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/1183/1183139.png",
                             iconWidth = 40, iconHeight = 40)

metro_icon <- makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Isotip_de_Metroval%C3%A8ncia.svg/512px-Isotip_de_Metroval%C3%A8ncia.svg.png",
                       iconWidth = 15, iconHeight = 15)

metro_icon_cercano <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/8059/8059120.png",
                               iconWidth = 50, iconHeight = 50)

destino_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/7357/7357704.png",
                         iconWidth = 50, iconHeight = 50)

camera_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/5301/5301448.png",
                        iconWidth = 25, iconHeight = 25)

radar_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/1789/1789189.png",
                       iconWidth = 25, iconHeight = 25)

accidente_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/2125/2125190.png",
                           iconWidth = 25, iconHeight = 25)

obras_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/6028/6028865.png",
                       iconWidth = 25, iconHeight = 25)

retenciones_icon <- makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/8737/8737587.png",
                             iconWidth = 25, iconHeight = 25)

# ---------------------------- Marcadores clusters-----------------------------
# marker_js_vbs <- JS("function(cluster) {
#                   var html = '<div style=\"background-color:rgba(135, 206, 250, 0.8)\"><span>' + cluster.getChildCount() + '</div><span>'
#                   return new L.DivIcon({html: html, className: 'marker-cluster'});
#                   }")
# 
# marker_js_bus <- JS("function(cluster) {
#                   var html = '<div style=\"background-color:rgba(255, 102, 102, 0.8)\"><span>' + cluster.getChildCount() + '</div><span>'
#                   return new L.DivIcon({html: html, className: 'marker-cluster'});
#                   }")
# marker_js_metro <- JS("function(cluster) {
#                   var html = '<div style=\"background-color:rgba(255, 255, 0, 0.8)\"><span>' + cluster.getChildCount() + '</div><span>'
#                   return new L.DivIcon({html: html, className: 'marker-cluster'});
#                   }")
# 
# marker_js_cams <- JS("function(cluster) {
#                   var html = '<div style=\"background-color:rgba(130, 130, 130, 0.8)\"><span>' + cluster.getChildCount() + '</div><span>'
#                   return new L.DivIcon({html: html, className: 'marker-cluster'});
#                   }")
# 
# marker_js_dgt <- JS("function(cluster) {
#                   var html = '<div style=\"background-color:rgba(69, 75, 27, 0.8)\"><span>' + cluster.getChildCount() + '</div><span>'
#                   return new L.DivIcon({html: html, className: 'marker-cluster'});
#                   }")
marker_js_vbs <- JS("function(cluster) {
  var count = cluster.getChildCount();
  var opacity = count / 10;  // Ajusta el valor para controlar la opacidad
  var backgroundColor = 'rgba(135, 206, 250, ' + opacity + ')';
  var borderColor = 'black';  // Cambia el color del borde aquí

  var html = '<div style=\"background-color:' + backgroundColor + '; border-color:' + borderColor + '\"><span>' + count + '</div><span>';
  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")

marker_js_bus <- JS("function(cluster) {
  var count = cluster.getChildCount();
  var opacity = count / 10;  // Ajusta el valor para controlar la opacidad
  var backgroundColor = 'rgba(255, 102, 102, ' + opacity + ')';
  var borderColor = 'black';  // Cambia el color del borde aquí

  var html = '<div style=\"background-color:' + backgroundColor + '; border-color:' + borderColor + '\"><span>' + count + '</div><span>';
  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")

marker_js_metro <- JS("function(cluster) {
  var count = cluster.getChildCount();
  var opacity = count / 10;  // Ajusta el valor para controlar la opacidad
  var backgroundColor = 'rgba(255, 255, 0, ' + opacity + ')';
  var borderColor = 'black';  // Cambia el color del borde aquí

  var html = '<div style=\"background-color:' + backgroundColor + '; border-color:' + borderColor + '\"><span>' + count + '</div><span>';
  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")

marker_js_cams <- JS("function(cluster) {
  var count = cluster.getChildCount();
  var opacity = count / 10;  // Ajusta el valor para controlar la opacidad
  var backgroundColor = 'rgba(130, 130, 130, ' + opacity + ')';
  var borderColor = 'black';  // Cambia el color del borde aquí

  var html = '<div style=\"background-color:' + backgroundColor + '; border-color:' + borderColor + '\"><span>' + count + '</div><span>';
  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")

marker_js_dgt <- JS("function(cluster) {
  var count = cluster.getChildCount();
  var opacity = count / 10;  // Ajusta el valor para controlar la opacidad
  var backgroundColor = 'rgba(69, 75, 27, ' + opacity + ')';
  var borderColor = 'black';  // Cambia el color del borde aquí

  var html = '<div style=\"background-color:' + backgroundColor + '; border-color:' + borderColor + '\"><span>' + count + '</div><span>';
  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")

# -------descargar_datos: funcion para descargar los datos de trafico en tiempo real-----
descargar_datos <- function() {
  
  url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/estat-transit-temps-real-estado-trafico-tiempo-real/exports/shp?lang=es&timezone=Europe%2FBerlin"
  temp_zip_file <- tempfile(fileext = ".zip")
  download.file(url, temp_zip_file, mode = "wb", quiet = TRUE)
  unzip(temp_zip_file, exdir = "./datos")
  datos <- st_read("./datos/estat-transit-temps-real-estado-trafico-tiempo-real.shp")
  datos <- st_transform(datos, 4326)
  datos$estado <- factor(datos$estado,
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  
  datos <- datos %>%
    mutate(Estado_denominacios = case_when(
      estado == 0 ~ "Fluido",
      estado == 1 ~ "Denso",
      estado == 2 ~ "Congestionado",
      estado == 3 ~ "Cortado",
      estado == 4 ~ "Sin datos",
      estado == 5 ~ "Paso inferior fluido",
      estado == 6 ~ "Paso inferior denso",
      estado == 7 ~ "Paso inferior congestionado",
      estado == 8 ~ "Paso inferior cortado",
      estado == 9 ~ "Sin datos (paso inferior)",
      TRUE ~ "Valor no identificado"
    )) 
  
  datos$hora_descarga <- Sys.time()
  datos$hora_descarga <- format(datos$hora_descarga, format="%H:%M:%S")
  datos$Estado_denominacios <- factor(datos$Estado_denominacios,
                                      levels = c("Fluido", "Denso", "Congestionado",
                                                 "Cortado", "Sin datos", "Paso inferior fluido",
                                                 "Paso inferior denso", "Paso inferior congestionado",
                                                 "Paso inferior cortado", "Sin datos (paso inferior)"))
  datos$estados_completos <- paste(datos$estado, datos$Estado_denominacios, sep = ", ")
  
  fecha_actual <- format(Sys.time(), "%Y-%m-%d")
  # Verificar si han pasado al menos tres minutos desde el último dato almacenado
  dif <- difftime(fecha_actual, fecha_anterior, units = "mins")
  if (dif >= 3) {
    # Actualizar la fecha almacenada en la variable global y el dataframe vacío
    fecha_anterior <<- fecha_actual
    resultados_df <<- resultados_df()
  }
  # Agregar los nuevos datos al dataframe
  else{
    resultados_df <<- rbind(resultados_df, datos)
  }
  return(datos)
}

# ----descargar_datos_emt----
# descargar_datos_emt: carga el fichero genereado en el fichero .py, hago esto
# para optimizar el tiempo de carga de la aplicacion, ya que el fichero .py
# se seguira ejecutando en segundo plano y actualizando los datos.

descargar_datos_emt <- function() {
  emt_data <- read_csv("emt_data_actual.csv")
  return(emt_data)
}

# ---------------------------- Descarga y extrae paradas mas proximas----------
descargar_datos_metro <- function() {
  # url <- "https://transitfeeds.com/p/ferrocarriles-de-la-generalidad-valenciana/1039/20220315/download"
  url <- "https://transitfeeds.com/p/ferrocarriles-de-la-generalidad-valenciana/1039/latest/download"
  temp_zip_file <- tempfile(fileext = ".zip")
  download.file(url, temp_zip_file, mode = "wb", quiet = TRUE)
  unzip(temp_zip_file, exdir = "./datos")
  stops <- read_csv("./datos/stops.txt")
  stop_times <- read_csv("./datos/stop_times.txt")
  trips <- read_csv("./datos/trips.txt")
  horarios_metro <- merge(stop_times, stops, by = "stop_id")
  horarios_metro <- merge(horarios_metro, trips, by = "trip_id")
  horarios_metro <- horarios_metro %>% select(c("stop_id",
                                                "stop_name","trip_headsign",
                                                "arrival_time","stop_lat",
                                                "stop_lon","zone_id"))
  hora_actual <- format(Sys.time(), format = "%H:%M:%S")
  
  
  horarios_metro_nuevo <- horarios_metro %>%
    mutate(arrival_datetime = as.POSIXct(paste(Sys.Date(), arrival_time),
                                         format = "%Y-%m-%d %H:%M:%S"))
  
  horarios_metro_nuevo <- horarios_metro_nuevo %>%
    mutate(arrival_time = as.POSIXct(arrival_datetime, format = "%Y-%m-%d %H:%M:%S"),
           diff_seconds = as.numeric(difftime(arrival_time, as.POSIXct(hora_actual, format = "%H:%M:%S"), units = "secs")))
  
  horarios_filtrados <- horarios_metro_nuevo %>%
    arrange(stop_name, trip_headsign, diff_seconds) %>%
    group_by(stop_name, trip_headsign) %>%
    filter(diff_seconds >= 0 & diff_seconds <= 600) %>%
    slice(1)
  
  posibles_destinos <- horarios_filtrados %>%
    mutate(arrival_time = format(arrival_time, format = "%H:%M:%S")) %>%
    group_by(stop_name, trip_headsign) %>%
    summarize(arrival_time = min(arrival_time)) %>%
    group_by(stop_name) %>%
    summarize(destinos = paste0("<i>", trip_headsign, "</i>", ": ", format(arrival_time, format = "%H:%M"), collapse = "<br>"))
  # summarize(destinos = paste(paste(trip_headsign, arrival_time, sep = ": "), collapse = ", "))
  
  geoloc_metro <- horarios_metro %>%
    distinct(stop_name, stop_lon, stop_lat) %>%
    group_by(stop_name) 
  
  data_metro_total <- merge(posibles_destinos,geoloc_metro, by="stop_name")
  return(data_metro_total)
}

# ---descargar_datos_vbs: descarga los datos de las estaciones de valenbisi ----
descargar_datos_vbs <- function() {
  url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/valenbisi-disponibilitat-valenbisi-dsiponibilidad/exports/csv?lang=es&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  dest_file <- "datos/vbs_data.csv"
  download.file(url, dest_file, method = "auto", quiet = TRUE, mode = "wb")
  vbs_data <- read_delim("./datos/vbs_data.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  vbs_data$latitud <- as.numeric(sapply(strsplit(vbs_data$geo_point_2d, ","), "[", 1))
  vbs_data$longitud <- as.numeric(sapply(strsplit(vbs_data$geo_point_2d, ","), "[", 2))
  vbs_data %>% st_as_sf(coords = c("latitud","longitud"), crs=4326)
  
  return(vbs_data)
}

# ------------Temperatura y humedad en tiempo real ----------------------------
descargar_datos_tiempo <- function() {
  url <- "https://www.aemet.es/es/eltiempo/observacion/ultimosdatos_8416Y_datos-horarios.csv?k=val&l=8416Y&datos=det&w=0&f=temperatura&x=h24"
  
  # Definimos la ruta y el nombre de archivo donde guardaremos el archivo CSV descargado
  ruta <- "datos/ultimosdatos_8416Y_datos-horarios.csv"
  
  # Descargamos el archivo CSV
  download.file(url, destfile = ruta, mode = "wb", quiet = TRUE)
  
  # Cambiamos el nombre del archivo descargado a "datos_horarios.csv"
  file.rename(ruta, "datos/datos_horarios.csv")
  
  datos_horarios <- read_table2("./datos/datos_horarios.csv", 
                                col_names = FALSE)
  datos_horarios <- slice(datos_horarios, 4)
  
  datos_horarios <- separate(datos_horarios, X2, into = c("Hora", "Temperatura", "V.Viento", "Direccion", "Racha", "Racha_Dir", "Prec", "Presion", "Tend", "Humedad"), sep = ",")
  
  datos_horarios <- apply(datos_horarios, 2, function(x) gsub("\"", "", x)) # Quitar las comillas
  datos_horarios <- data.frame(datos_horarios)
  datos_horarios <- t(datos_horarios)
  datos_horarios <- data.frame(datos_horarios)
  return(datos_horarios)
}

# ------ Camaras de trafico en valencia ----------------------------------------
descargar_datos_cam_trafico <- function() {
  url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/cameres-trafic-camaras-trafico/exports/csv?lang=es&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  dest_file <- "datos/cam_traf_data.csv"
  download.file(url, dest_file, method = "auto", quiet = TRUE, mode = "wb")
  cam_traf_data <- read_delim("./datos/cam_traf_data.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  cam_traf_data$latitud <- as.numeric(sapply(strsplit(cam_traf_data$geo_point_2d, ","), "[", 1))
  cam_traf_data$longitud <- as.numeric(sapply(strsplit(cam_traf_data$geo_point_2d, ","), "[", 2))
  cam_traf_data %>% st_as_sf(coords = c("latitud","longitud"), crs=4326)
  
  return(cam_traf_data)
}

# Descargamos datos DGT -------------------------------------
descargar_datos_dgt <- function() {
  # link to the API output as a JSON file
  url_json <- "https://infocar.dgt.es/etraffic/BuscarElementos?latNS=39.6&longNS=-0.2&latSW=39.4&longSW=-0.5&zoom=13&accion=getElementos&Camaras=true&SensoresTrafico=true&SensoresMeteorologico=true&Paneles=true&Radares=true&IncidenciasRETENCION=true&IncidenciasOBRAS=true&IncidenciasMETEOROLOGICA=true&IncidenciasPUERTOS=true&IncidenciasOTROS=true&IncidenciasEVENTOS=true&IncidenciasRESTRICCIONES=true&niveles=true&caracter=acontecimiento"
  
  # get the raw json into R
  raw_json <- httr::GET(url_json) %>% 
    httr::content()
  
  # convertir texto JSON en dataframe
  df <- jsonlite::fromJSON(raw_json)
  return(df)
}
