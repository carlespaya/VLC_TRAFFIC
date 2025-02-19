
library(shinyjs)
library(shiny)

Sys.setenv(TZ="Europe/Paris")

# Server
server <- function(input, output, session) {
  # Descargamos los datos por primera vez
  datos_server <<- descargar_datos()
  datos_server_emt <<- descargar_datos_emt()
  datos_server_vbs <<- descargar_datos_vbs()
  datos_server_metro <<- descargar_datos_metro()
  datos_horarios <<- descargar_datos_tiempo()
  datos_cam_traf <<- descargar_datos_cam_trafico()
  datos_server_dgt <<- descargar_datos_dgt()
  observe({
    withProgress(message = "Descargando datos...", value = 0, {
      for (i in 1:6) {
        incProgress(1/6)
        Sys.sleep(1)
      }
      invalidateLater(600000) 
      # resultados_df <- rbind(resultados_df, datos_server)
      datos_server <<- descargar_datos()
      datos_server_emt <<- descargar_datos_emt()
      datos_server_vbs <<- descargar_datos_vbs()
      datos_server_metro <<- descargar_datos_metro()
      datos_horarios <<- descargar_datos_tiempo()
      datos_server_dgt <<- descargar_datos_dgt()
      #datos_cam_traf <<- descargar_datos_cam_trafico()
    })
  })
  
  
  # Mapa
  output$map <- renderLeaflet({
    
    mpvlc <- leaflet()
    mpvlc <- addTiles(mpvlc) %>%
      setView(lng = input$long, lat = input$lat, zoom = 16) %>%
      addMarkers(lng = input$long, lat = input$lat,
                 icon = localitation_icon,
                 popup = paste("<b>Ubicación actual</b><br>Latitud: ",
                               round(input$lat, 6),
                               "<br>Longitud: ",
                               round(input$long, 6), sep = "")) %>%
      addCircles(lng = input$long,
                 lat = input$lat,
                 radius = 200,
                 color = "#03F",
                 weight = 3)
    # ------------------------------------------------------------------------
    distances_emt <- apply(datos_server_emt[, c("latitud", "longitud")], 1,
                           function(x) distGeo(c(x["longitud"], x["latitud"]),
                                               c(input$long, input$lat)))
    
    datos_server_emt$distance <- distances_emt
    
    closest_stops_emt <- datos_server_emt %>%
      arrange(distance) %>%
      head(5)
    
    names(closest_stops_emt)[1] <- "Parada"
    names(datos_server_emt)[1] <- "Parada"
    names(closest_stops_emt)[5] <- "Denominacion"
    names(datos_server_emt)[5] <- "Denominacion"
    names(closest_stops_emt)[6] <- "Lineas"
    names(datos_server_emt)[6] <- "Lineas"
    
    distances_vbs <- apply(datos_server_vbs[, c("latitud", "longitud")], 1,
                           function(x) distGeo(c(x["longitud"], x["latitud"]),
                                               c(input$long, input$lat)))
    datos_server_vbs$distance <- distances_vbs
    
    closest_stops_vbs <- datos_server_vbs %>%
      arrange(distance) %>%
      head(5)
    
    
    distances_metro <- apply(datos_server_metro[, c("stop_lon", "stop_lat")], 1,
                             function(x) distGeo(c(x["stop_lon"], x["stop_lat"]),
                                                 c(input$long, input$lat)))
    datos_server_metro$distance <- distances_metro
    
    closest_stops_metro <- datos_server_metro %>%
      arrange(distance) %>%
      head(5)
    # ------------------------------------------------------------------------
    datos_server_vbs_filtered <- datos_server_vbs %>%
      filter(!Direccion %in% closest_stops_vbs$Direccion)
    
    # datos_server_emt_filtered <- datos_server_emt %>%
    #   filter(!Parada %in% closest_stops_emt$Parada)
    datos_server_emt_filtered <- datos_server_emt %>%
      filter(!Parada %in% closest_stops_emt$Parada)
    
    datos_server_metro_filtered <- datos_server_metro %>%
      filter(!datos_server_metro$stop_name %in% closest_stops_metro$stop_name)
    # ------------------------------------------------------------------------
    paleta_colores <- colorFactor(
      palette = c( "chartreuse", "darkorange", "firebrick1","gray0" ,"floralwhite",
                   "chartreuse4","darkorange3" ,"brown1", "gray0","floralwhite"),
      domain = c(0:9))
    
    # colores <- c("red", "blue", "green")
    punto_inicio <- c(input$long, input$lat)  # Longitud, Latitud
    # Crear una lista con los tres puntos finales
    puntos_finales <- list(c(closest_stops_emt$longitud[1], closest_stops_emt$latitud[1]),
                           c(closest_stops_vbs$longitud[1], closest_stops_vbs$latitud[1]),
                           c(closest_stops_metro$stop_lon[1], closest_stops_metro$stop_lat[1]))
    
    mpvlc <- addPolylines(mpvlc, data = datos_server,
                          color = ~paleta_colores(estado),
                          weight = 4,
                          opacity = 1,
                          label = datos_server$estados_completos,
                          popup = ~paste("<strong>Calle: </strong> ",
                                         denominacio, "<br>",
                                         "<strong>Estado: </strong> ",
                                         estado, "<br>", 
                                         "<strong>Significado: </strong>",
                                         Estado_denominacios, "<br>", 
                                         "<strong>Hora: </strong>",
                                         hora_descarga
                          )
    )
    grupos <- c("EMT", "VBS","METRO")
    # Calcular y trazar las tres rutas en el mapa
    for (i in 1:3) {
      # Calcular la ruta más corta entre los dos puntos
      ruta <- osrm::osrmRoute(src = c(input$long, input$lat),
                              dst = puntos_finales[[i]],
                              osrm.profile = "foot",
                              returnclass = "sf")
      
      # Añadir la ruta al mapa
      mpvlc <- mpvlc %>%
        addPolylines(data = ruta, weight = 4,opacity = 1,
                     group = grupos[i],
                     color = rainbow(3)[i], dashArray = c(10, 5),
                     popup = paste("Distancia:", round(ruta$duration), "mins"))
    }
    # ------------------------------------------------------------------------  
    
    mpvlc <- mpvlc %>% addLegend(pal = paleta_colores,
                                 values = datos_server$estado,
                                 title = "Estado del tráfico",
                                 position = "bottomright")
    
    # ------------------------------------------------------------------------
    mpvlc <- mpvlc %>% 
      addMarkers(data = datos_server_emt_filtered,
                 group = "EMT",
                 lng = ~longitud, 
                 lat = ~latitud,
                 icon = bus_icon,
                 popup = paste("<strong>Denominación:</strong> ",
                               datos_server_emt$Denominacion,
                               "<br>",
                               "<strong>Líneas:</strong> ",
                               datos_server_emt$Lineas,
                               "<br>",
                               "<strong>Próximos: </strong> ",
                               datos_server_emt$Proximas_llegadas),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_bus)
                 
      ) 
    mpvlc <- mpvlc %>% 
      addMarkers(data = closest_stops_emt,
                 group = "EMT",
                 lng = ~longitud, 
                 lat = ~latitud,
                 icon = bus_icon_cercano,
                 popup = paste("<strong>Denominación:</strong> ",
                               closest_stops_emt$Denominacion,
                               "<br>",
                               "<strong>Líneas:</strong> ",
                               closest_stops_emt$Lineas,
                               "<br>",
                               "<strong>Próximos: </strong> ",
                               closest_stops_emt$Proximas_llegadas),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100, 
                                                       minimumClusterSize = 5,
                                                       iconCreateFunction = marker_js_bus)
                 
      )
    # ------------------------------------------------------------------------
    mpvlc <- mpvlc %>%
      addMarkers(data = datos_server_vbs_filtered,
                 group = "VBS",
                 lng = ~longitud, 
                 lat = ~latitud,
                 icon = vbs_icon,
                 popup = paste("<strong>Calle: </strong> ",
                               datos_server_vbs_filtered$Direccion,
                               "<br>",
                               "<strong>Total: </strong> ",
                               datos_server_vbs_filtered$Espacios_totales,
                               "<br>",
                               "<strong>Espacios Disponibles: </strong> ",
                               datos_server_vbs_filtered$Espacios_libres,
                               "<br>",
                               "<strong>>Bicis disponibles: </strong> ",
                               datos_server_vbs_filtered$Bicis_disponibles,
                               "<br>",
                               "<strong>Hora Actualización: </strong> ",
                               datos_server_vbs_filtered$fecha_actualizacion),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100, 
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_vbs)
                 
      )
    mpvlc <- mpvlc %>%
      addMarkers(data = closest_stops_vbs,
                 group = "VBS",
                 lng = ~longitud, 
                 lat = ~latitud,
                 icon = vbs_icon_cercano,
                 popup = paste("<strong>Calle: </strong> ",
                               closest_stops_vbs$Direccion,
                               "<br>",
                               "<strong>Total: </strong> ",
                               closest_stops_vbs$Espacios_totales,
                               "<br>",
                               "<strong> Espacios Disponibles: </strong> ",
                               closest_stops_vbs$Espacios_libres,
                               "<br>",
                               "<strong>Bicis disponibles: </strong> ",
                               closest_stops_vbs$Bicis_disponibles,
                               "<br>",
                               "<strong>Hora Actualización: </strong> ",
                               closest_stops_vbs$fecha_actualizacion),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 5,
                                                       iconCreateFunction = marker_js_vbs)
                 
      )
    # ------------------------------------------------------------------------
    mpvlc <- mpvlc %>%
      addMarkers(data = datos_server_metro_filtered,
                 group = "METRO",
                 lng = ~stop_lon,
                 lat = ~stop_lat,
                 icon = metro_icon,
                 popup = paste("<strong>Nombre parada: </strong> ",
                               datos_server_metro_filtered$stop_name,
                               "<br>",
                               "<strong>Destinos: </strong> ",
                               datos_server_metro_filtered$destinos,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_metro)
                 
      )
    mpvlc <- mpvlc %>%
      addMarkers(data = closest_stops_metro,
                 group = "METRO",
                 lng = ~stop_lon,
                 lat = ~stop_lat,
                 icon = metro_icon_cercano,
                 popup = paste("<strong>Nombre parada: </strong> ",
                               closest_stops_metro$stop_name,
                               "<br>",
                               "<strong>Línieas: </strong> ",
                               closest_stops_metro$destinos,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 5,
                                                       iconCreateFunction = marker_js_metro)
                 
      )
    
    mpvlc <- mpvlc %>%
      addMarkers(data = datos_cam_traf,
                 group = "CAM",
                 lng = ~longitud,
                 lat = ~latitud,
                 icon = camera_icon,
                 popup = paste("<strong>Id Cam: </strong> ",
                               datos_cam_traf$`Id. Càmera / Id. Cámara`,
                               "<br>",
                               "<strong>URL: </strong> ",
                               datos_cam_traf$URL,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 5,
                                                       iconCreateFunction = marker_js_cams))
    
    # -------------------------- DGT ------------------------------------------
    obras <- datos_server_dgt %>% filter(tipo == "Incidencia" & suceso == "OBRAS")
    accidentes <- datos_server_dgt %>% filter(tipo == "Incidencia" & suceso == "OTROS")
    retenciones <- datos_server_dgt %>% filter(tipo == "Incidencia" & suceso == "RETENCIÓN / CONGESTIÓN")
    radares <- datos_server_dgt %>% filter(tipo == "Radar")
    
    mpvlc <- mpvlc %>%
      addMarkers(data = radares,
                 group = "DGT",
                 lng = ~lng,
                 lat = ~lat,
                 icon = radar_icon,
                 popup = paste("<strong> RADAR </strong> ",
                               "<br>",
                               "<strong>Carretera: </strong> ",
                               radares$carretera,
                               "<br>",
                               "<strong>Sentido: </strong> ",
                               radares$sentido,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_dgt)
      )
    
    mpvlc <- mpvlc %>%
      addMarkers(data = accidentes,
                 group = "DGT",
                 lng = ~lng,
                 lat = ~lat,
                 icon = accidente_icon,
                 popup = paste("<strong> ACCIDENTE </strong> ",
                               "<br>",
                               "<strong>Carretera: </strong> ",
                               accidentes$carretera,
                               "<br>",
                               "<strong>Hora: </strong> ",
                               accidentes$hora,
                               "<br>",
                               "<strong>Fecha Inicio: </strong> ",
                               accidentes$fecha,
                               "<br>",
                               "<strong>Fecha Fin: </strong> ",
                               accidentes$fechaFin,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_dgt)
      )
    
    mpvlc <- mpvlc %>%
      addMarkers(data = obras,
                 group = "DGT",
                 lng = ~lng,
                 lat = ~lat,
                 icon = obras_icon,
                 popup = paste("<strong> OBRAS </strong> ",
                               "<br>",
                               "<strong>Carretera: </strong> ",
                               obras$carretera,
                               "<br>",
                               "<strong>Hora: </strong> ",
                               obras$hora,
                               "<br>",
                               "<strong>Fecha Inicio: </strong> ",
                               obras$fecha,
                               "<br>",
                               "<strong>Fecha Fin: </strong> ",
                               obras$fechaFin,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_dgt))
    
    mpvlc <- mpvlc %>%
      addMarkers(data = retenciones,
                 group = "DGT",
                 lng = ~lng,
                 lat = ~lat,
                 icon = retenciones_icon,
                 popup = paste("<strong> RETENCIÓN </strong> ",
                               "<br>",
                               "<strong>Carretera: </strong> ",
                               retenciones$carretera,
                               "<br>",
                               "<strong>Hora: </strong> ",
                               retenciones$hora,
                               "<br>",
                               "<strong>Fecha Inicio: </strong> ",
                               retenciones$fecha,
                               "<br>",
                               "<strong>Fecha Fin: </strong> ",
                               retenciones$fechaFin,
                               "<br>"),
                 clusterOptions = markerClusterOptions(maxClusterRadius = 100,
                                                       minimumClusterSize = 10,
                                                       iconCreateFunction = marker_js_dgt))
    
    # ------------------------------------------------------------------------
    mpvlc <- mpvlc %>%
      addLayersControl(
        overlayGroups = c("EMT", "VBS","METRO", "CAM", "DGT"),
        position = "topright",
        options = layersControlOptions(collapsed = TRUE)
      ) %>% hideGroup(c("EMT", "VBS","METRO", "CAM", "DGT")) %>%
      addProviderTiles(providers, group = "Esri") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldImagery", "Esri.WorldStreetMap"),
        position = "bottomright",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var myMap = this;
        myMap.on('baselayerchange',
          function (e) {
            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
          })
      }")
    
    
  })
  
  datos_filtrados_reactivo <- reactive({
    denominacio_elegida <- input$denominacio
    datos_filtrados <- resultados_df[resultados_df$denominacio == denominacio_elegida, ]
    return(datos_filtrados)
  })
  observe({
    updateSelectInput(session, "denominacio", 
                      choices = sort(unique(resultados_df$denominacio)))
  })
  
  
  output$plot <- renderPlot({
    datos_filtrados <- datos_filtrados_reactivo() %>%
      arrange(hora_descarga)  # Ordena los datos por la variable "hora_descarga"
    
    ggplot(datos_filtrados, aes(x = hora_descarga, y = as.numeric(estado) - 1, color = estado)) +
      geom_point(size = 6) +
      geom_line(aes(group=1)) +
      scale_color_manual(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                         values = c("chartreuse", "darkorange", "firebrick1", 
                                    "gray0", "floralwhite", 
                                    "chartreuse4", "darkorange3",
                                    "brown1", "gray0", "floralwhite")) +
      scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 1)) +
      geom_text(aes(label = estado), vjust = -1) +
      xlab("Hora") + ylab("Estado del tráfico") +
      labs(color = "Estado del Tráfico") + 
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = NA),
            panel.border = element_rect(linetype = "solid", fill = NA),
            plot.title = element_text(size = 16, face = "bold")) +
      ggtitle("Evolución del tráfico durante el día")
  })
  
  # Creamos el mapa con la ubicación actual del usuario
  output$rutas <- renderLeaflet({
    mapa_rutas <- leaflet()
    mapa_rutas <- addTiles(mapa_rutas) %>%
      setView(lng = input$long, lat = input$lat, zoom = 16) %>%
      addMarkers(lng = input$long, lat = input$lat,
                 icon = localitation_icon,
                 popup = paste("<b>Ubicación actual</b><br>Latitud: ",
                               round(input$lat, 6),
                               "<br>Longitud: ",
                               round(input$long, 6), sep = ""))
    mapa_rutas <- mapa_rutas %>%
      addSearchOSM(options = searchOptions(
        position = "topleft"
      ))
  })
  
  # Evento que se dispara cuando el usuario pulsa el botón de buscar ruta
  observeEvent(input$buscar_ruta, {
    # Obtenemos la dirección introducida por el usuario
    destino <- input$destino
    
    # Geolocalizamos la dirección introducida por el usuario
    destino_coords <- geocode(destino)
    
    # Calculamos la ruta más corta entre la ubicación actual del usuario y el destino introducido
    ruta <- osrmRoute(src = c(input$long, input$lat),
                      dst = c(destino_coords$lon, destino_coords$lat),
                      osrm.profile = input$transporte,
                      returnclass = "sf")
    
    # Dibujamos la ruta en el mapa
    leafletProxy("rutas", data = ruta) %>%
      clearShapes() %>%
      addPolylines(weight = 4, color = "blue",
                   popup = paste("Distancia:", round(ruta$distance), "kms<br>",
                                 "Tiempo:", round(ruta$duration), "mins"))%>%
      addMarkers(lng = destino_coords$lon, lat = destino_coords$lat,
                 icon = destino_icon,
                 popup = paste("<b>Su destino</b><br>Latitud: ",
                               round(destino_coords$lat, 6),
                               "<br>Longitud: ",
                               round(destino_coords$lon, 6), sep = ""))
  })
  output$vb1 <- renderValueBox({
    shinydashboard::valueBox(
      paste0("Temperatura: ", datos_horarios$Temperatura, " °C", sep = ""),
      subtitle = "",
      color = ifelse(datos_horarios$Temperatura>20, 'red', 'green' )
    )
  })
  
  output$vb2 <- renderValueBox({
    shinydashboard::valueBox(
      paste0("Humedad: ", datos_horarios$Humedad, " %", sep = ""),
      subtitle = "",
      color = ifelse(datos_horarios$Humedad>40, 'red', 'green')
    )
  })
  output$vb3 <- renderValueBox({
    shinydashboard::valueBox(
      paste0("Precipitación: ", datos_horarios$Prec, " mm", sep = ""),
      subtitle = "",
      color = "yellow"
    )
  })
}
