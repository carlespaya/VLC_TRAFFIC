library(shinyjs)
library(shiny)

Sys.setenv(TZ="Europe/Paris")

ui <- fluidPage(
  titlePanel("Tráfico en Valencia en Tiempo Real", 
             windowTitle = "Tráfico en Valencia"),
  tags$head(tags$style(HTML('* {font-family: "Georgia"};'))),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "clusters.css")
  ),
  tags$style(".small-box.bg-yellow { background-color:
                                     #d2efff !important; color: #000000 !important;
                                     box-shadow: 0px 3px 3px #888888 !important; }"),
  tags$style(".small-box.bg-green { background-color:
                                     #95c799 !important; color: #000000 !important;
                                     box-shadow: 0px 3px 3px #888888 !important;}"),
  tags$style(".small-box.bg-red { background-color:
                                     #f29883 !important; color: #000000 !important;
                                     box-shadow: 0px 3px 3px #888888 !important;}"),
  tags$style(".small-box h3 { margin-left: 10px; }"),
  tags$style(".small-box { margin: 8px 0; padding: 2px;}"),
  #tags$style(".valuebox { margin-bottom: 20px; }"),
  #tags$style(".value-box .description { font-size: 6px; }"),
  # shinythemes::themeSelector(),
  # UBICACION EN TIEMPO REAL DEL USUARIO
  tags$script('
              $(document).ready(function () {
              
                function getLocation(callback){
                var options = {
                  enableHighAccuracy: true,
                  timeout: 5000,
                  maximumAge: 0
                };
                
                navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                function onError (err) {
                  Shiny.onInputChange("geolocation", false);
                }
                
                function onSuccess (position) {
                  setTimeout(function () {
                    var coords = position.coords;
                    var timestamp = new Date();
                    
                    console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
                    Shiny.onInputChange("geolocation", true);
                    Shiny.onInputChange("lat", coords.latitude);
                    Shiny.onInputChange("long", coords.longitude);
                    Shiny.onInputChange("accuracy", coords.accuracy);
                    Shiny.onInputChange("time", timestamp)
                  
                    console.log(timestamp);
                
                    if (callback) {
                      callback();
                    }
                  }, 1100)
                }
              }
              
              var TIMEOUT = 1000; //SPECIFY
              var started = false;
              function getLocationRepeat(){
                //first time only - no delay needed
                if (!started) {
                  started = true;
                  getLocation(getLocationRepeat);
                  return;
                }
              
                setTimeout(function () {
                  getLocation(getLocationRepeat);
                }, TIMEOUT);
                
              };
                
              getLocationRepeat();
                
            });
            '), 
  
  # Pestañas
  tabsetPanel(
    tabPanel("Mapa",
             column(width = 12,
                    fluidRow(
                      column(width = 4, valueBoxOutput("vb1", width = 12)),
                      column(width = 4, valueBoxOutput("vb2", width = 12)),
                      column(width = 4, valueBoxOutput("vb3", width = 12))
                      
                    )
             ),
             column(width = 12,
                    div(style = "width:100%;height:100vh; margin-top: 20px;",
                        leafletOutput("map", width="100%", height="100%")))
    ),
    tabPanel("Gráficos", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("denominacio", "Seleccione la calle:",
                             choices = NULL)
               ),
               mainPanel(
                 plotOutput("plot")
               ))),
    tabPanel("Rutas",
             sidebarLayout(
               sidebarPanel(
                 textInput("destino", "Introduce la dirección de destino:", value = ""),
                 selectInput("transporte", "Selecciona el medio de transporte",
                             choices = c("A pie" = "foot", "Bicicleta" = "bike", "Coche" = "car"),
                             selected = "A pie"),
                 actionButton("buscar_ruta", "Buscar ruta")
               ),
               mainPanel(
                 leafletOutput("rutas")
               )
             )),
    tabPanel("Datos e Información",
             tags$div(
               class = "datos-informacion",
               tags$h2("Datos e Información"),
               tags$p("En esta pestaña se explicará el origen de los datos representados así como el significado de la leyenda."),
               tags$h2("Origen de los datos"),
               tags$p("Los datos utilizados son los siguientes: "),
               tags$p(
                 tags$strong("Tráfico Valencia: "),
                 tags$a(
                   "actualizado cada 3 minutos, procedente del catálogo de datos abiertos del Ayuntamiento de Valencia.",
                   href =
                     "https://valencia.opendatasoft.com/explore/dataset/estat-transit-temps-real-estado-trafico-tiempo-real/table/",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("Metrovalencia: "),
                 tags$a(
                   "base de datos procedente de Open Mobility Data",
                   href =
                     "https://transitfeeds.com/p/ferrocarriles-de-la-generalidad-valenciana/1039",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("Valenbisi: "),
                 tags$a(
                   "actualizado cada 10 minutos, procedente del catálogo de datos abiertos del Ayuntamiento de Valencia.",
                   href =
                     "https://valencia.opendatasoft.com/explore/dataset/valenbisi-disponibilitat-valenbisi-dsiponibilidad/",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("EMT: "),
                 tags$a(
                   "base de datos procedente del catálogo de datos abiertos del Ayuntamiento de Valencia, con posterior tratamiento de los datos mediante web-scraping para extraer los siguientes autobuses en cada parada.",
                   href =
                     "https://valencia.opendatasoft.com/explore/dataset/emt/",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("Cámaras de Tráfico: "),
                 tags$a(
                   "base de datos procedente del catálogo de datos abiertos del Ayuntamiento de Valencia.",
                   href =
                     "https://valencia.opendatasoft.com/explore/dataset/cameres-trafic-camaras-trafico/",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("DGT: "),
                 tags$a(
                   "base de datos procedente de la página web de la DGT.",
                   href =
                     "https://infocar.dgt.es/etraffic/BuscarElementos?latNS=39.6&longNS=-0.2&latSW=39.4&longSW=-0.5&zoom=13&accion=getElementos&Camaras=true&SensoresTrafico=true&SensoresMeteorologico=true&Paneles=true&Radares=true&IncidenciasRETENCION=true&IncidenciasOBRAS=true&IncidenciasMETEOROLOGICA=true&IncidenciasPUERTOS=true&IncidenciasOTROS=true&IncidenciasEVENTOS=true&IncidenciasRESTRICCIONES=true&niveles=true&caracter=acontecimiento",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$p(
                 tags$strong("Tiempo en Valencia: "),
                 tags$a(
                   "base de datos procedente de la AEMET.",
                   href =
                     "https://www.aemet.es/es/eltiempo/observacion/ultimosdatos?k=val&l=8416Y&w=0&datos=img&x=h24&f=temperatura",
                   target = "_blank",
                   style = "color: black; text-decoration: none;"
                 ),
                 style = "margin-left: 20px;"),
               tags$h2("Leyenda"),
               tags$p(
                 tags$strong("Estado 0, Fluído: las carreteras se encuentran con una baja ocupación de vehículos."),
                 style = "margin-left: 20px; color: chartreuse;"
               ),
               tags$p(
                 tags$strong("Estado 1, Denso: las carreteras se encuentran con una media ocupación de vehículos."),
                 style = "margin-left: 20px; color: darkorange;"
               ),
               tags$p(
                 tags$strong("Estado 2, Congestionado: hay atascos en estas carreteras a causa de una gran carga de vehículos."),
                 style = "margin-left: 20px; color: darkred;"
               ),
               tags$p(
                 tags$strong("Estado 3, Cortado: calle cortada."),
                 style = "margin-left: 20px; color: gray0;"
               ),
               tags$p(
                 tags$strong("Estado 4, Sin datos: no hay datos sobre esa vía. *"),
                 style = "margin-left: 20px; color: gray0;"
               ),
               tags$p(
                 tags$strong("Estado 5, Paso inferior fluído: los túneles se encuentran con una baja ocupación de vehículos."),
                 style = "margin-left: 20px; color: #458b74;"
               ),
               tags$p(
                 tags$strong("Estado 6, Paso inferior denso: los túneles se encuentran con una media ocupación de vehículos."),
                 style = "margin-left: 20px; color: #cd6600;"
               ),
               tags$p(
                 tags$strong("Estado 7, Paso inferior congestionado: los túneles se encuentran obstruidos por una gran cantidad de vehículos."),
                 style = "margin-left: 20px; color: darkred;"
               ),
               tags$p(
                 tags$strong("Estado 8, Paso inferior cortado: calle cortada. "),
                 style = "margin-left: 20px; color: gray0;"
               ),
               tags$p(
                 tags$strong("Estado 9, Sin datos: no hay datos sobre esa vía *"),
                 style = "margin-left: 20px; color: gray0;"
               ),
               tags$p(
                 tags$a("(*) Los colores de los estados 4 y 9 se han modificado para una mejor visualización del texto, realmente el color de dichos estados es el blanco"),
                 style = "margin-left: 20px; color: #b22222;"
               )
             )
    )
  )
)
