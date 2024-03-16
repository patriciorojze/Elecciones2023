
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)

ListaComandosTerritorial = list(
  pickerInput(
    inputId = "Division", 
    label = "División Territorial", 
    choices = c("Provincia", "Departamento", "Circuito"), 
    selected = "Departamento", 
    multiple = FALSE
  ),
  pickerInput(
    inputId = "Provincia", 
    label = "Provincia", 
    choices = ListaProvincias, 
    selected = "ARGENTINA", 
    multiple = FALSE
  ),
  conditionalPanel(
    condition = 'input.Provincia != "ARGENTINA" & input.Division != "Provincia"',
    pickerInput(
      inputId = "Departamento", 
      label = "Comuna", 
      choices = c(
        "Toda la ciudad" = "Todo", 
        MapaCircuitos[["Ciudad Autónoma de Buenos Aires"]]$departamen %>% unique(.) %>% 
          sort(.)), 
      selected = "ARGENTINA", 
      multiple = FALSE
    )
  )
)

ListaComandosEleccion = list(
  pickerInput(
    inputId = "Instancia", 
    label = "Instancia", 
    choices = c("PASO", "Generales", "Ballotage"), 
    selected = "Generales", 
    multiple = FALSE
  ),
  pickerInput(
    inputId = "Cargo", 
    label = "Cargo", 
    choices = ListaCargos, 
    selected = "PRESIDENTE Y VICE", 
    multiple = FALSE
  ),
  conditionalPanel(
    condition = "input.Instancia == 'PASO'",
    pickerInput(
      inputId = "TipoPaso", 
      choices = ListaPaso, 
      selected = ListaPaso[1], 
      multiple = FALSE
    )
  )
)

ListaComandosColores = list(
  pickerInput(
    inputId = "Colorear", 
    label = "Colorear según", 
    choices = ListaPartidos, 
    selected = "Puesto", 
    multiple = FALSE
  ),
  conditionalPanel(
    condition = "input.Colorear == 'Puesto'",
    numericInput(
      inputId = "Puesto", 
      label = "Puesto", 
      value = 1, 
      min = 1, 
      max = 5, 
      step = 1
    ),
    checkboxInput(
      inputId = "Degradado", 
      label = "Degradado", 
      value = FALSE
    )
  )
)

ListaBoton = list(
  div(
    style = 'padding-right:20px; margin-right:3px;
                    padding-left:20px; margin-left:5px;
                    padding-top:5px; margin-top:5px;
                    padding-bottom:5px; margin-bottom:5px;',
    column(width = 3, uiOutput("BotonActualizar")),
    column(
      width = 3,
      actionButton(
      inputId = "VerColores", 
      label = "Ver colores utilizados",
      style = "background-color: #d6d6d6;")
    ), 
    column(width = 6)
  )
)

ListaTitulo = list(
  div(
    style = 'padding-right:20px; margin-right:3px;
                    padding-left:20px; margin-left:5px;
                    padding-top:5px; margin-top:5px;
                    padding-bottom:5px; margin-bottom:5px;',
    h2(textOutput("Titulo")),
    h4(textOutput("Subtitulo"))
  )
)

ListaGrafico = list(
  div(
    id = "MapaEleccionContainer",
    style = 'padding-right:20px; margin-right:3px;
                        padding-left:20px; margin-left:5px;
                        padding-top:20px; margin-top:5px;
                        padding-bottom:20px; margin-bottom:5px;',
    withSpinner(plotOutput("Mapa", height = "700px")),
  )
)

ListaBarra = list(
  div(
    id = "BarrasContainer",
    style = 'padding-right:20px; margin-right:5px;
                        padding-left:20px; margin-left:3px;
                        padding-top:20px; margin-top:5px;
                        padding-bottom:20px; margin-bottom:5px;', 
    withSpinner(plotOutput("Barras"))
  )
)

Aclaraciones = list(
  div(
    style = 'padding-right:20px; margin-right:3px;
                        padding-left:25px; margin-left:5px;
                        padding-top:0px; margin-top:5px;
                        padding-bottom:20px; margin-bottom:5px;',
    HTML(
      '<a style = "font-size:15px; color:black"> Resultados provisorios. 
            Los resultados pueden consultarse en la página de la </a>
           <a style = "font-size:15px; color:blue" 
            href = "https://www.argentina.gob.ar/dine/resultados-electorales/elecciones-2023"
            target="_blank"> 
            Dirección Nacional Electoral </a>
           <a style = "font-size:15px; color:black"> y en </a>
           <a style = "font-size:15px; color:blue" href = "https://resultados.gob.ar/" target="_blank"> 
            resultados.gob.ar </a> <br>
           <a style = "font-size:15px; color:black"> Los circuitos electorales pueden descargarse a
            través de la página de la </a>
           <a style = "font-size:15px; color:blue" 
            href = "https://mapa2.electoral.gov.ar/descargas/"
            target = "_blank"> 
            Cámara Nacional Electoral.</a> <br>
           <a style = "font-size:15px; color:black"> Mapa de fondo: </a>
           <a style = "font-size:15px; color:blue" 
            href= "https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/ServiciosOGC"
            target = "_blank">
            Instituto Geográfico Nacional. </a>  
           '
    )
  )
)

InformacionContacto = list(
  div(
    style = 'padding-right:20px; margin-right:3px;
                        padding-left:25px; margin-left:5px;
                        padding-top:0px; margin-top:5px;
                        padding-bottom:20px; margin-bottom:5px;',
    HTML(
      '<a style = "font-size:12px; color:black"> Cualquier consulta dirigirse a </a>
       <a style = "font-size:12px; color:blue" 
        href = "mailto:patriciorojze@gmail.com?subject=Consulta"
        target = "_blank">
        patriciorojze@gmail.com </a> <br>
       <a style = "font-size:12px; color:black"> Contacto LinkedIn: </a>
       <a style = "font-size:12px; color:blue" 
        href = "https://www.linkedin.com/in/patricio-rojze/"
        target = "_blank">
        patricio-rojze </a><br>
       <a style = "font-size:12px; color:black"> Contacto GitHub: </a>
       <a style = "font-size:12px; color:blue" 
        href = "https://github.com/patriciorojze"
        target = "_blank">
        patriciorojze </a>'
    )
  )
)

ui = dashboardPage(
  dashboardHeader(title = "Elecciones 2023"),
  dashboardSidebar(
    ListaComandosTerritorial,
    ListaComandosEleccion, 
    ListaComandosColores
  ),
  dashboardBody(
    fluidRow(ListaBoton),
    fluidRow(ListaTitulo),
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 8, ListaGrafico),
      column(width = 4, ListaBarra)),
    fluidRow(Aclaraciones),
    fluidRow(InformacionContacto)
  )
)

