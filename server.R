

server <- function(input, output) {

  ## Control división territorial ----
  
  observeEvent(input$Division, {
    
    if (input$Division %in% c("Departamento", "Provincia")){
      
      if (input$Provincia %in% ListaProvincias){
        Elegido = input$Provincia
      } else {
        Elegido = "ARGENTINA"
      }
      
      updatePickerInput(
        session = getDefaultReactiveDomain(), 
        inputId = "Provincia", 
        choices = ListaProvincias, 
        selected = Elegido) 
      
    }
    
    if (input$Division == "Circuito") { 
      
      if (input$Provincia %in% ListaProvinciasCircuito){
        Elegido = input$Provincia
      } else {
        Elegido = "Ciudad Autónoma de Buenos Aires"
      }
      
      updatePickerInput(
        session = getDefaultReactiveDomain(), 
        inputId = "Provincia", 
        choices = ListaProvinciasCircuito %>% sort(), 
        selected = Elegido)
      
    }
    
  }, ignoreInit = TRUE)
  
  Mapa = reactive({
    
    r = departamentos %>% mutate(departamento = NAM)
    
    if (input$Division == "Circuito") if (input$Provincia %in% names(MapaCircuitos)) {
      
      r = MapaCircuitos[[input$Provincia]]
      
      if ("departamen" %in% variable.names(r)) r = r %>% mutate(departamento = departamen)
      
    }
    
    if (input$Division == "Provincia") r = provincias %>% mutate(departamento = NAM)
    
    r = r %>% sf::st_drop_geometry() %>% select(Provincia, departamento) %>% unique()
    
    return(r)
    
  })
  
  observeEvent(input$Provincia,{
    
    if (input$Provincia != "ARGENTINA"){
      
      TipoDivision = "Departamento"
      Todo = c("Toda la provincia" = "Todo")
      
      if (input$Provincia == "Ciudad Autónoma de Buenos Aires"){
        TipoDivision = "Comuna"
        Todo = c("Toda la ciudad" = "Todo")
      }
      if (input$Provincia == "Buenos Aires"){
        TipoDivision = "Partido"
      }
      
      ListaDepartamento = Mapa() %>%
        filter(Provincia == input$Provincia) %>%
        .$departamento %>% sort() %>% unique()
      
      names(ListaDepartamento) = ListaDepartamento
      
      ListaDepartamento = c(Todo, ListaDepartamento)
      
      if (input$Departamento %in% ListaDepartamento){
        Elegido = input$Departamento
      } else {
        Elegido = "Todo"
      }
      
      updatePickerInput(
        session = getDefaultReactiveDomain(), 
        inputId = "Departamento",
        label = TipoDivision, 
        choices = ListaDepartamento, 
        selected = Elegido
      )
      
    }
    
  }, ignoreInit = TRUE)
  
  ## Datos ----
  
  Datos = reactive({
    
    if (input$Instancia == "PASO"){
      
      if (input$TipoPaso == "Resultado por agrupación"){
        
        if (input$Division == "Circuito"){
          r = datosPorCircuitoPASO
        } else if (input$Division == "Provincia") {
          r = datosPorProvinciaPASO
        } else {
          r = datosPorDeptoPASO
        }
        
        
      } else if (input$TipoPaso == "Resultado por lista"){
        
        if (input$Division == "Circuito"){
          r = datosPorCircuitoInterna
        } else if (input$Division == "Provincia"){
          r = datosPorProvinciaInterna
        } else {
          r = datosPorDeptoInterna
        }
        
        r = r %>% mutate(agrupacion_nombre = paste0(Interna, " (", agrupacion_nombre, ")"))
        
      } else {
        
        PartidoInterna = input$TipoPaso
        PartidoInterna = sub("Interna de ", "", PartidoInterna)
        
        if (input$Division == "Circuito"){
          r = datosPorCircuitoInterna
        } else if (input$Division == "Provincia"){
          r = datosPorProvinciaInterna
        } else {
          r = datosPorDeptoInterna
        }
        
        r = r %>%
          filter(Interna == PartidoInterna) %>%
          select(-c(TOTAL, PorcPositivos, PorcTotal, Puesto)) %>%
          rename_all(~sub("_Interna", "", .))
        
      }
      
    } else if (input$Instancia == "Ballotage"){
      
      if (input$Division == "Circuito"){
        r = datosPorCircuitoBallotage %>% filter(agrupacion_nombre != "undefined")
      } else if (input$Division == "Provincia"){
        r = datosPorProvinciaBallotage %>% filter(agrupacion_nombre != "undefined")
      } else {
        r = datosPorDeptoBallotage %>% filter(agrupacion_nombre != "undefined") 
      }
      
    } else {
      
      if (input$Division == "Circuito"){
        r = datosPorCircuito
      } else if (input$Division == "Provincia"){
        r = datosPorProvincia
      } else {
        r = datosPorDepto
      }
      
    }
    
    return(r)
    
  })
  
  ## Reactive values ----
  
  rv = reactiveValues()
  rv$Inicio = TRUE
  
  rv$Puesto = 1
  rv$ColoresPartido = ColoresPartido
  
  rv$Actualizado = TRUE
  
  rv$MapaDepartamentos = list(
    datos = datosPorDepto %>% filter(cargo_nombre == "PRESIDENTE Y VICE"),
    Mapa = departamentos,
    cargo = "PRESIDENTE Y VICE",
    distrito = "ARGENTINA",
    Puesto = 1,
    Degradado = FALSE,
    Partido = NULL
  )
  
  rv$bbox = bboxArgentina
  rv$Titulo = "ARGENTINA"
  rv$Subtitulo = "PRESIDENTE Y VICE (Generales). 1° puesto, por departamento/partido/comuna."
  
  observeEvent(input$Puesto,{
    
    rv$Puesto = input$Puesto
    
  })
  
  output$BotonActualizar = renderUI({
   
    if (rv$Actualizado){
      
      r = list(actionButton(
        "Actualizado", "Tablero actualizado", style = "background-color: #d6d6d6;"))
      
    } else {
      
      r = list(actionButton(
        "Actualizar", "Actualizar tablero", style = "background-color: #d6d6d6;"))
      
    }
    
    return(r) 
    
  })
  
  ## Control Botones Eleccion ----
  
  Cambio = reactive({
    list(input$Provincia, input$Instancia)
  })
  
  observeEvent(Cambio(),{
    
    if (input$Provincia == "ARGENTINA"){
      ListaCargos1 = c("PRESIDENTE Y VICE")
    } else {
      ListaCargos1 = Datos() %>%
        filter(distrito_nombre == input$Provincia) %>%
        .$cargo_nombre %>% unique()
    }
    
    ListaCargos1 = ListaCargos[ListaCargos %in% ListaCargos1]
    
    if (input$Cargo %in% ListaCargos1){
      Elegido = input$Cargo
    } else {
      Elegido = ListaCargos1[1]
    }
    
    updatePickerInput(
      session = getDefaultReactiveDomain(), 
      inputId = "Cargo", 
      choices = "", 
      selected = "")
    
    updatePickerInput(
      session = getDefaultReactiveDomain(), 
      inputId = "Cargo", 
      choices = ListaCargos1, 
      selected = Elegido)
    
  }, ignoreInit = TRUE)
  
  ## Control Botones Colores ----
  
  Cambio3 = reactive({
    list(input$Cargo, input$Provincia, input$TipoPaso)
  })
  
  observeEvent(Cambio3(),{
    
    ListaPartidos1 = Datos() %>%
      filter(cargo_nombre == input$Cargo) 
    
    if (input$Provincia != "ARGENTINA"){
      
      ListaPartidos1 = ListaPartidos1 %>%
        filter(distrito_nombre == input$Provincia) 
      
    }
    
    ListaPartidos1 = ListaPartidos1 %>%
      .$agrupacion_nombre %>% unique(.)
    
    ListaPartidos1 = ListaPartidos1[ListaPartidos1 != ""]
    ListaPartidos1 = ListaPartidos1[ListaPartidos1 != " ()"]
    
    ListaPartidos1 = c("Puesto", paste("Porcentaje de", sort(ListaPartidos1)))
    
    if (input$Colorear %in% ListaPartidos1){
      Elegido = input$Colorear
    } else {
      Elegido = ListaPartidos1[1]
    }
    
    updatePickerInput(
      session = getDefaultReactiveDomain(), 
      inputId = "Colorear", 
      choices = ListaPartidos1, 
      selected = Elegido)
    
  }, ignoreInit = TRUE)
  
  ## Grafico ----
  
  observeEvent(input$Actualizar, {
    
    ## Mapa ----
    
    MapaFiltro = departamentos %>% mutate(departamento = NAM)
    
    if (input$Division == "Circuito") if (input$Provincia %in% names(MapaCircuitos)) {
      
      MapaFiltro = MapaCircuitos[[input$Provincia]]
      
      if ("departamen" %in% variable.names(MapaFiltro)){
        MapaFiltro = MapaFiltro %>% mutate(departamento = departamen)
      } 
      
    }
    
    if (input$Division == "Provincia") MapaFiltro = provincias %>% mutate(departamento = NAM)
    
    if (input$Colorear == "Puesto"){
      Partido = NULL
    } else {
      Partido = sub("Porcentaje de ", "", input$Colorear)
    }
    
    DatosFiltro = Datos()
    
    if (input$Provincia != "ARGENTINA"){
      
      DatosFiltro = DatosFiltro %>% filter(distrito_nombre == input$Provincia)
      
      MapaFiltro = MapaFiltro %>% filter(Provincia == input$Provincia)
      
      if (input$Departamento != "Todo" & input$Division != "Provincia"){
        
        DatosFiltro = DatosFiltro %>% filter(grepl(input$Departamento, NAM))
        
        MapaFiltro = MapaFiltro %>% filter(departamento == input$Departamento)
        
      }
      
    } 
    
    DatosFiltro = DatosFiltro %>% filter(cargo_nombre == input$Cargo)
    
    r = list(
      datos = DatosFiltro,
      Mapa = MapaFiltro,
      cargo = input$Cargo,
      distrito = input$Provincia,
      Puesto = input$Puesto,
      Degradado = input$Degradado,
      Partido = Partido
    )
    
    cat(input$Cargo, input$Instancia, input$Division, input$Provincia, input$Departamento,
        input$Puesto, Partido)
    
    #return(r)
    
    rv$MapaDepartamentos = r
    
    ### Bbox ----
    
    bbox = sf::st_bbox(rv$MapaDepartamentos$Mapa)
    
    if (bbox$ymin < -70) bbox[["ymin"]] = -70
    if (input$Provincia == "ARGENTINA") bbox = bboxArgentina
    if (input$Provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" &
        (input$Departamento == "Todo" | input$Division == "Provincia")){
      bbox = bboxTierraDelFuego
    } 
    
    rv$bbox = bbox
    
    ## Actualizado ----
    
    rv$Actualizado = TRUE
        
  })
  
  MapaDepartamentos = reactive({
    
    r = try(Mapear(
      datos = rv$MapaDepartamentos$datos,
      ColoresPartido = rv$ColoresPartido,
      Mapa = rv$MapaDepartamentos$Mapa,
      cargo = rv$MapaDepartamentos$cargo,
      distrito = rv$MapaDepartamentos$distrito,
      Puesto = rv$MapaDepartamentos$Puesto,
      Degradado = rv$MapaDepartamentos$Degradado,
      Partido = rv$MapaDepartamentos$Partido))

    if ("try-error" %in% class(r)){
      r = Mapear(
        datos = datosPorDepto, ColoresPartido = rv$ColoresPartido, Mapa = departamentos)
    }
    
    return(r)
    
  })
  
  output$Mapa = renderPlot({
    
    ListaColores = MapaDepartamentos()$Mapa %>% 
      sf::st_drop_geometry() %>% 
      select(grupo_elegido, color_elegido) %>%
      filter(!is.na(grupo_elegido)) %>%
      unique()
    
    listaColores1 = ListaColores$color_elegido
    listaColores1 = alpha(listaColores1, alpha = 0.7)
    names(listaColores1) = ListaColores$grupo_elegido
    
    r = ggplot()
    
    try({
      
      r = r + base_map_argentina(rv$bbox, increase_zoom = 2)
      
    })
    
    TamanioLetra = 12
    MaximoCaracteres = max(nchar(MapaDepartamentos()$Etiquetas$grupo_elegido), na.rm = TRUE)
    if (is.infinite(MaximoCaracteres) | is.na(MaximoCaracteres)) MaximoCaracteres = 0
    if (MaximoCaracteres > 40) TamanioLetra = 9
    
    r = r +
      geom_sf(data = MapaDepartamentos()$Mapa, mapping = aes(fill = grupo_elegido)) +
      scale_fill_manual(
        breaks = c(MapaDepartamentos()$Etiquetas$grupo_elegido, "No disponible"),
        values = listaColores1) +
      coord_sf(
        xlim = c(rv$bbox[["xmin"]], rv$bbox[["xmax"]]), 
        ylim = c(rv$bbox[["ymin"]], rv$bbox[["ymax"]]), 
        crs = 4326) +
      guides(fill = guide_legend(ncol = 1)) + 
      theme(
        legend.position = "top", 
        legend.title = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "panel",
        legend.text = element_text(size=TamanioLetra)
        ) 
    
    return(r)
    
  })
  
  ## Grafico Barra ----
  
  Barras = reactive({
    
    datosBarra = MapaDepartamentos()$Datos %>%
      filter(!is.na(agrupacion_nombre), !is.na(agrupacion_color)) %>%
      group_by(agrupacion_nombre, agrupacion_color) %>% 
      summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
      ungroup() %>% arrange(votos_cantidad) %>%
      mutate(
        agrupacion_nombre = factor(agrupacion_nombre, levels = c("Otros", agrupacion_nombre)), 
        Porcentaje = votos_cantidad / sum(votos_cantidad, na.rm = TRUE)) %>%
      mutate(
        Posicion = as.integer(agrupacion_nombre),
        Etiqueta = paste0(
          partirEnunciado(agrupacion_nombre, largo = 35), ": \n",
          puntos(votos_cantidad), " (", percent(Porcentaje, 1), ")"))
    
    
    if (nrow(datosBarra) > 6){
      
      datosOtros = datosBarra %>% slice(1:(nrow(.) - 6)) 
      
      datosOtros = datosOtros %>%
        summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
        mutate(
          agrupacion_nombre = factor("Otros", levels = c("Otros", datosBarra$agrupacion_nombre)), 
          Porcentaje = votos_cantidad / sum(datosBarra$votos_cantidad, na.rm = TRUE)) %>%
        mutate(
          Posicion = 1,
          Etiqueta = paste0(
            "Otros: \n",
            puntos(votos_cantidad), " (", percent(Porcentaje, 1), ")"))
      
      datosBarra = plyr::rbind.fill(datosOtros, datosBarra %>% slice((nrow(.) - 5):nrow(.)))
      
    }
    
    r = ggplot(data = datosBarra) + 
      geom_bar( 
        mapping = aes(x = agrupacion_nombre, y = Porcentaje, fill = agrupacion_nombre), 
        stat = "identity") + 
      scale_fill_manual(values = datosBarra$agrupacion_color) + 
      scale_y_continuous(labels = percent) + 
      geom_text(
        data = datosBarra, 
        mapping = aes(
          x = agrupacion_nombre, y = max(Porcentaje, na.rm = TRUE) / 2, 
          label = Etiqueta), 
        size = 4) + 
      coord_flip() + 
      theme(
        axis.text.x = element_text(angle = 0, size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "none",
        legend.title.align = 0.5,
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(hjust = 0, size = 12),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "solid", colour = "darkseagreen1")) + 
      guides(
        fill = guide_legend(title.position = "top", title = "Partido", ncol = 4)) 
    
    return(r)
    
  })
  
  output$Barras = renderPlot({
    Barras()
  })
  
  ## Colores ----
  
  output$tablaColores = DT::renderDataTable({
    
    listaOpciones = function(x){
      
      Aux = function(y){
        t = cbind.data.frame(
          ifelse(listaColores == y, 'selected = "true"', ''), listaColores, names(listaColores))
        
        r = paste0('<option ',t[[1]],' value="', t[[3]], '">', t[[2]], '</option>', collapse = "")
        
        return(r)
        
      }
      
      r = sapply(x, Aux)
      return(r)
      
    }
    
    tablaColor = rv$TablaColores %>%
      mutate(
        R = glue::glue(
          '<input type = "number" id="custom_btn1" min = "0" max = "255" value = "{red}"',
          'onclick="Shiny.onInputChange(\'button_color\', \'1_\' + \'{nn}\' + \'_\' + this.value)">'),
        V = glue::glue(
          '<input type = "number" id="custom_btn2" min = "0" max = "255" value = "{green}"',
          'onclick="Shiny.onInputChange(\'button_color\', \'2_\' + \'{nn}\' + \'_\' + this.value)">'),
        A = glue::glue(
          '<input type = "number" id="custom_btn3" min = "0" max = "255" value = "{blue}"',
          'onchange="Shiny.onInputChange(\'button_color\', \'3_\' + \'{nn}\' + \'_\' + this.value)">'),
        Lista = paste0(
          '<select id="listaColores" style = "color:black" ', 
          'onchange="Shiny.onInputChange(\'list_color\', \'', nn, '\' + \'_\' + this.value)">',
          listaOpciones(nombre), '</select>'),
        Color = glue::glue('<a style = "background-color:{color_elegido};padding-left: 60px;" ></a>')) %>%
      select(-c(nn, red, blue, green, color_elegido, nombre))
    
    tablaColor = tablaColor %>%
      DT::datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(
          searching = FALSE,
          pageLength = 10,
          scrollX = TRUE,
          lengthMenu = c(12, 24, 48, 96),
          dom = '<lfp<t>i>',
          language = list(
            info = 'Mostrando registros _START_ a _END_ de un total de _TOTAL_',
            paginate = list(previous = 'Anterior', `next` = 'Siguiente'),
            lengthMenu = 'Mostrar _MENU_ registros',
            infoEmpty = 'No hay registros que coincidan con la búsqueda',
            zeroRecords = 'No hay registros que coincidan con la búsqueda',
            loadingRecords = "Procesando...",
            processing = "Procesando...",
            search = "Buscar")#,
          #fixedColumns = list(leftColumns = ColumnasFijas())
        )
      )
    
    return(tablaColor)
    
  })
  
  observeEvent(input$VerColores,{
    
    tablaColor = MapaDepartamentos()$tablaColor
    
    tablaColor$nombre = sapply(tablaColor$color_elegido, ColorCercano)
    
    tablaColor = cbind.data.frame(
      tablaColor, t(col2rgb(tablaColor$color_elegido)))
    
    rv$TablaColores = tablaColor %>% mutate(nn = 1:n())
    
    showModal(
      modalDialog({
        DT::dataTableOutput("tablaColores")
      },
      title = 'Colores',
      footer = list(
        actionButton("AplicarModal","Aplicar"),
        actionButton("RestablecerColor","Reestablecer colores"),
        actionButton("CerrarModal","Cerrar")),
      easyClose = FALSE,
      )
    )
    
  })
  
  observeEvent(input$CerrarModal,{
    removeModal()
  })
  
  observeEvent(input$AplicarModal,{
    
    try({
      
      rv$ColoresPartido = merge(
        rv$ColoresPartido, rv$TablaColores, 
        by.x = "agrupacion_nombre", by.y = "agrupacion", 
        all.x = TRUE)
      
      rv$ColoresPartido = rv$ColoresPartido %>%
        mutate(agrupacion_color = ifelse(is.na(color_elegido), agrupacion_color, color_elegido)) %>%
        select(agrupacion_nombre, agrupacion_color)
      
    })
    
    removeModal()
  })
  
  observeEvent(input$RestablecerColor,{
    
    try({
      
      rv$ColoresPartido = ColoresPartido
      
    })
    
    removeModal()
  })
  
  observeEvent(input$button_color,{
    
    a = substr(input$button_color, 1,1)
    h = gregexpr("_", input$button_color)[[1]][2]
    b = substr(input$button_color, 3, h - 1)
    c = substr(input$button_color, h+1, nchar(input$button_color))
    
    if (a == "1"){
      rv$TablaColores[as.numeric(b), "red"] = as.numeric(c)
    }
    if (a == "2"){
      rv$TablaColores[as.numeric(b), "green"] = as.numeric(c)
    }
    if (a == "3"){
      rv$TablaColores[as.numeric(b), "blue"] = as.numeric(c)
    }
    
    rv$TablaColores = rv$TablaColores %>%
      mutate(color_elegido = rgb(red/255, green/255, blue/255))
    
    rv$TablaColores$nombre = sapply(rv$TablaColores$color_elegido, ColorCercano)
    
  })
  
  observeEvent(input$list_color,{
    
    h = regexpr("_", input$list_color)
    c = substr(input$list_color, h + 1, nchar(input$list_color))
    n = as.numeric(substr(input$list_color, 1, h - 1))
    
    rv$TablaColores$color_elegido[n] = c
    rv$TablaColores$red[n] = col2rgb(c)[1]
    rv$TablaColores$green[n] = col2rgb(c)[2]
    rv$TablaColores$blue[n] = col2rgb(c)[3]
    
    rv$TablaColores$nombre = sapply(rv$TablaColores$color_elegido, ColorCercano)
    
  })
  
  ## Puesto Maximo ----
  
  PuestoMaximo = reactive({
    
    Valor = Datos() %>% filter(cargo_nombre == input$Cargo) %>% 
      .$Puesto %>% max(., na.rm = TRUE)
    
    if (is.na(Valor) | is.infinite(Valor)) Valor = 5
    Valido = rv$Puesto <= Valor
    
    return(list("Valor" = Valor, "Valido" = Valido))
    
  })
  
  observeEvent(PuestoMaximo(),{
    
    if (rv$Puesto > PuestoMaximo()$Valor) rv$Puesto = PuestoMaximo()$Valor
    
    updateNumericInput(
      session = getDefaultReactiveDomain(), 
      inputId = "Puesto", 
      max = PuestoMaximo()$Valor#, 
      #value = rv$Puesto
    )
    
  }, ignoreInit = TRUE)
  
  ## Titulo ----
  
  observeEvent(rv$MapaDepartamentos,{
    
    ## Titulo ----
    
    Inicio = ""
    
    if (input$Departamento != "Todo" & input$Division != "Provincia" & input$Provincia != "ARGENTINA"){
      
      Inicio = paste0(input$Departamento, ", ")
      if (input$Provincia == "Buenos Aires"){
        Inicio = paste("Partido de", Inicio)
      } else if (input$Provincia != "Ciudad Autónoma de Buenos Aires") {
        Inicio = paste("Departamento", Inicio)
      }
    }
    
    if (!input$Provincia %in% c("ARGENTINA", "Ciudad Autónoma de Buenos Aires")){
      Inicio = paste0(Inicio, "Provincia de ")
    }
    
    r = paste0(Inicio, input$Provincia, ". ")
    
    rv$Titulo = r
    
    ## Subitulo ----
    
    r = paste0(input$Cargo, " (", input$Instancia, "). ")
    
    if (input$Instancia == "PASO") {
      r = paste0(r, input$TipoPaso, ". ")
    }
    
    if (input$Colorear == "Puesto"){
      r = paste0(r, MapaDepartamentos()$Puesto, "° puesto, ")
    } else {
      r = paste0(r, input$Colorear, ", ")
    }
    
    if (input$Division == "Departamento"){
      if (input$Provincia == "ARGENTINA"){
        Division = "departamento/partido/comuna"
      } else if (input$Provincia == "Buenos Aires"){
        Division = "partido"
      } else if (input$Provincia == "Ciudad Autónoma de Buenos Aires"){
        Division = "comuna"
      } else {
        Division = "departamento"
      }
    } else {
      Division = input$Division
    }
    
    r = paste0(r, "por ", tolower(Division), ".")
    
    rv$Subtitulo = r
    
  }, ignoreInit = TRUE)

  output$Titulo = renderText({
    rv$Titulo
  })
  
  output$Subtitulo = renderText({
    rv$Subtitulo
  })

  ## ControlActualizacion ----
  
  ParaBoton = reactive({
    list(
      input$Division, input$Instancia, input$Provincia, input$Departamento, input$Instancia, 
      input$Cargo, input$TipoPaso, input$Colorear, input$Puesto, input$Degradado
    )
  })
  
  observeEvent(ParaBoton(),{
    rv$Actualizado = FALSE
    
    if (rv$Inicio){
      showModal(
        modalDialog(
          h4("Cuando termine de elegir los parámetros, pulse el botón Actualizar tablero para
             aplicarlos."),
          footer = modalButton("Cerrar"),
          easyClose = TRUE))
    }
    
    rv$Inicio = FALSE
    
  }, ignoreInit = TRUE)
  
}