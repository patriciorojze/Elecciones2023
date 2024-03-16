
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(mapview)
library(webshot)
library(png)
library(grid)
library(gridExtra)
library(png)

#devtools::install_github('Chrisjb/basemapR')
library(basemapR)

ActivarActualizacion = FALSE #Activar en desarrollo

### Funciones auxiliares ----

convertirHoras = function(t){
  
  h = floor(t / 3600)
  m = floor((t - h * 3600) / 60)
  s = floor(t - h * 3600 - m * 60)
  
  horas = ifelse(h == 1, "hora,", "horas,")
  minutos = ifelse(m == 1, "minuto,", "minutos,")
  segundos = ifelse(s == 1, "segundo", "segundos")
  
  if (t > 3600){
    r = paste(h, horas, m, minutos, s, segundos)
  } else if (t > 60){
    r = paste(m, minutos, s, segundos)
  } else {
    r = paste(s, segundos)
  }
  
  return(r)
  
}

percent <- function(x, digits = 0){
  x1 <- sapply(x, function(y){
    if (!is.na(y) & !is.infinite(y)){
      if (digits >0){
        y <- round(y, digits = digits + 2)
        e <- abs(y*100) - floor(abs(y*100))
        f <- as.character(round(e *10^digits,0))
        if (nchar(f) < digits){ 
          f <- paste0(paste0(rep("0", times = digits - nchar(f)), collapse = ""), f)
        } 
        f <- paste0(",", f)
        r <- paste0(floor(abs(y*100)), f, "%")
      } else {
        r <- paste0(round(abs(y*100),0), "%")
      }
      if (y<0) r <- paste0("-", r)
    } else {
      r <- NA
    }
    
    return(r)
  })
  return(x1)
}

puntos <- function(x, decimales = 0){
  
  if (sum(!is.na(x) & !is.infinite(x)) == 0) return(x) 
  
  x <- round(x, digits = decimales)
  if (decimales == 0) entero <- round(abs(x), 0) else entero <- floor(abs(x))
  
  entero_character <- format(entero, scientific = FALSE)
  entero_character <- recortar(entero_character, inicio = TRUE)
  
  largo <- ceiling(nchar(entero_character)/3)
  x1 <- c()
  
  for (i in 1:max(largo, na.rm = TRUE)){
    x2 <- substr(entero_character, start = nchar(entero_character) - 2 - (i-1)* 3, 
                 stop = nchar(entero_character) - (i-1)* 3)
    if (length(x1) == 0){
      x1 <- x2
    } else {
      seleccion <- x2!="" & !is.na(x2)
      x1[seleccion] <- paste0(x2[seleccion], ".", x1[seleccion])
    }
  }
  
  if (decimales > 0) {
    parte <- abs(x) - entero
    parteC <- as.character(round(parte, digits = decimales))
    parteC <- paste0(parteC, paste0(rep("0",length.out= decimales + 3), collapse = ""))
    x3 <- substr(parteC, start = 3, stop = 3 + decimales - 1)
    x1 <- paste0(x1, ",", x3)
  }
  
  x1 <- paste0(ifelse(x < 0, "-", ""), x1)
  x1[is.na(x) | is.infinite(x)] <- NA
  return(x1)
}

recortar <- function(x, inicio = FALSE, interno = FALSE){
  x1 <- x
  if (any(!is.na(x1))){
    x2 <- substr(x1, start = nchar(x1), stop = nchar(x1))
    x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
    while (any(x2 %in% " ") & x3 > 0 & !is.infinite(x3)) {
      x4 <- x2 == " " & !is.na(x2) 
      x1[x4] <- substr(x1[x4], start = 1, stop = (nchar(x1[x4])-1))
      x2 <- substr(x1, start = nchar(x1), stop = nchar(x1))
      x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
    }
    if (inicio){
      x2 <- substr(x1, start = 1, stop = 1)
      x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
      while (any(x2 %in% " ") & x3 > 0 & !is.infinite(x3)) {
        x4 <- x2 == " " & !is.na(x2) 
        x1[x4] <- substr(x1[x4], start = 2, stop = nchar(x1[x4]))
        x2 <- substr(x1, start = 1, stop = 1)
        x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
      }
    }
    if (interno){
      x2 <- sapply(gregexpr(pattern = "  ", text = x1), max)
      x3 <- max(x2, na.rm = TRUE)
      while(x3 > -1 & !is.infinite(x3)){
        x4 <- x2 & !is.na(x1)
        x1[x4] <- gsub(pattern = "  ", replacement = " ", x = x1[x4])
        x2 <- sapply(gregexpr(pattern = "  ", text = x1), max)
        x3 <- max(x2, na.rm = TRUE)
      }
    }
  }
  return(x1)
}

SacarCeros = function(x){
  x1 <- x
  if (any(!is.na(x1))){
    x2 <- substr(x1, start = 1, stop = 1)
    x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
    while (any(x2 %in% "0") & x3 > 0 & !is.infinite(x3)) {
      x4 <- x2 == "0" & !is.na(x2) 
      x1[x4] <- substr(x1[x4], start = 2, stop = nchar(x1[x4]))
      x2 <- substr(x1, start = 1, stop = 1)
      x3 <- suppressWarnings(max(nchar(x1), na.rm = TRUE))
    }
    
  }
  return(x1)
}

partirEnunciado <- function(x, largo = 20){
  Aux = function(x){
    x <- as.character(x)
    x1 <- x
    x2 <- c()
    
    while (nchar(x1)>largo) {
      espacios <- gregexpr(" ",x1)[[1]]
      if (length(espacios)>1){
        corte <- max(espacios[espacios<largo])
      } else {
        corte <- largo
      }
      if (is.infinite(corte)){
        corte <- largo
      }
      x3 <- substr(x1, start = 1, stop = corte - 1)
      x1 <- substr(x1, start = corte + 1, stop = nchar(x1))
      x2 <- c(x2, x3)
    }
    x2 <- c(x2, x1)
    x2 <- paste(x2, sep = "\n", collapse = "\n")
    return(x2)
  }
  
  r = sapply(x, Aux)
  return(r)
} 

### Lista Mapas ----

ActualizarMapa = FALSE

if (!"MapaDatos.RData" %in% list.files(path = "data")) ActualizarMapa = TRUE

if (!ActualizarMapa){
  
  ActualizacionArchivosMapa = sapply(
    list.files(path = "data/Mapa", full.names = TRUE, recursive = TRUE), 
    function(x) file.info(x)$mtime)
  
  ActualizacionArchivosMapa = as_datetime(ActualizacionArchivosMapa) %>% 
    max(., na.rm = TRUE)
  ActualizacionDatos = file.info("data/MapaDatos.RData")$mtime
  
  ActualizacionProceso = file.info("data/datosMapas.R")$mtime
  
  if (ActualizacionDatos < ActualizacionArchivosMapa | ActualizacionDatos < ActualizacionProceso){
    ActualizarMapa = TRUE
  } 
  
} 

if (ActualizarMapa & ActivarActualizacion){
  
  cat("Actualizando mapas \n")
  
  t1 = proc.time()
  
  source("data/datosMapas.R")
  
  cat("Mapa finalizado\n")
  cat(convertirHoras((proc.time() - t1)[3]), "\n")
  
} else {
  
  load("data/MapaDatos.RData")
  
}


### Lista Datos ----

ActualizarDatos = FALSE

if (!"DatosProcesados.RData" %in% list.files(path = "data")) ActualizarDatos = TRUE

if (!"ListaDatos.RData" %in% list.files(path = "data")) ActualizarDatos = TRUE

if (!ActualizarDatos){
  
  ActualizacionArchivos = file.info("data/DatosProcesados.RData")$mtime
  
  ActualizacionProceso = file.info("data/procesar datos.R")$mtime
  
  ActualizacionProceso2 = file.info("data/datosPrimarios.R")$mtime
  
  if (ActualizacionProceso > ActualizacionArchivos | ActualizacionProceso2 > ActualizacionArchivos){
    ActualizarDatos = TRUE
  } 
  
} 

if (ActualizarDatos & ActivarActualizacion){
  
  cat("Actualizando datos \n")
  
  t1 = proc.time()
  
  source("data/procesar datos.R")
  
  cat("Datos finalizado\n")
  cat(convertirHoras((proc.time() - t1)[3]), "\n")
  
} else {
  
  load("data/DatosProcesados.RData")
  
}

## Previo a ejecución

#install.packages("webshot")
#webshot::install_phantomjs(force = TRUE)

### Funciones ----

Mapear = function(datos, ColoresPartido, Mapa, cargo = "PRESIDENTE Y VICE", distrito = "ARGENTINA", 
                  Puesto = 1, Degradado = FALSE, soloDato = FALSE, Partido = NULL){
  
  if (!is.numeric(Puesto)) Puesto = 1
  Puesto = floor(Puesto)
  if (Puesto < 1) Puesto = 1
  
  ResultadosDepto = datos %>% 
    filter(
      votos_tipo ==  "POSITIVO", cargo_nombre == cargo, !is.na(votos_cantidad), votos_cantidad != 0)
  
  if (distrito != "ARGENTINA"){
    
    ResultadosDepto = ResultadosDepto %>% filter(distrito_nombre == distrito)
    
  } 
  
  if (soloDato) return(list("Datos" = ResultadosDepto))
  
  if (cargo %in%  c("PRESIDENTE Y VICE", "PARLAMENTO MERCOSUR NACIONAL")){
    colorProv = "ARGENTINA"
  } else {
    colorProv = distrito
  }
  
  if (distrito == "ARGENTINA"){
    departamentos2 = Mapa
  } else {
    departamentos2 = Mapa[Mapa$Provincia == distrito,]
  }
  
  ResultadosDepto = merge(ResultadosDepto, ColoresPartido, all.x = TRUE)
  
  departamentos2$PopUp = ""
  departamentos2$color_elegido = "gray" 
  departamentos2$grupo_elegido = ""
  
  if (all(departamentos2$NAM == departamentos2$Provincia)){
    departamentos2$PopUpNombre = departamentos2$NAM
  } else {
    departamentos2$PopUpNombre = paste0(departamentos2$NAM, ", ", departamentos2$Provincia)
  }
  
  Etiquetas = data.frame()
  
  tablaColor = data.frame(
    "agrupacion" = "blanco",
    "color_elegido" = "#FFFFFF"
  )
  
  if (nrow(ResultadosDepto) > 0 & any(!is.na(ResultadosDepto$Puesto))) {
    
    departamentos2 = departamentos2 %>% select(-c(color_elegido, grupo_elegido))
    
    for (i in 1:max(ResultadosDepto$Puesto, na.rm = TRUE)){
      
      departamentos2 = merge(
        departamentos2, 
        ResultadosDepto %>% filter(Puesto == i) %>% 
          select(NAM, distrito_nombre, agrupacion_nombre, PorcPositivos, agrupacion_color), 
        by.x = c("NAM", "Provincia"), by.y = c("NAM", "distrito_nombre"), 
        all.x = TRUE, suffixes = c("", paste0("_", i)))
      
    }
    
    departamentos2 = departamentos2 %>%
      mutate(PopUp = paste0(
        "<b>", PopUpNombre, "</b>", 
        '<table style="width:100%">'))
    
    for (i in 1:max(ResultadosDepto$Puesto, na.rm = TRUE)){
      
      if (i == 1) i1 = "" else i1 = paste0("_", i)
      
      departamentos2$PopUp = 
        ifelse(
          is.na(departamentos2[[paste0("PorcPositivos",i1)]]), departamentos2$PopUp,
          paste0(
            departamentos2$PopUp,
            '<tr> <td> ', i,'- </td> <td><span class="tab"></span>', 
            departamentos2[[paste0("agrupacion_nombre", i1)]], 
            ': </td> <td  align="right">', percent(departamentos2[[paste0("PorcPositivos",i1)]], 1),
            '</td> </tr>'
          )
        )
      
    }
    
    departamentos2$PopUp = paste0(departamentos2$PopUp, '</table>')
    
    departamentos2$color_elegido = departamentos2$agrupacion_color
    departamentos2$grupo_elegido = departamentos2$agrupacion_nombre
    departamentos2$porcentaje_elegido = departamentos2$PorcPositivos
    
    if (Puesto > max(ResultadosDepto$Puesto, na.rm = TRUE)){
      Puesto = max(ResultadosDepto$Puesto, na.rm = TRUE) 
    } 
    
    if (Puesto > 1 & is.null(Partido)){
      departamentos2$color_elegido = departamentos2[[paste0("agrupacion_color_", Puesto)]]
      departamentos2$grupo_elegido = departamentos2[[paste0("agrupacion_nombre_", Puesto)]]
      departamentos2$porcentaje_elegido = departamentos2[[paste0("PorcPositivos_", Puesto)]]
      
    } 
    
    if (!is.null(Partido)) if (Partido %in% ResultadosDepto$agrupacion_nombre){
      
      departamentos2 = merge(
        departamentos2, 
        ResultadosDepto %>% filter(agrupacion_nombre == Partido) %>% 
          select(NAM, distrito_nombre, agrupacion_nombre, PorcPositivos, agrupacion_color), 
        by.x = c("NAM", "Provincia"), by.y = c("NAM", "distrito_nombre"), 
        all.x = TRUE, suffixes = c("", "_partido")
      )
      
      departamentos2$color_elegido = departamentos2$agrupacion_color_partido
      departamentos2$grupo_elegido = departamentos2$agrupacion_nombre_partido
      departamentos2$porcentaje_elegido = departamentos2$PorcPositivos_partido
      
    }
    
    Etiquetas = departamentos2 %>%
      filter(!is.na(grupo_elegido)) %>%
      select(grupo_elegido, color_elegido) %>%
      sf::st_drop_geometry() %>%
      unique() %>%
      arrange(grupo_elegido)
    
    tablaColor = departamentos2 %>%
      sf::st_drop_geometry() %>%
      select(grupo_elegido, color_elegido) %>% 
      filter(!is.na(grupo_elegido)) %>%
      rename(agrupacion = grupo_elegido) %>%
      unique() %>% arrange(agrupacion)
    
    if (Degradado | !is.null(Partido)){
      
      PorcMax = max(departamentos2$porcentaje_elegido, na.rm = TRUE)
      PorcMin = min(departamentos2$porcentaje_elegido, na.rm = TRUE) - 0.001
      
      departamentos2$GrupoPorc = ceiling((
        departamentos2$porcentaje_elegido - PorcMin) / (PorcMax - PorcMin) * 10)
      
      departamentos3 = departamentos2
      
      for (color in unique(departamentos2$color_elegido)) if (!is.na(color)){
        
        degradado = colorRampPalette(c("white", color))(10)
        
        coincide = departamentos2$color_elegido == color & !is.na(departamentos2$color_elegido)
        
        departamentos3$color_elegido[coincide] = degradado[departamentos2$GrupoPorc[coincide]]
        
      }
      
      departamentos2 = departamentos3
      
      Etiquetas = departamentos2 %>%
        sf::st_drop_geometry() %>%
        filter(!is.na(grupo_elegido), !is.na(porcentaje_elegido)) %>%
        group_by(color_elegido) %>%
        mutate(
          PorcMaximo = max(porcentaje_elegido, na.rm = TRUE), 
          PorcMinimo = min(porcentaje_elegido, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(grupo_elegido, PorcMaximo) %>%
        mutate(
          grupo_elegido = paste(grupo_elegido, percent(PorcMinimo, 1), "-", percent(PorcMaximo, 1))) %>%
        select(grupo_elegido, color_elegido) %>%
        unique() 
      
      departamentos2 = merge(
        departamentos2, Etiquetas, by = "color_elegido", all.x = TRUE, 
        suffixes = c("_anterior", ""))
      
    }
    
    departamentos2 = departamentos2 %>%
      mutate(
        grupo_elegido = ifelse(is.na(grupo_elegido), "No disponible", grupo_elegido),
        color_elegido = ifelse(is.na(color_elegido), "#404040", color_elegido)
      )
    
  }
  
  departamentos2 = departamentos2 %>% select(PopUp, PopUpNombre, grupo_elegido, color_elegido)
  ResultadosDepto = ResultadosDepto %>% select(agrupacion_nombre, agrupacion_color, votos_cantidad)
  
  return(list(
    "Mapa" = departamentos2, 
    "Datos" = ResultadosDepto, 
    "Etiquetas" = Etiquetas, 
    "tablaColor" = tablaColor,
    "Puesto" = Puesto))
  
}


## Modificacion de basemapR::base_map

base_map_argentina = function(bbox, increase_zoom = 0, nolabels = F) {
  
  require(httr)
  require(spectralGP)
  require(purrr)
  
  sec = function (x){
    1/cos(x)
  }
  
  lonlat2xy = function (lat_deg, lon_deg, zoom) {
    n <- 2^zoom
    x <- (n * (lat_deg + 180))%/%360
    lon_rad <- lon_deg * pi/180
    y <- (n * (1 - log(tan(lon_rad) + sec(lon_rad))/pi))%/%2
    list(x = x, y = y)
  }
  
  xy2lonlat = function (x, y, zoom) {
    n <- 2^zoom
    lon_deg <- x/n * 360 - 180
    lat_rad <- atan(sinh(pi * (1 - 2 * y/n)))
    lat_deg <- lat_rad * 180/pi
    list(lon_deg = lon_deg, lat_deg = lat_deg)
  }
  
  get_tile = function (url){
    tmp <- tempfile()
    h <- curl::new_handle()
    #curl::handle_setheaders(h, `User-Agent` = "basemapR")
    curl::handle_setheaders(h)
    curl::curl_download(url, destfile = tmp)
    tryCatch(png::readPNG(tmp), error = function(e) {
      jpeg::readJPEG(tmp)
    })
  }
  
  x_len <- bbox["xmax"] - bbox["xmin"]
  y_len <- bbox["ymax"] - bbox["ymin"]
  x_zoom <- sum(x_len < 360/2^(0:19)) - 1
  y_zoom <- sum(y_len < 170.1022/2^(0:19)) - 1
  zoom <- min(x_zoom, y_zoom)
  zoom <- zoom + increase_zoom
  corners <- expand.grid(x = bbox[c(1, 3)], y = bbox[c(2, 4)])
  xy <- lonlat2xy(bbox[c("xmin", "xmax")], bbox[c("ymin", "ymax")] ,zoom)
  
  
  tiles <- expand.grid(x = seq(xy$x["xmin"], xy$x["xmax"]), 
                       y = seq(xy$y["ymin"], xy$y["ymax"]))
  nw_corners <- purrr::pmap_dfr(tiles, xy2lonlat, zoom = zoom)
  se_corners <- purrr::pmap_dfr(dplyr::mutate_all(tiles, `+`, 1), xy2lonlat, zoom = zoom)
  names(nw_corners) <- c("xmin", "ymax")
  names(se_corners) <- c("xmax", "ymin")
  tile_positions <- dplyr::bind_cols(nw_corners, se_corners)
  url <- paste0(
    'https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/',
    'capabaseargenmap@EPSG%3A3857@png/', zoom, '/', tiles$x,'/', 2 ^ zoom - 1 - tiles$y, '.png')
  
  pngs <- purrr::map(url, get_tile)
  args <- tile_positions %>% dplyr::mutate(raster = pngs)
  return(purrr::pmap(args, ggplot2::annotation_raster, interpolate = TRUE))
}