
library(tidyverse)

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

## Bajar Datos ----
 
Direcciones = c(
  "https://www.argentina.gob.ar/sites/default/files/dine-resultados/2023-PROVISORIOS_PASO.zip", 
  "https://www.argentina.gob.ar/sites/default/files/2023_generales_1.zip")

Tipo = c("PASO", "Generales")

for (i in 1:2){
  
  cat(Tipo[i], "\n")
  
  try({
    options(timeout = 500)
    download.file(Direcciones[i], destfile = paste0("data/", Tipo[i], ".zip"), mode = "wb")
  })
  
  exito = FALSE
  
  tryCatch(
    expr = {
      suppressWarnings(unzip( 
        paste0("data/", Tipo[i], ".zip"), 
        exdir = "data"))
      cat("Intento 1 OK \n")
      exito = TRUE
    },
    error = function(cond){
      cat("Intento 1 ERROR\n")
    })
  
  if (!exito) tryCatch(
    expr = {
      suppressWarnings(untar(
        paste0("data/", Tipo[i], ".zip"), 
        exdir = "data"))
      cat("Intento 2 OK \n")
      exito = TRUE
    },
    error = function(cond){
      cat("Intento 2 ERROR\n")
    })
  
  
  file.remove(paste0("data/", Tipo[i], ".zip"))
  
}


### Proceso ----

ambitos = read.csv2("data/2023_Generales/AmbitosElectorales_2023_Generales.csv", sep = ",")

file.remove("data/2023_Generales/AmbitosElectorales_2023_Generales.csv")
file.remove("data/2023_PASO/Ambitos_PASO_2023.csv")

listaNombreGral = c(
  "año" = "integer",
  "eleccion_tipo" = "character",
  "eleccion_id" = "integer",
  "recuento_tipo" = "character",
  "recuento_id" = "integer",
  "padron_tipo" = "character",
  "distrito_id" = "integer",
  "distrito_nombre" = "character",
  "seccionprovincial_id" = "integer",
  "seccionprovincial_nombre" = "character",
  "seccion_id" = "integer",
  "seccion_nombre" = "character",          
  "circuito_id" = "integer",
  "circuito_nombre" = "character",
  "mesa_id" = "integer",                 
  "mesa_tipo" = "character",
  "mesa_electores" = "integer",
  "cargo_id" = "integer",                
  "cargo_nombre" = "character",
  "agrupacion_id" = "integer",
  "agrupacion_nombre" = "character",       
  "lista_numero" = "logical",
  "lista_nombre" = "logical",
  "votos_tipo" = "character",              
  "votos_cantidad" = "integer"  
)

listaNombreGralList = list()
listaNombreGralList$character = names(listaNombreGral)[listaNombreGral == "character"]
listaNombreGralList$integer = names(listaNombreGral)[listaNombreGral == "integer"]
listaNombreGralList$logical = names(listaNombreGral)[listaNombreGral == "logical"]


datosGral = data.table::fread(
  file = "data/2023_Generales/ResultadoElectorales_2023_Generales.csv", sep = ",", header = FALSE,
  skip = 1, col.names = names(listaNombreGral), colClasses = as.vector(listaNombreGral)) %>%
  suppressWarnings(.)

datosGral = datosGral %>%
  mutate_if(is.character, SacarCeros) %>%
  group_by(
    año, eleccion_tipo, eleccion_id, recuento_tipo, recuento_id, padron_tipo, distrito_id, 
    distrito_nombre, seccionprovincial_id, seccionprovincial_nombre, seccion_id, seccion_nombre,
    circuito_id, circuito_nombre, cargo_id, cargo_nombre, agrupacion_id, agrupacion_nombre,
    votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% as.data.frame() %>%
  suppressMessages(.)

file.remove("data/2023_Generales/ResultadoElectorales_2023_Generales.csv")

listaNombresPASO = c(
  "año" = "integer", 
  "eleccion_tipo" = "character", 
  "recuento_tipo" = "character",
  "padron_tipo" = "character", 
  "distrito_id" = "integer", 
  "distrito_nombre" = "character",
  "seccionprovincial_id" = "integer", 
  "seccionprovincial_nombre" = "character", 
  "seccion_id" = "integer",
  "seccion_nombre" = "character", 
  "circuito_id" = "character", 
  "circuito_nombre" = "character",
  "mesa_id" = "integer", 
  "mesa_tipo" = "character", 
  "mesa_electores" = "integer",
  "cargo_id" = "integer", 
  "cargo_nombre" = "character", 
  "agrupacion_id" = "integer",  
  "agrupacion_nombre" = "character", 
  "lista_numero" = "integer", 
  "lista_nombre" = "character",
  "votos_tipo" = "character", 
  "votos_cantidad" = "integer")

listaNombresPASOList = list()
listaNombresPASOList$character = names(listaNombresPASO)[listaNombresPASO == "character"]
listaNombresPASOList$integer = names(listaNombresPASO)[listaNombresPASO == "integer"]

datosPASO = data.table::fread(
  "data/2023_PASO/ResultadosElectorales.csv", sep = ',', encoding = "UTF-8", 
  header = FALSE, skip = 1,
  col.names = names(listaNombresPASO), colClasses = as.vector(listaNombresPASO)) %>%
  suppressWarnings(.)

datosPASO = datosPASO %>%
  mutate_if(is.character, SacarCeros) %>%
  group_by(
    año, eleccion_tipo, recuento_tipo, padron_tipo, distrito_id, distrito_nombre, seccionprovincial_id,
    seccionprovincial_nombre, seccion_id, seccion_nombre, circuito_id, circuito_nombre, cargo_id, 
    cargo_nombre, agrupacion_id, agrupacion_nombre, lista_numero, lista_nombre,votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% as.data.frame() %>%
  suppressMessages(.)

file.remove("data/2023_PASO/ResultadosElectorales.csv")

if (!"datosBallotageResumen.csv" %in% list.files(path = "data")){
  source(file = "cambioBases.R", encoding = "UTF-8")
}

datosBallotage = read.csv2(file = "data/datosBallotageResumen.csv", sep = ";") %>%
  mutate_if(is.character, SacarCeros) %>%
  suppressMessages(.)

Colores = read.csv2("data/2023_Generales/Colores_2023.csv", sep = ",")

file.remove("data/2023_Generales/Colores_2023.csv")

rm(Direcciones, exito, i, Tipo, listaNombresPASO, listaNombreGral, 
   listaNombresPASOList, listaNombreGralList)
save(ambitos, datosGral, datosPASO, datosBallotage, Colores, 
     file = "data/ListaDatos.RData")
