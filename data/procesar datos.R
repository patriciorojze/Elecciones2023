

library(tidyverse)

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

## Datos ----

modificacion = file.info("data/datosPrimarios.R")$mtime

Actualizar = FALSE

if (!"ListaDatos.RData" %in% list.files(path = "data")){
  Actualizar = TRUE
} else if (file.info("data/ListaDatos.RData")$mtime < modificacion){
  Actualizar = TRUE
}


if (Actualizar){
  
  cat("Datos Primarios\n")
  try({
    t = proc.time()
    source("data/datosPrimarios.R", encoding = "UTF-8")
    cat("OK actualización ")
    cat(convertirHoras((proc.time() - t)[3]), "\n") 
  })
  
} 

if (!all(c("ambitos", "datosGral", "datosPASO", "datosBallotage", "Colores") %in% list.files())){
  
  load(file = "data/ListaDatos.RData")
  cat("OK carga datos Primarios")
}

## Proceso datos ----

CambioLugares = function(datos) {
  
  datos %>%
    mutate(NAM = case_when(
      grepl("Comuna 0", seccion_nombre) ~ sub("Comuna 0", "Comuna ", seccion_nombre),
      seccion_nombre == "Puan" & distrito_nombre == "Buenos Aires" ~ "Puán",
      seccion_nombre == "Coronel de Marina L. Rosales" & 
        distrito_nombre == "Buenos Aires" ~ "Coronel de Marina Leonardo Rosales",
      seccion_nombre == "Luján de Cuyo" & distrito_nombre == "Mendoza" ~ "Cuyo",
      seccion_nombre == "Jáchal" & distrito_nombre == "San Juan" ~ "Jachal",
      seccion_nombre == "General Ortiz de Ocampo" & distrito_nombre == "La Rioja" ~ "General Ocampo",
      seccion_nombre == "General Juan Facundo Quiroga" & 
        distrito_nombre == "La Rioja" ~ "General Juan F. Quiroga",
      seccion_nombre == "Ángel Vicente Peñaloza" & 
        distrito_nombre == "La Rioja" ~ "General  Angel V. Peñaloza",
      seccion_nombre == "San Blas de los Sauces" & 
        distrito_nombre == "La Rioja" ~ "San Blas de Los Sauces",
      seccion_nombre == "Valle Viejo" & distrito_nombre == "Catamarca" ~ "Valle viejo",
      seccion_nombre == "Silípica" & distrito_nombre == "Santiago del Estero" ~ "Silipica",
      seccion_nombre == "Mayor Luis J. Fontana" & 
        distrito_nombre == "Chaco" ~ "Mayor Luis Jorge Fontana",
      seccion_nombre == "1º de Mayo" & distrito_nombre == "Chaco" ~ "1ro. de Mayo",
      seccion_nombre == "25 de Mayo" & distrito_nombre == "Misiones" ~ "25de Mayo",
      seccion_nombre == "Chical Có" & distrito_nombre == "La Pampa" ~ "Chical Co",
      seccion_nombre == "9 de Julio" & distrito_nombre == "Río Negro" ~ "9 de julio",
      TRUE ~ seccion_nombre
    ))
  
}
 
CambioCircuitos = function(datos){
  
  ParaProcesoCordoba = c(
    "Cruz del Eje", "Ischilín", "Totoral", "Río Seco", "San Justo", "Colón", "Punilla", 
    "Tulumba")
  ParaProcesoCordobaCirc = MapaCircuitos[["Córdoba"]] %>% 
    filter(departamen %in% ParaProcesoCordoba) %>% .$circuito
  
  ParaProcesoMendoza = c(
    "Las Heras", "Santa Rosa", "La Paz", "San Rafael", "General Alvear", "Malargüe")
  ParaProcesoMendozaCirc = MapaCircuitos[["Mendoza"]] %>% 
    filter(departamen %in% ParaProcesoMendoza) %>% .$circuito
  
  r = datos %>%
    mutate(
      circuito_nombre = case_when(
        (distrito_nombre == "Córdoba" & seccion_nombre %in% ParaProcesoCordoba &
          !circuito_id %in% ParaProcesoCordobaCirc) |
        (distrito_nombre == "Mendoza" & seccion_nombre %in% ParaProcesoMendoza &
          !circuito_id %in% ParaProcesoMendozaCirc) ~ paste(
            "en proceso de división -", seccion_nombre),
        distrito_nombre == "Salta" & circuito_id %in% paste0("8", LETTERS[1:3]) ~ "8",
        distrito_nombre == "Salta" & circuito_id %in% paste0("9", LETTERS[1:2]) ~ "9",
        distrito_nombre == "Salta" & circuito_id %in% paste0("20", LETTERS[1:1]) ~ "20",
        distrito_nombre == "Salta" & circuito_id %in% paste0("21", LETTERS[1:2]) ~ "21",
        distrito_nombre == "Salta" & circuito_id %in% paste0("22", LETTERS[1:6]) ~ "22",
        distrito_nombre == "Salta" & circuito_id %in% paste0("23", LETTERS[1:1]) ~ "23",
        distrito_nombre == "Salta" & circuito_id %in% paste0("24", LETTERS[1:6]) ~ "24",
        distrito_nombre == "Salta" & circuito_id %in% paste0("25", LETTERS[1:6]) ~ "25",
        distrito_nombre == "Salta" & circuito_id %in% paste0("26", LETTERS[1:3]) ~ "26",
        distrito_nombre == "Salta" & circuito_id %in% paste0("27", LETTERS[1:4]) ~ "27",
        distrito_nombre == "Salta" & circuito_id %in% paste0("28", LETTERS[1:8]) ~ "28",
        distrito_nombre == "Salta" & circuito_id %in% paste0("29", LETTERS[1:9]) ~ "29",
        distrito_nombre == "Salta" & circuito_id %in% paste0("30", LETTERS[1:2]) ~ "30",
        distrito_nombre == "Salta" & circuito_id %in% paste0("31", LETTERS[1:4]) ~ "31",
        distrito_nombre == "Salta" & circuito_id %in% paste0("32", LETTERS[1:2]) ~ "32",
        distrito_nombre == "Salta" & circuito_id %in% paste0("33", LETTERS[1:3]) ~ "33",
        distrito_nombre == "Salta" & circuito_id %in% paste0("34", LETTERS[1:2]) ~ "34",
        distrito_nombre == "Salta" & circuito_id %in% paste0("35", LETTERS[1:1]) ~ "35",
        distrito_nombre == "Salta" & circuito_id %in% paste0("36", LETTERS[1:2]) ~ "36",
        distrito_nombre == "Salta" & circuito_id %in% paste0("37", LETTERS[1:1]) ~ "37",
        distrito_nombre == "Salta" & circuito_id %in% paste0("38", LETTERS[1:2]) ~ "38",
        distrito_nombre == "Salta" & circuito_id %in% paste0("39", LETTERS[1:1]) ~ "39",
        distrito_nombre == "Salta" & circuito_id %in% paste0("40", LETTERS[1:3]) ~ "40",
        distrito_nombre == "Salta" & circuito_id %in% paste0("41", LETTERS[1:5]) ~ "41",
        distrito_nombre == "Salta" & circuito_id %in% paste0("42", LETTERS[1:3]) ~ "42",
        distrito_nombre == "Salta" & circuito_id %in% paste0("43", LETTERS[1:7]) ~ "43",
        distrito_nombre == "Salta" & circuito_id %in% paste0("44", LETTERS[1:7]) ~ "44",
        distrito_nombre == "Salta" & circuito_id %in% paste0("45", LETTERS[1:1]) ~ "45",
        distrito_nombre == "Salta" & circuito_id %in% paste0("46", LETTERS[1:11]) ~ "46",
        distrito_nombre == "Salta" & circuito_id %in% paste0("47", LETTERS[1:3]) ~ "47",
        distrito_nombre == "Salta" & circuito_id %in% paste0("48", LETTERS[1:4]) ~ "48",
        distrito_nombre == "Salta" & circuito_id %in% paste0("49", LETTERS[1:11]) ~ "49",
        distrito_nombre == "Salta" & circuito_id %in% paste0("50", LETTERS[1:3]) ~ "50",
        distrito_nombre == "Salta" & circuito_id %in% paste0("51", LETTERS[1:6]) ~ "51",
        distrito_nombre == "Salta" & circuito_id %in% paste0("56", LETTERS[1:7]) ~ "56",
        distrito_nombre == "Salta" & circuito_id %in% paste0("57", LETTERS[1:2]) ~ "57",
        distrito_nombre == "Salta" & circuito_id %in% paste0("58", LETTERS[1:1]) ~ "58",
        distrito_nombre == "Salta" & circuito_id %in% paste0("59", LETTERS[1:1]) ~ "59",
        distrito_nombre == "Salta" & circuito_id %in% paste0("60", LETTERS[1:1]) ~ "60",
        distrito_nombre == "Salta" & circuito_id %in% paste0("61", LETTERS[1:1]) ~ "61",
        distrito_nombre == "Salta" & circuito_id %in% paste0("63", LETTERS[1:1]) ~ "63",
        distrito_nombre == "Salta" & circuito_id %in% paste0("64", LETTERS[1:2]) ~ "64",
        distrito_nombre == "Salta" & circuito_id %in% paste0("65", LETTERS[1:2]) ~ "65",
        distrito_nombre == "Salta" & circuito_id %in% paste0("66", LETTERS[1:2]) ~ "66",
        distrito_nombre == "Salta" & circuito_id %in% paste0("67", LETTERS[1:8]) ~ "67",
        distrito_nombre == "Salta" & circuito_id %in% paste0("68", LETTERS[1:2]) ~ "68",
        distrito_nombre == "Salta" & circuito_id %in% paste0("69", LETTERS[1:1]) ~ "69",
        distrito_nombre == "Salta" & circuito_id %in% paste0("70", LETTERS[1:2]) ~ "70",
        distrito_nombre == "Salta" & circuito_id %in% paste0("71", LETTERS[1:2]) ~ "71",
        distrito_nombre == "Salta" & circuito_id %in% paste0("72", LETTERS[1:4]) ~ "72",
        distrito_nombre == "Salta" & circuito_id %in% paste0("73", LETTERS[1:2]) ~ "73",
        distrito_nombre == "Salta" & circuito_id %in% paste0("74", LETTERS[1:1]) ~ "74",
        distrito_nombre == "Salta" & circuito_id %in% paste0("75", LETTERS[1:2]) ~ "75",
        distrito_nombre == "Salta" & circuito_id %in% paste0("76", LETTERS[1:1]) ~ "76",
        distrito_nombre == "Salta" & circuito_id %in% paste0("77", LETTERS[1:6]) ~ "77",
        distrito_nombre == "Salta" & circuito_id %in% paste0("78", LETTERS[1:1]) ~ "78",
        distrito_nombre == "Salta" & circuito_id %in% paste0("79", LETTERS[1:3]) ~ "79",
        distrito_nombre == "Salta" & circuito_id %in% paste0("80", LETTERS[1:1]) ~ "80",
        distrito_nombre == "Salta" & circuito_id %in% paste0("81", LETTERS[1:1]) ~ "81",
        distrito_nombre == "Salta" & circuito_id %in% paste0("82", LETTERS[1:2]) ~ "82",
        distrito_nombre == "Salta" & circuito_id %in% paste0("83", LETTERS[1:3]) ~ "83",
        distrito_nombre == "Salta" & circuito_id %in% paste0("84", LETTERS[1:1]) ~ "84",
        distrito_nombre == "Salta" & circuito_id %in% paste0("85", LETTERS[1:2]) ~ "85",
        distrito_nombre == "Salta" & circuito_id %in% paste0("86", LETTERS[1:1]) ~ "86",
        distrito_nombre == "Salta" & circuito_id %in% paste0("87", LETTERS[1:1]) ~ "87",
        distrito_nombre == "Salta" & circuito_id %in% paste0("88", LETTERS[1:3]) ~ "88",
        distrito_nombre == "Salta" & circuito_id %in% paste0("89", LETTERS[1:5]) ~ "89",
        distrito_nombre == "Salta" & circuito_id %in% paste0("90", LETTERS[1:3]) ~ "90",
        distrito_nombre == "Salta" & circuito_id %in% paste0("91", LETTERS[1:4]) ~ "91",
        distrito_nombre == "Salta" & circuito_id %in% paste0("92", LETTERS[1:1]) ~ "92",
        distrito_nombre == "Salta" & circuito_id %in% paste0("93", LETTERS[1:2]) ~ "93",
        distrito_nombre == "Salta" & circuito_id %in% paste0("94", LETTERS[1:1]) ~ "94",
        distrito_nombre == "Salta" & circuito_id %in% paste0("95", LETTERS[1:1]) ~ "95",
        distrito_nombre == "Salta" & circuito_id %in% paste0("96", LETTERS[1:2]) ~ "96",
        distrito_nombre == "Salta" & circuito_id %in% paste0("97", LETTERS[1:4]) ~ "97",
        distrito_nombre == "Salta" & circuito_id %in% paste0("98", LETTERS[1:1]) ~ "98",
        distrito_nombre == "Salta" & circuito_id %in% paste0("99", LETTERS[1:3]) ~ "99",
        TRUE ~ circuito_nombre)) %>%
    mutate(
      circuito_id = case_when(
        (distrito_nombre == "Córdoba" & seccion_nombre %in% ParaProcesoCordoba &
           !circuito_id %in% ParaProcesoCordobaCirc) |
        (distrito_nombre == "Mendoza" & seccion_nombre %in% ParaProcesoMendoza &
          !circuito_id %in% ParaProcesoMendozaCirc) ~ paste(
               "en proceso de división -", seccion_nombre),
        distrito_nombre == "Salta" & circuito_id %in% paste0("8", LETTERS[1:3]) ~ "8",
        distrito_nombre == "Salta" & circuito_id %in% paste0("9", LETTERS[1:2]) ~ "9",
        distrito_nombre == "Salta" & circuito_id %in% paste0("20", LETTERS[1:1]) ~ "20",
        distrito_nombre == "Salta" & circuito_id %in% paste0("21", LETTERS[1:2]) ~ "21",
        distrito_nombre == "Salta" & circuito_id %in% paste0("22", LETTERS[1:6]) ~ "22",
        distrito_nombre == "Salta" & circuito_id %in% paste0("23", LETTERS[1:1]) ~ "23",
        distrito_nombre == "Salta" & circuito_id %in% paste0("24", LETTERS[1:6]) ~ "24",
        distrito_nombre == "Salta" & circuito_id %in% paste0("25", LETTERS[1:6]) ~ "25",
        distrito_nombre == "Salta" & circuito_id %in% paste0("26", LETTERS[1:3]) ~ "26",
        distrito_nombre == "Salta" & circuito_id %in% paste0("27", LETTERS[1:4]) ~ "27",
        distrito_nombre == "Salta" & circuito_id %in% paste0("28", LETTERS[1:8]) ~ "28",
        distrito_nombre == "Salta" & circuito_id %in% paste0("29", LETTERS[1:9]) ~ "29",
        distrito_nombre == "Salta" & circuito_id %in% paste0("30", LETTERS[1:2]) ~ "30",
        distrito_nombre == "Salta" & circuito_id %in% paste0("31", LETTERS[1:4]) ~ "31",
        distrito_nombre == "Salta" & circuito_id %in% paste0("32", LETTERS[1:2]) ~ "32",
        distrito_nombre == "Salta" & circuito_id %in% paste0("33", LETTERS[1:3]) ~ "33",
        distrito_nombre == "Salta" & circuito_id %in% paste0("34", LETTERS[1:2]) ~ "34",
        distrito_nombre == "Salta" & circuito_id %in% paste0("35", LETTERS[1:1]) ~ "35",
        distrito_nombre == "Salta" & circuito_id %in% paste0("36", LETTERS[1:2]) ~ "36",
        distrito_nombre == "Salta" & circuito_id %in% paste0("37", LETTERS[1:1]) ~ "37",
        distrito_nombre == "Salta" & circuito_id %in% paste0("38", LETTERS[1:2]) ~ "38",
        distrito_nombre == "Salta" & circuito_id %in% paste0("39", LETTERS[1:1]) ~ "39",
        distrito_nombre == "Salta" & circuito_id %in% paste0("40", LETTERS[1:3]) ~ "40",
        distrito_nombre == "Salta" & circuito_id %in% paste0("41", LETTERS[1:5]) ~ "41",
        distrito_nombre == "Salta" & circuito_id %in% paste0("42", LETTERS[1:3]) ~ "42",
        distrito_nombre == "Salta" & circuito_id %in% paste0("43", LETTERS[1:7]) ~ "43",
        distrito_nombre == "Salta" & circuito_id %in% paste0("44", LETTERS[1:7]) ~ "44",
        distrito_nombre == "Salta" & circuito_id %in% paste0("45", LETTERS[1:1]) ~ "45",
        distrito_nombre == "Salta" & circuito_id %in% paste0("46", LETTERS[1:11]) ~ "46",
        distrito_nombre == "Salta" & circuito_id %in% paste0("47", LETTERS[1:3]) ~ "47",
        distrito_nombre == "Salta" & circuito_id %in% paste0("48", LETTERS[1:4]) ~ "48",
        distrito_nombre == "Salta" & circuito_id %in% paste0("49", LETTERS[1:11]) ~ "49",
        distrito_nombre == "Salta" & circuito_id %in% paste0("50", LETTERS[1:3]) ~ "50",
        distrito_nombre == "Salta" & circuito_id %in% paste0("51", LETTERS[1:6]) ~ "51",
        distrito_nombre == "Salta" & circuito_id %in% paste0("56", LETTERS[1:7]) ~ "56",
        distrito_nombre == "Salta" & circuito_id %in% paste0("57", LETTERS[1:2]) ~ "57",
        distrito_nombre == "Salta" & circuito_id %in% paste0("58", LETTERS[1:1]) ~ "58",
        distrito_nombre == "Salta" & circuito_id %in% paste0("59", LETTERS[1:1]) ~ "59",
        distrito_nombre == "Salta" & circuito_id %in% paste0("60", LETTERS[1:1]) ~ "60",
        distrito_nombre == "Salta" & circuito_id %in% paste0("61", LETTERS[1:1]) ~ "61",
        distrito_nombre == "Salta" & circuito_id %in% paste0("63", LETTERS[1:1]) ~ "63",
        distrito_nombre == "Salta" & circuito_id %in% paste0("64", LETTERS[1:2]) ~ "64",
        distrito_nombre == "Salta" & circuito_id %in% paste0("65", LETTERS[1:2]) ~ "65",
        distrito_nombre == "Salta" & circuito_id %in% paste0("66", LETTERS[1:2]) ~ "66",
        distrito_nombre == "Salta" & circuito_id %in% paste0("67", LETTERS[1:8]) ~ "67",
        distrito_nombre == "Salta" & circuito_id %in% paste0("68", LETTERS[1:2]) ~ "68",
        distrito_nombre == "Salta" & circuito_id %in% paste0("69", LETTERS[1:1]) ~ "69",
        distrito_nombre == "Salta" & circuito_id %in% paste0("70", LETTERS[1:2]) ~ "70",
        distrito_nombre == "Salta" & circuito_id %in% paste0("71", LETTERS[1:2]) ~ "71",
        distrito_nombre == "Salta" & circuito_id %in% paste0("72", LETTERS[1:4]) ~ "72",
        distrito_nombre == "Salta" & circuito_id %in% paste0("73", LETTERS[1:2]) ~ "73",
        distrito_nombre == "Salta" & circuito_id %in% paste0("74", LETTERS[1:1]) ~ "74",
        distrito_nombre == "Salta" & circuito_id %in% paste0("75", LETTERS[1:2]) ~ "75",
        distrito_nombre == "Salta" & circuito_id %in% paste0("76", LETTERS[1:1]) ~ "76",
        distrito_nombre == "Salta" & circuito_id %in% paste0("77", LETTERS[1:6]) ~ "77",
        distrito_nombre == "Salta" & circuito_id %in% paste0("78", LETTERS[1:1]) ~ "78",
        distrito_nombre == "Salta" & circuito_id %in% paste0("79", LETTERS[1:3]) ~ "79",
        distrito_nombre == "Salta" & circuito_id %in% paste0("80", LETTERS[1:1]) ~ "80",
        distrito_nombre == "Salta" & circuito_id %in% paste0("81", LETTERS[1:1]) ~ "81",
        distrito_nombre == "Salta" & circuito_id %in% paste0("82", LETTERS[1:2]) ~ "82",
        distrito_nombre == "Salta" & circuito_id %in% paste0("83", LETTERS[1:3]) ~ "83",
        distrito_nombre == "Salta" & circuito_id %in% paste0("84", LETTERS[1:1]) ~ "84",
        distrito_nombre == "Salta" & circuito_id %in% paste0("85", LETTERS[1:2]) ~ "85",
        distrito_nombre == "Salta" & circuito_id %in% paste0("86", LETTERS[1:1]) ~ "86",
        distrito_nombre == "Salta" & circuito_id %in% paste0("87", LETTERS[1:1]) ~ "87",
        distrito_nombre == "Salta" & circuito_id %in% paste0("88", LETTERS[1:3]) ~ "88",
        distrito_nombre == "Salta" & circuito_id %in% paste0("89", LETTERS[1:5]) ~ "89",
        distrito_nombre == "Salta" & circuito_id %in% paste0("90", LETTERS[1:3]) ~ "90",
        distrito_nombre == "Salta" & circuito_id %in% paste0("91", LETTERS[1:4]) ~ "91",
        distrito_nombre == "Salta" & circuito_id %in% paste0("92", LETTERS[1:1]) ~ "92",
        distrito_nombre == "Salta" & circuito_id %in% paste0("93", LETTERS[1:2]) ~ "93",
        distrito_nombre == "Salta" & circuito_id %in% paste0("94", LETTERS[1:1]) ~ "94",
        distrito_nombre == "Salta" & circuito_id %in% paste0("95", LETTERS[1:1]) ~ "95",
        distrito_nombre == "Salta" & circuito_id %in% paste0("96", LETTERS[1:2]) ~ "96",
        distrito_nombre == "Salta" & circuito_id %in% paste0("97", LETTERS[1:4]) ~ "97",
        distrito_nombre == "Salta" & circuito_id %in% paste0("98", LETTERS[1:1]) ~ "98",
        distrito_nombre == "Salta" & circuito_id %in% paste0("99", LETTERS[1:3]) ~ "99",
        TRUE ~ circuito_id))
  
  return(r)
  
}

CalculoPuestos = function(datos) {
  
  datos %>%
    group_by(seccion_nombre, distrito_nombre, cargo_nombre) %>%
    mutate(
      TOTAL = sum(votos_cantidad),
      PorcPositivos = ifelse(votos_tipo == "POSITIVO", votos_cantidad, NA) / 
        sum(ifelse(votos_tipo == "POSITIVO", votos_cantidad, 0)),
      PorcTotal = votos_cantidad / sum(votos_cantidad)) %>%
    mutate(Puesto = rank(-PorcPositivos, na.last = TRUE, ties.method = "first")) %>%
    mutate(Puesto = ifelse(votos_tipo == "POSITIVO", Puesto, NA)) %>%
    ungroup()
  
}

# ListaCargos = c(
#   "PRESIDENTE Y VICE", "SENADOR NACIONAL", "DIPUTADO NACIONAL", "GOBERNADOR Y VICE", 
#   "JEFE/A DE GOBIERNO", "PARLAMENTO MERCOSUR NACIONAL", "PARLAMENTO MERCOSUR REGIONAL")

ListaCargos = c("PRESIDENTE Y VICE", "GOBERNADOR Y VICE", "JEFE/A DE GOBIERNO")

ListaProvinciasCircuito = c(
  "Ciudad Autónoma de Buenos Aires", "Buenos Aires", "Santa Fe", "Jujuy", "Neuquén", "Córdoba", 
  "Corrientes", "Chaco", "Chubut", "Entre Ríos", "La Pampa", "Mendoza", "Misiones", "Río Negro",
  "Salta", "San Juan", "San Luis", "Santiago del Estero", "Tucumán", "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
)

names(ListaProvinciasCircuito) = ListaProvinciasCircuito
names(ListaProvinciasCircuito)[ListaProvinciasCircuito == "Río Negro"] = "Río Negro (parcial)"
names(ListaProvinciasCircuito)[ListaProvinciasCircuito == "San Luis"] = "San Luis (parcial)"

datosPorDepto = datosGral %>% 
  filter(cargo_nombre %in% ListaCargos) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre,agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  CambioLugares(.) %>% 
  suppressMessages(.)

datosPorProvincia = datosGral %>%
  filter(cargo_nombre %in% ListaCargos) %>%
  mutate(seccion_id = distrito_id, seccion_nombre = distrito_nombre) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre, agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  mutate(NAM = seccion_nombre) %>% 
  suppressMessages(.)

datosPorCircuito = datosGral %>%
  filter(
    cargo_nombre %in% ListaCargos, 
    distrito_nombre %in% ListaProvinciasCircuito) %>%
  CambioCircuitos(.) %>% CambioLugares(.) %>%
  mutate(
    seccion_id = circuito_id, 
    seccion_nombre = paste(circuito_nombre, "-", NAM)) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre, agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  mutate(NAM = seccion_nombre) %>% 
  suppressMessages(.)

datosPorDeptoPASO = datosPASO %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  filter(cargo_nombre %in% ListaCargos) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre,agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  CambioLugares(.) %>% 
  suppressMessages(.)

datosPorProvinciaPASO = datosPASO %>%
  mutate(seccion_id = distrito_id, seccion_nombre = distrito_nombre) %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  filter(cargo_nombre %in% ListaCargos) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre, agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)

datosPorCircuitoPASO = datosPASO %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  filter(
    cargo_nombre %in% ListaCargos, 
    distrito_nombre %in% ListaProvinciasCircuito) %>% 
  CambioCircuitos(.) %>% CambioLugares(.) %>%
  mutate(
    seccion_id = circuito_id, 
    seccion_nombre = paste(circuito_nombre, "-", NAM)) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(cargo_nombre, distrito_nombre, seccion_nombre, agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)


datosPorDeptoInterna = datosPASO %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  rename(Interna = agrupacion_nombre, Interna_id = agrupacion_id) %>%
  rename(agrupacion_id = lista_numero, agrupacion_nombre = lista_nombre) %>%
  filter(cargo_nombre %in% ListaCargos) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>% 
  group_by(seccion_nombre, distrito_nombre, cargo_nombre, Interna_id) %>%
  mutate(
    TOTAL_Interna = sum(votos_cantidad),
    PorcPositivos_Interna = ifelse(votos_tipo == "POSITIVO", votos_cantidad, NA) / 
      sum(ifelse(votos_tipo == "POSITIVO", votos_cantidad, 0)),
    PorcTotal_Interna = votos_cantidad / sum(votos_cantidad)) %>%
  mutate(Puesto_Interna = rank(-PorcPositivos_Interna, na.last = TRUE, ties.method = "first")) %>%
  mutate(Puesto_Interna = ifelse(votos_tipo == "POSITIVO", Puesto_Interna, NA)) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  CambioLugares(.) %>% 
  suppressMessages(.)

datosPorProvinciaInterna = datosPASO %>%
  mutate(seccion_id = distrito_id, seccion_nombre = distrito_nombre) %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  rename(Interna = agrupacion_nombre, Interna_id = agrupacion_id) %>%
  rename(agrupacion_id = lista_numero, agrupacion_nombre = lista_nombre) %>%
  filter(cargo_nombre %in% ListaCargos) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>% 
  group_by(seccion_nombre, distrito_nombre, cargo_nombre, Interna_id) %>%
  mutate(
    TOTAL_Interna = sum(votos_cantidad),
    PorcPositivos_Interna = ifelse(votos_tipo == "POSITIVO", votos_cantidad, NA) / 
      sum(ifelse(votos_tipo == "POSITIVO", votos_cantidad, 0)),
    PorcTotal_Interna = votos_cantidad / sum(votos_cantidad)) %>%
  mutate(Puesto_Interna = rank(-PorcPositivos_Interna, na.last = TRUE, ties.method = "first")) %>%
  mutate(Puesto_Interna = ifelse(votos_tipo == "POSITIVO", Puesto_Interna, NA)) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)


datosPorCircuitoInterna = datosPASO %>%
  mutate(cargo_nombre = case_when(
    cargo_nombre == "PRESIDENTE/A" ~ "PRESIDENTE Y VICE",
    cargo_nombre == "SENADORES/AS NACIONALES" ~ "SENADOR NACIONAL",
    cargo_nombre == "DIPUTADOS/AS NACIONALES" ~ "DIPUTADO NACIONAL",
    cargo_nombre == "SENADORES/AS PROVINCIALES" ~ "SENADOR PROVINCIAL",
    cargo_nombre == "DIPUTADOS/AS PROVINCIALES" ~ "DIPUTADO PROVINCIAL",
    cargo_nombre == "GOBERNADOR/A" ~ "GOBERNADOR Y VICE",
    cargo_nombre == "INTENDENTE/A" ~ "INTENDENTE",
    TRUE ~ cargo_nombre)) %>%
  rename(Interna = agrupacion_nombre, Interna_id = agrupacion_id) %>%
  rename(agrupacion_id = lista_numero, agrupacion_nombre = lista_nombre) %>%
  filter(
    cargo_nombre %in% ListaCargos, 
    distrito_nombre %in% ListaProvinciasCircuito) %>%
  CambioCircuitos(.) %>% CambioLugares(.) %>%
  mutate(
    seccion_id = circuito_id, 
    seccion_nombre = paste(circuito_nombre, "-", NAM)) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, Interna_id, Interna, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>% 
  group_by(seccion_nombre, distrito_nombre, cargo_nombre, Interna_id) %>%
  mutate(
    TOTAL_Interna = sum(votos_cantidad),
    PorcPositivos_Interna = ifelse(votos_tipo == "POSITIVO", votos_cantidad, NA) / 
      sum(ifelse(votos_tipo == "POSITIVO", votos_cantidad, 0)),
    PorcTotal_Interna = votos_cantidad / sum(votos_cantidad)) %>%
  mutate(Puesto_Interna = rank(-PorcPositivos_Interna, na.last = TRUE, ties.method = "first")) %>%
  mutate(Puesto_Interna = ifelse(votos_tipo == "POSITIVO", Puesto_Interna, NA)) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)


ListaConInterna = datosPorDeptoInterna %>%
  filter(Puesto_Interna > 1) %>%
  select(distrito_nombre, Interna_id, cargo_nombre) %>%
  unique() %>% mutate(TieneInterna = TRUE) %>% 
  suppressMessages(.)


datosPorDeptoInterna = merge(datosPorDeptoInterna, ListaConInterna, all.x = TRUE) %>%
  mutate(TieneInterna = ifelse(is.na(TieneInterna), FALSE, TieneInterna)) %>% 
  suppressMessages(.)


datosPorDeptoBallotage = datosBallotage %>%
  mutate(cargo_nombre = "PRESIDENTE Y VICE") %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  CambioLugares(.) %>% 
  suppressMessages(.)

datosPorProvinciaBallotage = datosBallotage %>%
  mutate(seccion_id = distrito_id, seccion_nombre = distrito_nombre) %>%
  mutate(cargo_nombre = "PRESIDENTE Y VICE") %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)


datosPorCircuitoBallotage = datosBallotage %>%
  mutate(cargo_nombre = "PRESIDENTE Y VICE") %>%
  filter(
    cargo_nombre %in% ListaCargos, 
    distrito_nombre %in% ListaProvinciasCircuito) %>%
  CambioCircuitos(.) %>% CambioLugares(.) %>%
  mutate(
    seccion_id = circuito_id, 
    seccion_nombre = paste(circuito_nombre, "-", NAM)) %>%
  # group_by(
  #   año, cargo_id, cargo_nombre, distrito_id, distrito_nombre, seccion_id, seccion_nombre, 
  #   agrupacion_id, agrupacion_nombre, votos_tipo) %>%
  group_by(
    cargo_nombre, distrito_nombre, seccion_nombre,
    agrupacion_nombre, votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% CalculoPuestos(.) %>%
  mutate_if(is.numeric, ~round(.x, digits = 6)) %>%
  mutate(NAM = seccion_nombre) %>% 
  mutate_at(c("cargo_nombre", "distrito_nombre", "votos_tipo"), as.factor) %>%
  suppressMessages(.)

## Proceso Colores ----

listaColores = colors()

col2HEX = function(x){
  rgb(t(col2rgb(x))/256)
}

listaHEX = col2HEX(listaColores)
names(listaHEX) = listaColores

names(listaColores) = listaHEX

ColorCercano = function(x){
  coloresHEX = t(sapply(names(listaColores), function(x)
    t(col2rgb(x))
  )) 
  
  colorComp = try(col2rgb(x))
  
  if ("try-error" %in% class(colorComp)){
    r = ""
  } else {
    for (i in 1:3) coloresHEX[,i] = coloresHEX[,i] - colorComp[i]
    
    a = which.min(apply(coloresHEX, 1, function(x) sum(x^2)))
    
    r = listaColores[a]
  }
  
  return(r)
}

ColoresPartido = Colores %>%
  arrange(distrito_id, agrupacion_id) %>%
  group_by(agrupacion_nombre) %>%
  summarise(agrupacion_color = agrupacion_color[1]) %>%
  ungroup() %>% 
  suppressMessages(.)

ColoresPartido = rbind.data.frame(
  ColoresPartido,
  list("CAMBIA JUJUY", "#FEDD00"),
  list("JUNTOS POR EL CAMBIO CHUBUT", "#FEDD00"),
  list("JUNTOS POR EL CAMBIO LA PAMPA", "#FEDD00"),
  list("JUNTOS POR ENTRE RIOS", "#FEDD00"),
  list("CAMBIA MENDOZA", "#FEDD00"),
  list("FRENTE JUNTOS POR EL CAMBIO", "#FEDD00"),
  list("JUNTOS POR EL CAMBIO TDF", "#FEDD00"),
  list("FTE DE IZQ. Y DE TRABAJADORES - UNIDAD", "#F95461"),
  list("A- LA FUERZA DEL CAMBIO", "#44e42a"),
  list("JUNTOS POR EL CAMBIO (A- LA FUERZA DEL CAMBIO)", "#44e42a"),
  list("JUNTOS POR EL CAMBIO (B- EL CAMBIO DE NUESTRAS VIDAS)","#ea9999"),
  list("B- EL CAMBIO DE NUESTRAS VIDAS", "#ea9999"),
  list("FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD (A- UNIR Y FORTALECER LA IZQUIERDA)", "#449600"),
  list("A- UNIR Y FORTALECER LA IZQUIERDA", "#449600"),
  list("FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD (B- UNIDAD DE LUCHADORES Y LA IZQUIERDA)","#e51c12"),
  list("B- UNIDAD DE LUCHADORES Y LA IZQUIERDA","#e51c12"),
  list("A- CELESTE Y BLANCA", "#009CDE"),
  list("UNION POR LA PATRIA (B- JUSTA Y SOBERANA)", "#b28d32"),
  list("B- JUSTA Y SOBERANA", "#b28d32")
)

PartidosSinInterna = datosPorDeptoInterna %>%
  filter(!TieneInterna) %>%
  select(agrupacion_nombre, Interna) %>%
  mutate(agrupacion_nombre = paste0(Interna, " (", agrupacion_nombre, ")")) %>%
  unique() %>% 
  suppressMessages(.)

PartidosSinInterna = merge(
  PartidosSinInterna, ColoresPartido, by.x = "Interna", by.y = "agrupacion_nombre", all.x = TRUE) %>%
  select(-c(Interna)) %>%
  filter(!is.na(agrupacion_color)) %>% 
  suppressMessages(.)

ColoresPartido = rbind.data.frame(ColoresPartido, PartidosSinInterna)

PartidosFaltantes = c(
  datosPorDepto$agrupacion_nombre, datosPorDeptoPASO$agrupacion_nombre, 
  datosPorDeptoInterna$agrupacion_nombre, 
  paste0(datosPorDeptoInterna$Interna, " (", datosPorDeptoInterna$agrupacion_nombre, ")"))

PartidosFaltantes = PartidosFaltantes %>% unique()
PartidosFaltantes = PartidosFaltantes[!PartidosFaltantes %in% c("", " ()")]
PartidosFaltantes = PartidosFaltantes[!PartidosFaltantes %in% ColoresPartido$agrupacion_nombre]

ColoresNuevo = rgb(
  red = runif(n = length(PartidosFaltantes)),
  green = runif(n = length(PartidosFaltantes)),
  blue = runif(n = length(PartidosFaltantes)), 
  alpha = 1)

a = 1

while (any(ColoresNuevo %in% ColoresPartido$agrupacion_color) & a < 100) {
  
  ColoresNuevo = rgb(
    red = runif(n = length(PartidosFaltantes)),
    green = runif(n = length(PartidosFaltantes)),
    blue = runif(n = length(PartidosFaltantes)), 
    alpha = 1)
  
  a = a + 1
}

ColoresPartido = rbind.data.frame(
  ColoresPartido,
  list(PartidosFaltantes, ColoresNuevo)
)

rm(PartidosFaltantes, ColoresNuevo, a)

## Listas ----

ListaPartidos = datosPorDepto %>%
  filter(cargo_nombre == "PRESIDENTE Y VICE") %>%
  .$agrupacion_nombre %>% unique(.)

ListaPartidos = ListaPartidos[ListaPartidos != ""]

ListaPartidos = c("Puesto", paste("Porcentaje de", sort(ListaPartidos)))

ListaPaso = datosPorDeptoInterna %>% 
  filter(Puesto_Interna > 1, cargo_nombre == "PRESIDENTE Y VICE") %>% 
  .$Interna %>% unique() %>% sort()

ListaPaso = c(
  "Resultado por agrupación", "Resultado por lista",
  paste("Interna de", ListaPaso))

## Guardar ----

rm(ambitos, Colores, datosBallotage, datosGral, datosPASO, ListaConInterna, PartidosSinInterna, 
   CalculoPuestos, CambioLugares, CambioCircuitos)

save(datosPorCircuito, datosPorCircuitoBallotage, datosPorCircuitoInterna, datosPorCircuitoPASO,
     datosPorDepto, datosPorDeptoBallotage, datosPorDeptoInterna, datosPorDeptoPASO, 
     datosPorProvincia, datosPorProvinciaBallotage, datosPorProvinciaInterna, datosPorProvinciaPASO,
     ColoresPartido, ListaCargos, ListaPartidos, ListaPaso, ListaProvinciasCircuito,
     listaColores, listaHEX, ColorCercano, col2HEX,
     file = "data/DatosProcesados.RData")
