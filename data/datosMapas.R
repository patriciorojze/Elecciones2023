
library(tidyverse)
library(sf)

load(file = "data/MapaDatos.RData")

### Funciones auxiliares ----

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

ConsultaCircuitos = function(Carpeta, Identificador, RemoverArchivo = TRUE,
                             NombrePagina = "https://mapa2.electoral.gov.ar/descargas/"){
  
  Pagina = scan(NombrePagina, what = "c", sep = "\n", quiet = TRUE)
  p = grep(pattern = Identificador, x = Pagina)
  p2 = which(
    grepl(pattern = "href", x = Pagina[p:length(Pagina)]) &
      grepl(pattern = "ShapeFile", x = Pagina[p:length(Pagina)])
  )
  p2 = p2[1]
  
  direccion = Pagina[p:length(Pagina)][p2]
  direccion = substr(
    direccion, 
    start = regexpr(pattern = "href=\"", text = direccion) + 6, 
    stop = nchar(direccion))
  direccion = substr(
    direccion, 
    start = 1, 
    stop = regexpr(pattern = "\"", text = direccion) - 1)
  direccion = paste0("https://mapa2.electoral.gov.ar/", direccion)
  
  CarpetaComp = paste0("data/Mapa/circuitos/", Carpeta) 
  
  if (!Carpeta %in% list.dirs(path = "data/Mapa/circuitos", full.names = FALSE)){
    dir.create(CarpetaComp)
  }
  
  download.file(
    direccion, 
    destfile = paste0(CarpetaComp, "/ArchivoCircuito.zip"), 
    mode = "wb")
  
  exito = FALSE
  
  tryCatch(
    expr = {
      suppressWarnings(
        unzip(paste0(CarpetaComp, "/ArchivoCircuito.zip"), exdir = CarpetaComp))
      cat("Intento 1 OK \n")
      exito = TRUE
    },
    error = function(cond){
      cat("Intento 1 ERROR\n")
    })
  
  if (!exito) tryCatch(
    expr = {
      suppressWarnings(
        untar(paste0(CarpetaComp, "/ArchivoCircuito.zip"), exdir = CarpetaComp))
      cat("Intento 2 OK \n")
      exito = TRUE
    },
    error = function(cond){
      cat("Intento 2 ERROR\n")
    })
  
  if (!exito) stop("Error al extraer los datos de la descarga.")
  
  ArchivoFile = list.files(path = CarpetaComp, pattern = "\\.shp", full.names = TRUE)[1]
  
  r = try(
    sf::read_sf(ArchivoFile, options = "ENCODING=WINDOWS-1252") %>%
      sf::st_transform('+proj=longlat +datum=WGS84') %>%
      #sf::st_transform(crs = 4326) %>%
      suppressWarnings(.))
  
  if (RemoverArchivo) for (arch in list.files(CarpetaComp, full.names = TRUE)){
    file.remove(arch)
  }
  
  return(r)
  
}


### Mapas ----

provincias = sf::read_sf("data/Mapa/ign_provincia/Provincia/ign_provincia.shp") %>%
  st_simplify(dTolerance = 100) %>%
  #sf::st_transform(crs = 4326) %>%
  suppressWarnings(.)

provincias = provincias %>% mutate(Provincia = NAM)

provinciaCodigo = data.frame(
  CodigoProvincia = provincias$IN1, Provincia = provincias$NAM) %>%
  unique()

departamentos = sf::read_sf("data/Mapa/departamentos/departamentos.shp") %>%
  st_simplify(dTolerance = 100) %>%
  #sf::st_transform(crs = 4326) %>%
  suppressWarnings(.)

departamentos = merge(
  departamentos %>% mutate(CodigoProvincia = substr(IN1, 1, 2)),
  provinciaCodigo, by = "CodigoProvincia", all.x = TRUE, )

## CABA

CABA = try(ConsultaCircuitos(
  Carpeta = "CABA", Identificador = "Circuitos CABA"))

if (!"try-error" %in% class(CABA)){
  
  CABA = CABA %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Ciudad Autónoma de Buenos Aires") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  CABA = MapaCircuitos[["Ciudad Autónoma de Buenos Aires"]]
  
}

## Buenos Aires 

BuenosAires = try(ConsultaCircuitos(
  Carpeta = "Buenos Aires", Identificador = "Circuitos de Buenos Aires"))

if (!"try-error" %in% class(BuenosAires)){
  
  BuenosAires = BuenosAires %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Buenos Aires") %>%
    mutate_if(is.character, SacarCeros)
  
  PP = BuenosAires %>% filter(circuito %in% c("354", "356")) %>% sf::st_union()
  
  PP3 = BuenosAires %>% filter(circuito == "354")
  
  st_geometry(PP3) = PP
  
  BuenosAires = rbind.data.frame(
    BuenosAires %>% filter(!circuito %in% c("354", "356")),
    PP3
  )
  
  rm(PP, PP3)
  
  BuenosAires = BuenosAires %>% 
    mutate(departamen = case_when(
      departamen == "A. Gonzales Chaves" ~ "Adolfo Gonzales Chaves",
      departamen == "Bahia Blanca" ~ "Bahía Blanca",
      departamen == "Benito Juarez" ~ "Benito Juárez" ,
      departamen == "Bolivar" ~ "Bolívar",
      departamen == "Ca?uelas" ~ "Cañuelas" ,
      departamen == "Capitan Sarmiento" ~ "Capitán Sarmiento",
      departamen == "Chascomus" ~ "Chascomús",
      departamen == "Cnel. de Marina L.Rosales" ~  "Coronel de Marina Leonardo Rosales",
      departamen == "Colon" ~ "Colón",
      departamen == "Coronel Suarez" ~ "Coronel Suárez" ,
      departamen == "Esteban Echeverria" ~ "Esteban Echeverría" ,
      departamen == "Exaltacion de la Cruz" ~ "Exaltación de la Cruz",
      departamen == "General Lamadrid" ~ "General La Madrid" ,
      departamen == "General Pueyrredon" ~ "General Pueyrredón",
      departamen == "General Rodriguez" ~ "General Rodríguez" ,
      departamen == "General San Martin" ~ "General San Martín" ,
      departamen == "Guamini" ~ "Guaminí",
      departamen == "Hipolito Yrigoyen" ~ "Hipólito Yrigoyen",
      departamen == "Ituzaingo" ~ "Ituzaingó",
      departamen == "Jose C. Paz" ~ "José C. Paz",
      departamen == "Junin" ~ "Junín",
      departamen == "Lanus" ~ "Lanús",
      departamen == "Loberia" ~ "Lobería",
      departamen == "Lujan" ~ "Luján",
      departamen == "Maipu" ~ "Maipú",
      departamen == "Moron" ~ "Morón",
      departamen == "Presidente Peron" ~ "Presidente Perón",
      departamen == "Olavarria" ~ "Olavarría",
      departamen == "Pehuajo" ~ "Pehuajó",
      departamen == "Puan" ~ "Puán",
      departamen == "Roque Perez" ~ "Roque Pérez",
      departamen == "Salliquelo" ~ "Salliqueló",
      departamen == "San Andres de Giles" ~ "San Andrés de Giles",
      departamen == "San Nicolas" ~ "San Nicolás",
      departamen == "Tapalque" ~ "Tapalqué",
      departamen == "Vicente Lopez" ~ "Vicente López",
      departamen == "Zarate" ~ "Zárate",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  BuenosAires = MapaCircuitos[["Buenos Aires"]]
  
}

## Santa Fe

SantaFe = try(ConsultaCircuitos(
  Carpeta = "Santa Fe", Identificador = "Circuitos de Santa Fe"))

if (!"try-error" %in% class(SantaFe)){
  
  SantaFe = SantaFe %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Santa Fe") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Constitucion" ~ "Constitución",
      departamen == "General Lopez" ~ "General López",
      departamen == "San Cristobal" ~ "San Cristóbal" ,
      departamen == "San Jeronimo" ~ "San Jerónimo",
      departamen == "San Martin" ~ "San Martín",
      TRUE ~ departamen)) %>%
    mutate(NAM = substr(circuito, 1, regexpr("\\(ex", circuito) -1)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  SantaFe = MapaCircuitos[["Santa Fe"]]
  
}

## Jujuy

Jujuy = try(ConsultaCircuitos(
  Carpeta = "Jujuy", Identificador = "Circuitos de Jujuy"))

if (!"try-error" %in% class(Jujuy)){

  Jujuy = Jujuy %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Jujuy") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Dr Manuel Belgrano" ~ "Dr. Manuel Belgrano",
      departamen == "Palpala" ~ "Palpalá",
      departamen == "Santa Barbara" ~ "Santa Bárbara",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
    
} else {
  
  Jujuy = MapaCircuitos[["Jujuy"]]
  
}

## Neuquen

Neuquen = try(ConsultaCircuitos(
  Carpeta = "Neuquen", Identificador = "Circuitos de Neuquén"))

if (!"try-error" %in% class(Neuquen)){
  
  Neuquen = Neuquen %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Neuquén") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Alumine" ~ "Aluminé",
      departamen == "Catan Lil" ~ "Catán Lil",
      departamen == "Collon Cura" ~ "Collón Curá",
      departamen == "Lacar" ~ "Lácar",
      departamen == "Loncopue" ~ "Loncopué",
      departamen == "Ñorquin" ~ "Ñorquín",
      departamen == "Picun Leufu" ~ "Picún Leufú",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Neuquen = MapaCircuitos[["Neuquén"]]
  
}

## Cordoba

Cordoba = try(ConsultaCircuitos(
  Carpeta = "Cordoba", Identificador = "Circuitos de Córdoba"))

if (!"try-error" %in% class(Cordoba)){
  
  Cordoba = Cordoba %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Córdoba") %>%
    mutate_if(is.character, SacarCeros)
  
  PP = Cordoba %>% filter(circuito == "en proceso", departamen == "Tulumba") %>% sf::st_union()
  
  PP4 = Cordoba %>% filter(circuito == "en proceso", departamen == "Tulumba")
  PP4 = PP4[1,]
  
  st_geometry(PP4) = PP
  
  Cordoba = rbind.data.frame(
    Cordoba %>% filter(circuito != "en proceso" | departamen != "Tulumba"),
    PP4
  )
  
  rm(PP, PP4)
  
  Cordoba = Cordoba %>%
    mutate(departamen = case_when(
      departamen == "Colon" ~ "Colón",
      departamen == "Cruz Del Eje" ~ "Cruz del Eje",
      departamen == "General San Martin" ~ "General San Martín",
      departamen == "Ischilin" ~ "Ischilín",
      departamen == "Juarez Celman" ~ "Juárez Celman",
      departamen == "Marcos Juarez" ~ "Marcos Juárez" ,
      departamen == "Pres. Roque S. Peña" ~ "Presidente Roque Sáenz Peña",
      departamen == "Rio Cuarto" ~ "Río Cuarto",
      departamen == "Rio Primero" ~ "Río Primero",
      departamen == "Rio Seco" ~ "Río Seco",
      departamen == "Rio Segundo" ~ "Río Segundo",
      departamen == "Santa Maria" ~ "Santa María",
      departamen == "Union" ~ "Unión",
      TRUE ~ departamen)) %>%
    mutate(NAM = ifelse(
      circuito == "en proceso", paste("en proceso de división -", departamen), circuito)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Cordoba = MapaCircuitos[["Córdoba"]]
  
}

## Corrientes

Corrientes = try(ConsultaCircuitos(
  Carpeta = "Corrientes", Identificador = "Circuitos de Corrientes"))

if (!"try-error" %in% class(Corrientes)){
  
  Corrientes = Corrientes %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Corrientes") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(NAM = ifelse(
      circuito == "en proceso", paste("en proceso de división -", departamen), circuito)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Corrientes = MapaCircuitos[["Corrientes"]]
}

## Chaco

Chaco = try(ConsultaCircuitos(
  Carpeta = "Chaco", Identificador = "Circuitos de Chaco"))

if (!"try-error" %in% class(Chaco)){
  
  Chaco = Chaco %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Chaco") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "1º de Mayo" ~ "1ro. de Mayo",
      departamen == "1 de Mayo" ~ "1ro. de Mayo",
      departamen == "Comandante Fernan" ~ "Comandante Fernández",
      departamen == "Fray Justo Santa Maria de Oro" ~ "Fray Justo Santa María de Oro",
      departamen == "General Guemes" ~ "General Güemes",
      departamen == "Libertador General San Martin" ~ "Libertador General San Martín",
      departamen == "Maipu" ~ "Maipú" ,
      departamen == "O Higgins"  ~ "O'Higgins",
      departamen == "Presidencia De La Plaza" ~ "Presidencia de la Plaza",
      departamen == "Primero De Mayo" ~ "1ro. de Mayo",
      departamen == "Tapenaga" ~ "Tapenagá",
      departamen == "Mayor Luis J. Fontana" ~ "Mayor Luis Jorge Fontana",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Chaco = MapaCircuitos[["Chaco"]]
  
}

## Chubut

Chubut = try(ConsultaCircuitos(
  Carpeta = "Chubut", Identificador = "Circuitos de Chubut"))

if (!"try-error" %in% class(Chubut)){
  
  Chubut = Chubut %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Chubut") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Futaleufu" ~ "Futaleufú",
      departamen == "Martires" ~ "Mártires",
      departamen == "Rio Senguer" ~ "Río Senguer",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Chubut = MapaCircuitos[["Chubut"]]
  
}

## Entre Rios

EntreRios = try(ConsultaCircuitos(
  Carpeta = "Entre Rios", Identificador = "Circuitos de Entre Ríos"))

if (!"try-error" %in% class(EntreRios)){
  
  EntreRios = EntreRios %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Entre Ríos") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Colon" ~ "Colón",
      departamen == "Federacion" ~ "Federación",
      departamen == "Gualeguaychu" ~ "Gualeguaychú",
      departamen == "Nogoya" ~ "Nogoyá",
      departamen == "Parana" ~ "Paraná",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  EntreRios = MapaCircuitos[["Entre Ríos"]]
  
}

## La Pampa

LaPampa = try(ConsultaCircuitos(
  Carpeta = "La Pampa", Identificador = "Circuitos de La Pampa"))

if (!"try-error" %in% class(LaPampa)){

  LaPampa = LaPampa %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "La Pampa") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Atreuco" ~ "Atreucó",
      departamen == "Catrilo" ~ "Catriló",
      departamen == "Chapaleufu" ~ "Chapaleufú",
      departamen == "Curaco" ~ "Curacó",
      departamen == "Guatrache" ~ "Guatraché",
      departamen == "Loventue" ~ "Loventué",
      departamen == "Maraco" ~ "Maracó",
      departamen == "Puelen" ~ "Puelén",
      departamen == "Quemu Quemu" ~  "Quemú Quemú",
      departamen == "Realico" ~ "Realicó",
      departamen == "Utracan" ~ "Utracán",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
    
} else {
  
  LaPampa = MapaCircuitos[["La Pampa"]]
  
}

## Mendoza

Mendoza = try(ConsultaCircuitos(
  Carpeta = "Mendoza", Identificador = "Circuitos de Mendoza"))

if (!"try-error" %in% class(Mendoza)){
 
  Mendoza = Mendoza %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Mendoza") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Guaymallen" ~ "Guaymallén",
      departamen == "Junin" ~ "Junín",
      departamen == "Lujan de Cuyo" ~ "Cuyo",
      departamen == "Maipu" ~ "Maipú",
      departamen == "San Martin" ~ "San Martín",
      departamen == "Tunuyan" ~ "Tunuyán",
      TRUE ~ departamen)) %>%
    mutate(NAM = ifelse(
      circuito == "", paste("en proceso de división -", departamen), circuito)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Mendoza = MapaCircuitos[["Mendoza"]]
  
}

## Misiones

Misiones = try(ConsultaCircuitos(
  Carpeta = "Misiones", Identificador = "Circuitos de Misiones"))

if (!"try-error" %in% class(Misiones)){
  
  Misiones = Misiones %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Misiones") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Cainguas" ~ "Cainguás",
      departamen == "El Dorado" ~ "Eldorado",
      departamen == "Guarani" ~ "Guaraní",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Misiones = MapaCircuitos[["Misiones"]]
  
}

## Rio Negro

RioNegro = try(ConsultaCircuitos(
  Carpeta = "Rio Negro", Identificador = "Circuitos de Río Negro"))

if (!"try-error" %in% class(RioNegro)){
  
  RioNegro = RioNegro %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Río Negro") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Gral. Conesa" ~ "Conesa",
      departamen == "San Carlos de Bariloche" ~ "Bariloche",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  RioNegro = MapaCircuitos[["Río Negro"]]
  
}

## Salta

Salta = try(ConsultaCircuitos(
  Carpeta = "Salta", Identificador = "Circuitos de Salta"))

if (!"try-error" %in% class(Salta)){
  
  Salta = Salta %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Salta") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(
      circuito = ifelse(gid == 1256, 75, circuito),
      NAM = ifelse(gid == 1256, "75", NAM)) %>%
    mutate(departamen = case_when(
      departamen == "General Jose de San Martin" ~ "General José de San Martín",
      departamen == "Metan" ~ "Metán",
      departamen == "Oran" ~ "Orán",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Salta = MapaCircuitos[["Salta"]]
  
}

## San Juan

SanJuan = try(ConsultaCircuitos(
  Carpeta = "San Juan", Identificador = "Circuitos de San Juan"))

if (!"try-error" %in% class(SanJuan)){
  
  SanJuan = SanJuan %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "San Juan") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen %in% c("Capiatl D", "Capital A", "Capital B", "Capital C", "Capital E") ~ "Capital",
      departamen == "Iglesias" ~ "Iglesia",
      departamen == "Ullún" ~ "Ullum",
      departamen == "Jáchal" ~ "Jachal",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  SanJuan = MapaCircuitos[["San Juan"]]
  
}

## San Luis

SanLuis = try(ConsultaCircuitos(
  Carpeta = "San Luis", Identificador = "Circuitos de San Luis"))

if (!"try-error" %in% class(SanLuis)){

  SanLuis = SanLuis %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "San Luis") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      cabecera %in% 
        c("Villa de Praga", "Las Aguadas", "La Cocha", "San Martin", "La Vertiente", 
          "Paso Grande") ~ "Libertador General San Martín",
      cabecera %in% 
        c("Justo Daract", "Villa Mercedes", "Juan Jorba", "Lavaisse", "San Jose del Morro", 
          "Juan Llerena", "La Punilla", "San Antonio") ~ "General Pedernera",
      cabecera %in% 
        c("San Luis", "La Punta", "Alto Pelado", "Beazley", "Zanjitas", "Las Chacras", "Juana Koslay", 
          "San Geronimo", "Balde", "Alto Pencoso", "Chosmes", "Desaguadero", "El Calden", "Jarilla", 
          "Las Barrancas", "Potrero de los Funes", "Salinas del Bebedero", "Suyuque", "Varela", 
          "Volcan") ~ "Juan Martín de Pueyrredón",
      cabecera %in% 
        c("La Toma", "Saladillo", "La Florida", "Estancia Grande", "Pampa del Tamboreo", "El Durazno", 
          "Carolina", "El Tarapiche", "Fraga", "Juan W Gez", "Paso del Rey") ~ "Coronel Pringles",
      cabecera %in% 
        c("Merlo", "Talita", "Carpinteria", "Lafinur", "Los Molles", "Santa Rosa") ~ "Junín",
      cabecera %in% 
        c("Quines", "Candelaria", "Leandro N Alem", "Botija", "Chipicu", "El Barrial", "La Cañada", 
          "Lomas Blancas", "Lujan", "Rio Juan Gomez", "San Fransisco") ~ "Ayacucho",
      cabecera %in% 
        c("Nogoli", "Arbol Solo", "Gigante", "La Calera", "Pampa grande", "Represa El Carmen", 
          "Toro Negro", "Villa de la Quebrada", "Villla General Roca") ~ "Belgrano",
      cabecera %in% 
        c("Naschel", "Concaran", "Tilisarao", "San Pablo", "Cortaderas", "Papagayos", "Renca", 
          "Villa del Carmen", "Villa Larca") ~ "Chacabuco",
      cabecera %in% 
        c("Buena Esperanza", "Union", "Bagual", "Batavia", "Fortin El Patria", "Navia", "Fortuna", 
          "Nueva Galia", "Arizona", "Anchorena", "La Verde", "Martin Loyola", "Nahuel Mapa", 
          "Toscal") ~ "Gobernador Dupuy",
      TRUE ~ "")) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  SanLuis = MapaCircuitos[["San Luis"]]
  
}

## Santiago del Estero

SantiagoDelEstero = try(ConsultaCircuitos(
  Carpeta = "Santiago del Estero", Identificador = "Circuitos de Santiago Del Estero"))

if (!"try-error" %in% class(SantiagoDelEstero)){
  
  SantiagoDelEstero = SantiagoDelEstero %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Santiago del Estero") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Brig.Gral. Juan F. Ibarra" ~ "Juan F. Ibarra",
      departamen == "Guasayan" ~ "Guasayán",
      departamen == "Jimenez" ~ "Jiménez",
      departamen == "Rio Hondo" ~ "Río Hondo",
      departamen == "San Martin" ~ "San Martín",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  SantiagoDelEstero = MapaCircuitos[["Santiago del Estero"]]
  
}

## Tucuman

Tucuman = try(ConsultaCircuitos(
  Carpeta = "Tucuman", Identificador = "Circuitos de Tucumán"))

if (!"try-error" %in% class(Tucuman)){
  
  Tucuman = Tucuman %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Tucumán") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Aguilares" ~ "Río Chico",
      departamen == "Bella Vista" ~ "Leales",
      departamen == "Concepción" ~ "Chicligasta",
      departamen == "Curz Alta" ~ "Cruz Alta",
      departamen == "Juan Baiutista Alberdi" ~ "Juan Bautista Alberdi",
      departamen == "San Miguel de Tucumán" ~ "Capital",
      departamen == "Villa Burruyaca" ~ "Burruyacú",
      departamen == "Villa de Trancas" ~ "Trancas",
      departamen == "Yerba Buena - Marcos Paz" ~ "Yerba Buena",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
} else {
  
  Tucuman = MapaCircuitos[["Tucumán"]]
  
}

## Tierra del Fuego

TierraDelFuego = try(ConsultaCircuitos(
  Carpeta = "Tierra del Fuego", Identificador = "Circuitos de Tierra del Fuego"))

if (!"try-error" %in% class(TierraDelFuego)){
  
  TierraDelFuego = TierraDelFuego %>% 
    mutate(
      NAM = circuito, IN1 = 1:n(), 
      Provincia = "Tierra del Fuego, Antártida e Islas del Atlántico Sur") %>%
    mutate_if(is.character, SacarCeros) %>%
    mutate(departamen = case_when(
      departamen == "Antartida Argentina" ~ "Antártida Argentina",
      departamen == "Rio Grande" ~ "Río Grande",
      TRUE ~ departamen)) %>%
    mutate(NAM = paste(NAM, "-", departamen))
  
  
} else {
  
  TierraDelFuego = MapaCircuitos[["Tierra del Fuego, Antártida e Islas del Atlántico Sur"]]
  
}


### Centroides mapas ----

centroides = cbind.data.frame(
  Provincia = provincias$NAM,
  st_coordinates(st_centroid(provincias, of_largest_polygon = TRUE))) %>%
  mutate(
    Y = case_when(
      Provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ -54, 
      Provincia == "Buenos Aires" ~ Y - 0.5, 
      Provincia %in% c("Córdoba", "Mendoza") ~ Y - 0.3, 
      Provincia == "Formosa" ~ Y + 0.3, 
      TRUE ~ Y),
    X = ifelse(Provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur", -67, X),
    zoom = case_when(
      Provincia %in% c("BUenos Aires", "Río Negro", "Chubut", "Santa Fe") ~ 5.5,
      Provincia %in% c("Santa Cruz") ~ 5,
      Provincia %in% c("Tucumán", "Jujuy", "Misiones") ~ 7,
      Provincia %in% c("Chaco", "Formosa", "Corrientes") ~ 6.5,
      Provincia == "Ciudad Autónoma de Buenos Aires" ~ 11,
      TRUE ~ 6))

Caja = function(x){
  
  if (is.null(x)){
    r = 0
  } else {
    coordenadas = sf::st_bbox(x)
    r = max(coordenadas$xmax - coordenadas$xmin, coordenadas$ymax - coordenadas$ymin)
  }
  return(r)
}

centroidesDepto = cbind.data.frame(
  Provincia = departamentos$Provincia,
  Departamento = departamentos$NAM,
  st_coordinates(st_centroid(departamentos)), 
  zoom = ceiling(8 - 3*log10(sapply(departamentos$geometry, Caja)))
)

rm(Caja)

ListaProvincias = c("ARGENTINA", sort(provincias$NAM))

argentina2 = departamentos %>%
  filter(Provincia != "Tierra del Fuego, Antártida e Islas del Atlántico Sur" |
           !NAM %in% c("Islas del Atlántico Sur", "Antártida Argentina"))

bboxArgentina = sf::st_bbox(argentina2)

TierraDelFuego2 = departamentos %>%
  filter(Provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" &
           !NAM %in% c("Islas del Atlántico Sur", "Antártida Argentina"))

bboxTierraDelFuego = sf::st_bbox(TierraDelFuego2)

### Lista Mapa Circuitos ----

MapaCircuitos = list()
MapaCircuitos[["Ciudad Autónoma de Buenos Aires"]] = CABA
rm(CABA)

MapaCircuitos[["Buenos Aires"]] = BuenosAires
rm(BuenosAires)

MapaCircuitos[["Chaco"]] = Chaco
rm(Chaco)

MapaCircuitos[["Chubut"]] = Chubut
rm(Chubut)

MapaCircuitos[["Córdoba"]] = Cordoba
rm(Cordoba)

MapaCircuitos[["Corrientes"]] = Corrientes
rm(Corrientes)

MapaCircuitos[["Entre Ríos"]] = EntreRios
rm(EntreRios)

MapaCircuitos[["Jujuy"]] = Jujuy
rm(Jujuy)

MapaCircuitos[["La Pampa"]] = LaPampa
rm(LaPampa)

MapaCircuitos[["Mendoza"]] = Mendoza
rm(Mendoza)

MapaCircuitos[["Misiones"]] = Misiones
rm(Misiones)

MapaCircuitos[["Neuquén"]] = Neuquen
rm(Neuquen)

MapaCircuitos[["Río Negro"]] = RioNegro
rm(RioNegro)

MapaCircuitos[["Salta"]] = Salta
rm(Salta)

MapaCircuitos[["San Juan"]] = SanJuan
rm(SanJuan)

MapaCircuitos[["San Luis"]] = SanLuis
rm(SanLuis)

MapaCircuitos[["Santa Fe"]] = SantaFe
rm(SantaFe)

MapaCircuitos[["Santiago del Estero"]] = SantiagoDelEstero
rm(SantiagoDelEstero)

MapaCircuitos[["Tierra del Fuego, Antártida e Islas del Atlántico Sur"]] = TierraDelFuego
rm(TierraDelFuego)

MapaCircuitos[["Tucumán"]] = Tucuman
rm(Tucuman)

### Reduccion ----

provincias = provincias %>% select(Provincia, NAM)
departamentos = departamentos %>% select(Provincia, NAM)

for (i in names(MapaCircuitos)){
  MapaCircuitos[[i]] = MapaCircuitos[[i]] %>% select(Provincia, NAM, departamen)
} 

### Guardar ----

rm(argentina2, i, ConsultaCircuitos, TierraDelFuego2)

save(centroides, centroidesDepto, departamentos, MapaCircuitos, provinciaCodigo, 
     provincias, bboxArgentina, bboxTierraDelFuego,
     ListaProvincias, file = "data/MapaDatos.RData")
