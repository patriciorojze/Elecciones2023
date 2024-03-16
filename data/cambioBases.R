

library(tidyverse)

datosBallotage = read.csv2(file = "datos/presentacionDeResultados (2).csv", sep = ",") %>%
  mutate_if(is.character, SacarCeros)

datosBallotage2 = datosBallotage %>%
  group_by(
    año,eleccion_tipo,eleccion_id,recuento_id,recuento_tipo,padron_tipo,distrito_id,distrito_nombre,seccion_id,seccion_nombre,circuito_id,
    circuito_nombre, cargo_id,cargo_nombre,agrupacion_id,agrupacion_nombre,votos_tipo) %>%
  summarise(votos_cantidad = sum(votos_cantidad, na.rm = TRUE)) %>%
  ungroup() %>% as.data.frame()

write.csv2(datosBallotage2, file = "datos/datosBallotageResumen.csv", row.names = FALSE)

rm(datosBallotage, datosBallotage2)