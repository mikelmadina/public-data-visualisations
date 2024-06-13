#############################################################
###                         Librerías                     ###
#############################################################

library(here)
library(tidyverse)
library(readODS)
library(svglite)

#############################################################
###              Cargar datos del archivo ODS             ###
#############################################################

publico <-read_ods(here("2024-05_Eurovision", "votos.ods"), sheet = 1) |> 
  pivot_longer(-TO, names_to = "from", values_to = "puntuacion") |> 
  mutate(tipo = "publico")

jurado <- read_ods(here("2024-05_Eurovision", "votos.ods"), sheet = 2) |> 
  pivot_longer(-TO, names_to = "from", values_to = "puntuacion") |> 
  mutate(tipo = "jurado")

mugak <- read_ods(here("2024-05_Eurovision", "votos.ods"), sheet = 3)

#############################################################
###                     Preparar datos                    ###
#############################################################

# Unir votos por tipo

votos <- jurado |> 
  bind_rows(publico)

datos <- votos |> 
  left_join(mugak, by = c( "TO" = "to", "from" = "from")) |> 
  filter(!is.na(puntuacion)) |> 
  mutate(muga = case_when(
    is.na(muga) ~ "ez",
    .default = muga
  ))

# Ordenar países por número de puntos recibidos

orden <- datos |> 
  group_by(TO) |> 
  summarise(puntos = sum(puntuacion)) |> 
  ungroup() |> 
  mutate(TO = fct_reorder(as.factor(TO), puntos))

datos <- datos |> 
  mutate(TO = fct_relevel(as.factor(TO), levels(orden$TO)))

# Clases de variables necesarias para crear andamiaje estructural para los datos

puntuaciones_posibles <- seq(from=-5, to = 17)
paises <- unique(datos$TO)
mugak <- unique(datos$muga)
tipos <- unique(datos$tipo)

# Crear distintos conjuntos de datos con distintos andamiajes

estructura <- crossing(paises, puntuaciones_posibles) |> 
  rename(TO = paises, puntuacion = puntuaciones_posibles)

# A) Datos base agregados a nivel de país y forzando puntuaciones de -5 a 17

datos_ <- datos |> 
  group_by(TO, puntuacion) |> 
  summarise(n = n(),
            posicion = max(posicion)) |>
  full_join(estructura) |> 
  mutate(n = replace_na(n, 0)) |> 
  arrange(TO) |> 
  fill(posicion)

# B) Datos base y tipo de voto

datos_tipo <- datos |> 
  group_by(TO, puntuacion, tipo) |> 
  summarise(n = n(),
            posicion = max(posicion)) |>
  full_join(crossing(estructura, tipos), by = c("TO" = "TO", "puntuacion" = "puntuacion", "tipo" = "tipos")) |> 
  mutate(n = replace_na(n, 0)) |> 
  arrange(TO) |> 
  fill(posicion)


# C) Datos base, tipo de voto y fronteras

datos_tipo_muga <- datos |> 
  group_by(TO, puntuacion, tipo, muga) |> 
  summarise(n = n(),
            posicion = max(posicion)) |>
  full_join(crossing(estructura, tipos, mugak), by = c("TO" = "TO", "puntuacion" = "puntuacion", "tipo" = "tipos", "muga" = "mugak")) |> 
  mutate(n = replace_na(n, 0)) |> 
  arrange(TO) |> 
  fill(posicion)

# D) Puntuaciones agregadas por país, tipo de voto y fronteras

metadata <- datos_tipo_muga |> 
  group_by(TO, tipo, muga) |> 
  summarise(puntos = sum(puntuacion * n))


#############################################################
###                         GRAFICO                       ###
#############################################################

p <- ggplot(datos_tipo_muga, aes(x = puntuacion, y = n)) +
  geom_area(aes(fill = tipo), alpha = 0.75) +
  geom_line(data = datos_, color = "white", linewidth = 1) +
  geom_text(data = metadata |> filter(tipo == "publico"), aes(x = -2, y = 2, label = puntos), hjust = 1, vjust = 0, color = "#1DC1DF", fontface = "bold") +
  geom_text(data = metadata |> filter(tipo == "jurado"), aes(x = 15.5, y = 2, label = puntos), hjust = 1, vjust = 0, color = "#F3DB15", fontface = "bold") +
  facet_grid(cols = vars(fct_rev(muga)), rows = vars(fct_rev(TO))) +
  scale_fill_manual(values = c("#F3DB15", "#1DC1DF", "#FE44E7", "#662CB5", "#FF7323", "#0244FA")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(5, "cm"))

ggsave(here("2024-05_Eurovision", paste0("eurovision_", janitor::make_clean_names(Sys.time()) ,".svg")),
       plot = p,
       device = svglite,
       width = 16.5,
       height = 23.4
)

# NOTA: la composición final se ha realizado con Inkscape