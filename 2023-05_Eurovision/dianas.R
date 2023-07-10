library(here)
library(tidyverse)
library(readODS)
library(geosphere)
library(classInt)
library(ggforce)
library(ggflags) # devtools::install_github("jimjam-slam/ggflags")
library(ggnewscale)
library(patchwork)
library(systemfonts)



# EU: datuak ekarri eta prestatu
# ES: cargar y preparar datos
# EN: load and prepare data

theme_eurovision <- function() {
   theme_void() +
   theme(text = element_text(family = "Poppins"),
         legend.position = "none",
         plot.margin=unit(c(0,2,0,2), 'cm'),
         plot.background = element_rect(fill = "#02025e", color = NA),
         strip.text = element_text(hjust = 0.1, color = "white"),
         panel.spacing.x = unit(0, "cm"))
}

votos_publico <- readODS::read_ods("votaciones.ods", sheet = 1)
votos_jurado <- readODS::read_ods("votaciones.ods", sheet = 2)

votos <- votos_publico %>%
   dplyr::bind_rows(votos_jurado) %>%
   # dplyr::select(pos_jurado, tidyselect::everything()) %>%
   tidyr::pivot_longer(
      cols = Albania:`Rest of the world`,
      names_to = "from_country",
      values_to = "puntos"
   ) %>% 
   filter(!is.na(puntos)) %>% 
   pivot_wider(names_from = tipo,
               values_from = puntos)


## Datos de las capitales (para iso2 con ggflag)
### Datos descargados de: https://simplemaps.com/data/world-cities

ciudades <- readr::read_csv(here::here("worldcities.csv"))

# Filtramos los datos para quedarnos con las capitales

capitales <- ciudades %>%
   filter(capital == "primary") %>%
   select(country, city, lat, lng, iso2, iso3)

# No todos los países del mundo participan en Eurovisión.
# Obtenemos la lista de los que pueden votar
# NOTA: obtendremos también un país "Rest of the world"

paises_votantes <- votos %>%
   group_by(from_country) %>%
   summarise()


paises_vot_geo <- paises_votantes %>%
   left_join(capitales, by = c("from_country" = "country")) %>%
   # En el dataset de ciudades hay dos cappitales para Países Bajos, y no hay datos para República Checa
   filter(city != "The Hague") %>%
   arrange(from_country) %>%
   # Se añaden datos por defecto para "Rest of the world, que no existe como país
   add_row(
      from_country = "Rest of the world",
      lat = 0,
      lng = 0,
      iso2 = "RW",
      iso3 = "RoW"
   ) %>%
   mutate(id = row_number())

# Añadimos los datos de geolocalización al conjunto de datos con los votos

votos_geo <- votos %>%
   left_join(paises_vot_geo, by = c("from_country" = "from_country")) %>%
   rename(from = id) %>%
   left_join(paises_vot_geo, by = c("to_country" = "from_country")) %>%
   rename(to = id) %>%
   #select(from, to, tipo, puntos, lat.x, lng.x, iso3.x) %>%
   filter(!is.na(from) | !is.na(to))

# https://stackoverflow.com/a/67414595

votos_dist <- votos_geo %>%
   mutate(
      distancia = pmap(
         list(
            a = lng.x,
            b = lat.x,
            c = lng.y,
            d = lat.y
         ),
         ~ geosphere::distm(
            x = c(..1, ..2),
            y = c(..3, ..4),
            fun = distGeo
         )
      ),
      dist_km = unlist(distancia) / 1000,
      grupo_dist = classInt::classify_intervals(dist_km, 5, style = "jenks")
   ) %>% 
   select(-distancia)
   
datos <- votos_dist %>%
   replace_na(list(jurado = 0, publico = 0)) %>% 
   group_by(to_country, iso2.y, grupo_dist) %>%
   summarise(voto_tele = sum(publico),
             voto_jurado = sum(jurado)) %>% 
   ungroup() %>% 
   group_by(to_country) %>% 
   mutate(porcentaje_voto_tele = voto_tele / sum(voto_tele),
          porcentaje_voto_jurado = voto_jurado / sum(voto_jurado),
          total_tele = sum(voto_tele),
          total_jurado = sum(voto_jurado),
          total = sum(voto_jurado) + sum(voto_tele),
          grupo_jurado = case_when(
             porcentaje_voto_jurado < 0.333 ~ "A",
             porcentaje_voto_jurado < 0.666 ~ "B",
             porcentaje_voto_jurado <= 1 ~ "C",
             TRUE ~ NA
          ),
          grupo_tele = case_when(
             porcentaje_voto_tele < 0.333 ~ "A",
             porcentaje_voto_tele < 0.666 ~ "B",
             porcentaje_voto_tele <= 1 ~ "C",
             TRUE ~ NA
          ),
          index = row_number()
   ) %>%
   pivot_longer(cols = starts_with("voto"),
                names_to = "tipo",
                values_to = "puntos")

# plot principal

dianas_paises <- ggplot(datos) +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[7010.879,18058.67]" & tipo == "voto_jurado" & puntos > 0), aes(x0 = 0, y0 = 0, r = 1.2, fill = grupo_jurado), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[4075.897,7010.879)" & tipo == "voto_jurado" & puntos > 0), aes(x0 = 0, y0 = 0, r = 1, fill = grupo_jurado), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[2306.628,4075.897)" & tipo == "voto_jurado" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.80, fill = grupo_jurado), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[1226.087,2306.628)" & tipo == "voto_jurado" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.60, fill = grupo_jurado), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[82.43844,1226.087)" & tipo == "voto_jurado" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.40, fill = grupo_jurado), color = "#0043ffff") +
   scale_fill_manual(values = c("C" = "#fff800", "B" = "#f8ed7a", "A" = "#eae3bd")) +
   ggnewscale::new_scale_fill() +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[7010.879,18058.67]" & tipo == "voto_tele" & puntos > 0), aes(x0 = 0, y0 = 0, r = 1.2, fill = grupo_tele), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[4075.897,7010.879)" & tipo == "voto_tele" & puntos > 0), aes(x0 = 0, y0 = 0, r = 1, fill = grupo_tele), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[2306.628,4075.897)" & tipo == "voto_tele" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.80, fill = grupo_tele), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[1226.087,2306.628)" & tipo == "voto_tele" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.60, fill = grupo_tele), color = "#0043ffff") +
   ggforce::geom_circle(data = . %>% filter(grupo_dist == "[82.43844,1226.087)" & tipo == "voto_tele" & puntos > 0), aes(x0 = 0, y0 = 0, r = 0.40, fill = grupo_tele), color = "#0043ffff") +
   ggflags::geom_flag(aes(x = 0, y = 0, country = tolower(iso2.y)), size = 6) +
   geom_text(data = . %>% filter(tipo == "voto_jurado" & index == 1), aes(x = -1.1, y = 1.1, label = total), color = "white", fontface = "bold", size = 5, family = "Poppins") +
   geom_text(data = . %>% filter(tipo == "voto_jurado" & index == 1), aes(x = 1.1, y = -1.1, label = total_jurado), color = "#fff800", fontface = "bold", family = "Poppins") + 
   geom_text(data = . %>% filter(tipo == "voto_tele" & index == 1), aes(x = 1.1, y = -1.1, label = total_tele), color = "#f80087", fontface = "bold", family = "Poppins") + 
   geom_vline(data = . %>% filter(tipo == "voto_jurado"), aes(xintercept = -Inf), color = "white") +
   geom_vline(data = . %>% filter(tipo == "voto_tele"), aes(xintercept = Inf), color = "white") +
   scale_fill_manual(values = c("C" = "#f80087", "B" = "#ff89b7", "A" = "#ffd5e4")) +
   facet_wrap(vars(forcats::fct_reorder(to_country, desc(total)), tipo), ncol = 8) +
   scale_x_continuous(limits = c(-1.5, 1.5)) +
   scale_y_continuous(limits = c(-1.5, 1.5)) +
   coord_equal() +
   theme_eurovision()


dianas_suecia <- ggplot(data = datos %>% filter(to_country == "Sweden")) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[7010.879,18058.67]" &
            tipo == "voto_jurado" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 1.2,
         fill = grupo_jurado
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[4075.897,7010.879)" &
            tipo == "voto_jurado" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 1,
         fill = grupo_jurado
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[2306.628,4075.897)" &
            tipo == "voto_jurado" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.80,
         fill = grupo_jurado
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[1226.087,2306.628)" &
            tipo == "voto_jurado" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.60,
         fill = grupo_jurado
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[82.43844,1226.087)" &
            tipo == "voto_jurado" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.40,
         fill = grupo_jurado
      ),
      color = "black"
   ) +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.2),
                        fill = "grey50",
                        color = "black") +
   scale_fill_manual(values = c(
      "C" = "#fff800",
      "B" = "#f8ed7a",
      "A" = "#eae3bd"
   )) +
   ggnewscale::new_scale_fill() +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[7010.879,18058.67]" &
            tipo == "voto_tele" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 1.2,
         fill = grupo_tele
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[4075.897,7010.879)" &
            tipo == "voto_tele" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 1,
         fill = grupo_tele
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[2306.628,4075.897)" &
            tipo == "voto_tele" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.80,
         fill = grupo_tele
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[1226.087,2306.628)" &
            tipo == "voto_tele" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.60,
         fill = grupo_tele
      ),
      color = "black"
   ) +
   ggforce::geom_circle(
      data = . %>% filter(
         grupo_dist == "[82.43844,1226.087)" &
            tipo == "voto_tele" &
            puntos > 0
      ),
      aes(
         x0 = 0,
         y0 = 0,
         r = 0.40,
         fill = grupo_tele
      ),
      color = "black"
   ) +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.2),
                        fill = "grey50",
                        color = "black") +
   ggflags::geom_flag(aes(
      x = 0,
      y = 0,
      country = tolower(iso2.y)
   ), size = 35) +
   geom_text(
      data = . %>% filter(tipo == "voto_jurado" & index == 1),
      aes(x = -1.1, y = 1.1, label = total),
      color = "white",
      fontface = "bold",
      size = 5
   ) +
   geom_text(
      data = . %>% filter(tipo == "voto_jurado" & index == 1),
      aes(x = 1.1, y = -1.1, label = total_jurado),
      color = "#fff800",
      fontface = "bold", family = "Poppins"
   ) +
   geom_text(
      data = . %>% filter(tipo == "voto_tele" & index == 1),
      aes(x = 1.1, y = -1.1, label = total_tele),
      color = "#f80087",
      fontface = "bold", family = "Poppins"
   ) +
   scale_fill_manual(values = c(
      "C" = "#f80087",
      "B" = "#ff89b7",
      "A" = "#ffd5e4"
   )) +
   facet_wrap(vars(forcats::fct_reorder(to_country, desc(total)), tipo), ncol = 8) +
   scale_x_continuous(limits = c(-1.5, 1.5)) +
   scale_y_continuous(limits = c(-1.5, 1.5)) +
   coord_equal() +
   theme_eurovision()


# Leyendas

## Radio

x <- c(0.3, 
       cos(pi / 8) * 0.5, 
       cos(pi / 8 * 2) * 0.7, 
       cos(pi / 8 * 3) * 0.9, 
       0)
xend <-
   c(1.3, 
     cos(pi / 8) * 1.3, 
     cos(pi / 8 * 2) * 1.3, 
     cos(pi / 8 * 3) * 1.3, 
     0)
y <-
   c(0, 
     sin(pi / 8) * 0.5, 
     sin(pi / 8 * 2) * 0.7, 
     sin(pi / 8 * 3) * 0.9, 
     1.1)
yend <-
   c(0, 
     sin(pi / 8) * 1.3, 
     sin(pi / 8 * 2) * 1.3, 
     sin(pi / 8 * 3) * 1.3, 
     1.3)
labels <-
   c(
      "Menos de\n1 225 km",
      "Entre 1 225\n y 2 306 km",
      "Entre 2 306\ny 4 075",
      "Entre 4 075\n y 7 010 km",
      "Más de\n7 010 km"
   )


leyenda_radios <- ggplot() +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), color = "white") +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "white") +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.8), color = "white") +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.6), color = "white") +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.4), color = "white") +
   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0.2), fill = "white", color = "black") +
   geom_text(aes(x = 0, y = 0, label = "País"), color = "black", fontface = "bold") +
   geom_text(aes(x = -1.1, y = 1.1, label = "Total\npuntos"), color = "white", fontface = "bold", size = 5) +
   geom_text(aes(x = 1.1, y = -1.1, label = "Puntos"), color = "white") +
      annotate(geom = "segment", x = x, y = y, xend = xend, yend = yend, color = "grey75", linewidth = 0.5) +
      annotate(geom = "text", x = xend + 0.05, y = yend, label = labels, color = "white", hjust = 0,  size = 2) +
      annotate(geom = "point", x = x, y = y, color = "white") +
   scale_x_continuous(limits = c(-1.5, 1.5)) +
   scale_y_continuous(limits = c(-1.5, 1.5)) +
   coord_equal() +
   theme_eurovision()

## Color de relleno

x_r <- c(1,2,3,1,2,3)
y_r <- c(1,1,1,2,2,2)
fill_r <- c("A1", "A2", "A3", "B1", "B2", "B3")
colores_r <- c("A1" = "#ffd5e4", "A2" = "#ff89b7", "A3" = "#f80087", "B1" = "#eae3bd", "B2" = "#f8ed7a", "B3" = "#fff800")
labels_r <- c("Menos de 33%","Entre 33% y 66%","Más de 66%")

leyenda_rellenos <- ggplot() +
      annotate(geom = "tile", x = x_r, y = y_r, fill = colores_r, height = 1, width = 1) +
      annotate(geom = "text", x = c(1,2,3), y = 2, label = labels_r,  size = 2) +
      annotate(geom = "text", x = 0.4, y = c(1,2), label = c("Porcentaje\nde televotos", "Porcentaje\nde votos del jurado"), size = 2, color = c("#f80087","#fff800"), fontface = "bold", hjust = 1) +
      scale_x_continuous(expand = expansion(add = c(1, 0))) +
      scale_y_continuous(expand = expansion(add = c(0, 0))) +
      theme_eurovision()


# Composición

layout <- "
AAAAAA##
AAAAAABB
AAAAAABB
AAAAAACC
AAAAAA##
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
DDDDDDDD
########
"
plot <- dianas_suecia + leyenda_radios + leyenda_rellenos + dianas_paises + 
   plot_layout(design = layout) +
   plot_annotation(title ="Eurovision 2023") &
   theme_eurovision()

ggsave("pruebaA3.pdf",
       plot = plot,
       width = 8.3 * 2,
       height = 11.7 * 2,
       device = cairo_pdf)

ggsave("pruebaA3.svg",
       plot = plot,
       width = 8.3 * 2,
       height = 11.7 * 2,
       device = "svg")




votos_dist2 <- votos_dist %>% 
   pivot_longer(cols = publico:jurado,
                names_to = "tipo",
                values_to = "puntos")

ggplot(votos_dist2, aes(x = puntos, y = grupo_dist)) +
   geom_boxplot(aes(fill = tipo), alpha = 0.25) +
   geom_point() +
   facet_wrap(vars(to_country))


ggplot(votos_dist2, aes(x = puntos, y = grupo_dist, fill = tipo)) +
   ggdist::stat_halfeye() +
   ggdist::stat_dots(side = "bottom", alpha = 0.5, dotsize = 1) +
   facet_wrap(vars(to_country))


ggplot(votos_dist2, aes(x = puntos, fill = tipo)) +
   geom_dotplot() +
   facet_grid(vars(to_country), vars(grupo_dist))

































