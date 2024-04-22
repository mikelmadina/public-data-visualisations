library(here)
library(tidyverse)
library(sf)
library(glue)
library(ggsflabel)
library(systemfonts)

# Hauteskundeetako datuak: https://www.euskadi.eus/ab12aAREWar/resultado/maint
# Maparako datuak: https://www.geo.euskadi.eus/cartografia/DatosDescarga/Limites/CB_MUNICIPIOS_5000_ETRS89.zip

# Datuak kargatu

udalerriak <- sf::st_read(here("2024-04-22-EAE_Hauteskundeak", "CB_MUNICIPIOS_5000_ETRS89", "MUNICIPIOS_5000_ETRS89.shp")) |> 
  mutate(kodea = paste0(MUN_PROV,MUN_MUNI))

emaitzak <- read_csv2(here("2024-04-22-EAE_Hauteskundeak", "Emaitzak_abs.csv"), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

# Datuak prestatu

emaitzak2 <- emaitzak |> 
  pivot_longer(cols= eaj_pnv:sumar, names_to = "alderdia", values_to = "bozkak") |> 
  mutate(bozkak = as.numeric(bozkak)) |> 
  group_by(udalerria) |> 
  filter(bozkak == max(bozkak, na.rm = TRUE)) |> 
  filter(!str_detect(udalerria, "MUNICIPIO CERA")) |> 
  mutate(mun_prov = case_when(
    lurraldea == "Araba" ~ "01",
    lurraldea == "Gipuzkoa" ~ "20",
    lurraldea == "Bizkaia" ~ "48",
    .default = ""
  ),
  kodea = paste0(mun_prov, udalerriaren_kod))

# Udalerri bakoitzaren zentroidea kalkulatu puntuak kokatu ahal izateko

# ETRS89ren EPSG kodea: https://epsg.io/4258

zentroideak <- st_centroid(udalerriak, 4258) |> 
  left_join(emaitzak2)

# Bozka gehien eta gutxien jaso duten alderdi garaileak

gehiena <- max(emaitzak2$bozkak)
gutxiena <- min(emaitzak2$bozkak)

gehiena_udala <- pull(emaitzak2[emaitzak2$bozkak == gehiena, "udalerria"])
gutxiena_udala <- pull(emaitzak2[emaitzak2$bozkak == gutxiena, "udalerria"])

# Grafikoa sortu

p <- ggplot() +
  geom_sf(data = udalerriak, fill = "grey95") +
  geom_sf(data = udalerriak |> filter(NOMBRE_TOP %in% c("Gaintza", "Bilbao")), fill = "grey35") +
  geom_sf(data = zentroideak, aes(size = errolda, color = alderdia), alpha = 0.4) +
  scale_colour_manual(values = c("eaj_pnv" = "#1d803d", "pp" = "#0154a2", "eh_bildu" = "#09a1a6", "black")) +
  geom_sf_text(data = zentroideak  |> filter(udalerria %in% c("GAINTZA", "BILBAO")), aes(label = udalerria), alpha = 0.75, size = 3) +
  scale_size_continuous(range = c(1, 35)) +
  # scale_alpha_binned(breaks = seq(from = 0, to = 1, by = 0.25)) +
  # facet_wrap(vars(tarteak)) +
  labs(title = "2024ko Eusko Legebiltzarrerako Hauteskundeak", subtitle = paste0("Udalerri bakoitzean alderdi irabazleak jasotako bozka kopurua:\n", glue("{gehiena_udala}n EAJk {gehiena} bozka jaso ditu;\n{gutxiena_udala}n EH Bilduk {gutxiena} bozka jaso ditu.")), caption = "Mikel Madina (@neregauzak) | Datuak: euskadi.eus eta geo.euskadi.eus") +
  theme_void(base_family = "Open Sans") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 22, margin = margin(t = 1, b = 0.5, unit = "cm")),
        plot.subtitle = element_text(lineheight = 1.25),
        plot.caption = element_text(margin = margin(b = 0.5, unit = "cm")
                                    )
        )

# Grafikoa gorde

ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "mapa.png"),
  plot = p,
  device = "png",
  width = 1500 * 2,
  height = 1250 * 2,
  units = "px",
  bg = "white"
  
)
