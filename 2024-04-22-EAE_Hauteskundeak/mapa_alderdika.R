library(here)
library(tidyverse)
library(sf)
library(systemfonts)
library(ggtext)

# Hauteskundeetako datuak: https://www.euskadi.eus/ab12aAREWar/resultado/maint
# Maparako datuak: https://www.geo.euskadi.eus/cartografia/DatosDescarga/Limites/CB_MUNICIPIOS_5000_ETRS89.zip

# Datuak kargatu

udalerriak <- sf::st_read(here("2024-04-22-EAE_Hauteskundeak", "CB_MUNICIPIOS_5000_ETRS89", "MUNICIPIOS_5000_ETRS89.shp")) |> 
  mutate(kodea = paste0(MUN_PROV,MUN_MUNI))

# puntuak kokatu ahal izateko udalerri bakoitzaren zentroidea kalkulatu behar da

zentroideak <- st_centroid(udalerriak, 4258) |> 
  select(kodea, geometry)

# emaitzak biltzen dituen datutegia prestatzeko pausoak

emaitzak <- read_csv2(here("2024-04-22-EAE_Hauteskundeak", "Emaitzak_guztiak.csv"), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

colnames(emaitzak)[1:12] <- emaitzak[1,1:12]

emaitzak2 <- emaitzak |> 
  filter(LH != "LH") |> 
  mutate(mun_prov = case_when(
    LH == "Araba" ~ "01",
    LH == "Gipuzkoa" ~ "20",
    LH == "Bizkaia" ~ "48",
    .default = ""
  ),
  kodea = paste0(mun_prov, `Udalerriaren kod.`),
  errolda = as.integer(Errolda)) |> 
  pivot_longer(eaj_pnv_13:sumar_54, names_to = "alderdia", values_to = "balorea") |> 
  mutate(alderdia = str_replace(alderdia, "_\\d{2}", "")
         ) |> 
  group_by(Udalerria, alderdia) |> 
  mutate(aldagaia = case_when(
    row_number() == 1 ~ "B. Hautagaitzari",
    row_number() == 2 ~ "%B. Hautagaitzari",
    row_number() == 3 ~ "%B. Baliodunak",
    .default = NA
    )) |> 
  ungroup() |> 
  pivot_wider(names_from = aldagaia, values_from = balorea) |> 
  janitor::clean_names() |> 
  filter(!str_detect(udalerria, "MUNICIPIO CERA")) |> 
  filter(alderdia %in% c("eaj_pnv", "pp", "pse_ee_psoe", "eh_bildu", "vox", "sumar")) |> 
  mutate(alderdia = fct_relevel(alderdia, "eaj_pnv", "eh_bildu", "pse_ee_psoe", "pp", "sumar", "vox"))

# grafikoa sortzeko erabiliko diren datutegiak

# dataframe batek ezin ditu bi "geometry" zutabe izan
# Horregatik, poligonoak eta zentroideak bereizturik gehitu egiten dira

poligonoak <- emaitzak2 |> 
  left_join(udalerriak)

puntuak <- emaitzak2 |> 
  left_join(zentroideak) |> 
  filter(as.numeric(b_hautagaitzari) > 0)

# facet_wrapekin grafiko bakoitzak dagokion titulua izan dezan

tituluak <- tibble(
  alderdia = c("eaj_pnv", "pp", "pse_ee_psoe", "eh_bildu", "vox", "sumar"),
  legenda = c("EAJ", "PP", "PSE", "EH Bildu", "Vox", "Sumar")
  )

# Datu interesgarriak marrazteko hainbat datutegi sortu behar dira

etiketak_max <- emaitzak2 |> 
  group_by(alderdia) |> 
  filter(!str_detect(udalerria, "MUNICIPIO CERA")) |> 
  filter(as.numeric(b_hautagaitzari) == max(as.numeric(b_hautagaitzari), na.rm = TRUE)) |> 
  mutate(etiketa = paste0(udalerria, "\n(", b_hautagaitzari," - %", percent_b_hautagaitzari,")")) |> 
  summarise(udalerriak = paste0(etiketa, collapse = "\n"), 
            kodea = max(kodea)) |> 
  left_join(zentroideak)

etiketak_percent_max <- emaitzak2 |> 
  group_by(alderdia) |> 
  filter(as.numeric(percent_b_hautagaitzari) == max(as.numeric(percent_b_hautagaitzari), na.rm = TRUE)) |> 
  mutate(etiketa = paste0(udalerria, "\n(%", percent_b_hautagaitzari," - ", b_hautagaitzari,")")) |> 
  summarise(udalerriak = paste0(etiketa, collapse = "\n"), 
            kodea = max(kodea)) |> 
  left_join(zentroideak)

etiketak_min <- emaitzak2 |> 
  group_by(alderdia) |> 
  filter(!str_detect(udalerria, "MUNICIPIO CERA")) |> 
  filter(as.numeric(b_hautagaitzari) == min(as.numeric(b_hautagaitzari), na.rm = TRUE)) |> 
  mutate(etiketa = paste0(udalerria, " (", b_hautagaitzari,")")) |> 
  summarise(udalerriak = paste0(etiketa, collapse = "\n"))

# grafikoa prestatu

p <- ggplot(puntuak) +
  geom_sf(data = poligonoak, mapping = aes(geometry = geometry)) +
  geom_sf(data = puntuak, mapping = aes(geometry = geometry, color = alderdia, size = as.numeric(b_hautagaitzari)), alpha = 0.6) +
  geom_sf_label(data = etiketak_max, aes(geometry = geometry, label = udalerriak), size = 2.5, alpha = 0.8, fill = "#CD661D", color = "white") +
  geom_sf_label(data = etiketak_percent_max, aes(geometry = geometry, label = udalerriak), size = 2.5, alpha = 0.8, fill = "gold") +
  geom_text(data = tituluak, aes(x = 480000, y = 4710000, label = legenda, color = alderdia), family = "Open Sans", size = 7, fontface = "bold", hjust = 0, vjust = 0) +
  geom_text(data = etiketak_min, aes(x = 600000, y = 4700000, label = udalerriak), family = "Open Sans", size = 1.75, hjust = 1, vjust = 0) +
  scale_size_continuous(range = c(0,35)) +
  scale_color_manual(values = c("eaj_pnv" = "#1d803d", "pp" = "#0154a2", "eh_bildu" = "#08a3a7", "pse_ee_psoe" = "#e12023", "vox" = "#5abf33", "sumar" = "#e61455")) +
  facet_wrap(vars(alderdia)) +
  labs(title = "2024ko Eusko Legebiltzarrerako Hauteskundeak",
       subtitle = "Bozka kopurua alderdi eta udalerriekiko<br>
       <span style='font-size: 11pt'><span style='color:#CD661D;'>**Zenbakia aurretik duten etiketek**</span> alderdi bakoitzak boto gehien eskuratu duen udalerria identifikatzen dute.</span> | 
       <span style='font-size: 11pt'><span style='color:#FFD700;'>**Ehunekoa aurretik duten etiketek**</span> alderdi bakoitzak botoen ehuneko handiena eskuratu duen udalerria identifikatzen dute.</span>",
       caption = "Mikel Madina (@neregauzak) | Datuak: euskadi.eus eta geo.euskadi.eus") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 22, margin = margin(t = 0, b = 0.5, l = 1, unit = "cm")),
        plot.subtitle = element_markdown(size = 18, lineheight = 1.25, margin = margin(l = 1, b = 1, unit = "cm")),
        plot.caption = element_text(margin = margin(t = 1, b = 0, r = 1, unit = "cm")),
        strip.background = element_blank(),
        strip.text = element_blank())

# grafikoa esportatu

ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "mapa_alderdika.png"),
  plot = p,
  device = "png",
  width = 6000,
  height = 4000,
  units = "px",
  bg = "white"
)  

    
ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "mapa_alderdika.pdf"),
  plot = p,
  device = cairo_pdf,
  width = 6000,
  height = 4000,
  units = "px",
  bg = "white"
) 

