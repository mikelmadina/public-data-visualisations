library(here)
library(tidyverse)
library(systemfonts)
library(ggtext)
library(classInt)
library(glue)
library(sf)



# Hauteskundeetako datuak: https://www.euskadi.eus/ab12aAREWar/resultado/maint
# Maparako datuak: https://www.geo.euskadi.eus/cartografia/DatosDescarga/Limites/CB_MUNICIPIOS_5000_ETRS89.zip

# Datuak kargatu

poligonoak <- sf::st_read(here("2024-04-22-EAE_Hauteskundeak", "CB_MUNICIPIOS_5000_ETRS89", "MUNICIPIOS_5000_ETRS89.shp")) |> 
  mutate(kodea = paste0(MUN_PROV,MUN_MUNI))

# puntuak kokatu ahal izateko udalerri bakoitzaren zentroidea kalkulatu behar da

zentroideak <- st_centroid(poligonoak, 4258) |> 
  select(kodea, geometry)

emaitzak <- read_csv2(here("2024-04-22-EAE_Hauteskundeak", "Emaitzak_guztiak.csv"), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

# DATUAK PRESTATU

colnames(emaitzak)[1:12] <- emaitzak[1,1:12]

emaitzak2 <- emaitzak |> 
  filter(LH != "LH") |> 
  pivot_longer(eaj_pnv_13:sumar_54, names_to = "alderdia", values_to = "balorea") |> 
  mutate(alderdia = str_replace(alderdia, "_\\d{2}", "")) |> 
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
  mutate(percent_b_hautagaitzari = as.numeric(percent_b_hautagaitzari)) |> 
  group_by(udalerria) |> 
  arrange(udalerria, desc(percent_b_hautagaitzari)) |> 
  mutate(postua = row_number(),
    hurrengoarekiko_aldea = percent_b_hautagaitzari - lead(percent_b_hautagaitzari)) |> 
  ungroup() |> 
  filter(postua == 1) |> 
  filter(!str_detect(udalerria, "CERA")) |> 
  filter(hurrengoarekiko_aldea != 0) |> 
  mutate(
    mun_prov = case_when(
      lh == "Araba" ~ "01",
      lh == "Gipuzkoa" ~ "20",
      lh == "Bizkaia" ~ "48",
      .default = ""
    ),
    angelua = case_when(
      alderdia == "eaj_pnv" ~ 0.7853981634, 
      alderdia == "eh_bildu" ~ 2.3561944902,
      alderdia %in% c("pse_pp_psoe", "sumar") ~ 3.926990817, 
      alderdia %in% c("pp", "vox") ~ 5.4977871438, 
      .default = NA
    ),
    luzera = scales::rescale(hurrengoarekiko_aldea, to = c(500,7500)),
    zabalera = classInt::classify_intervals(as.numeric(errolda), 6, style = "jenks"),
    kodea = paste0(mun_prov, `udalerriaren_kod`))
  
  
datuak <- emaitzak2 |> 
  left_join(zentroideak)



oharrak <- filter(datuak, str_detect(udalerria, "LAGUARDIA|OREXA"))

oharrak2 <- oharrak |> 
  mutate(xend = unlist(map(oharrak$geometry,1)),
         yend = unlist(map(oharrak$geometry,2))) |> 
  select(udalerria, xend, yend, hurrengoarekiko_aldea) |> 
  as.data.frame() |> 
  mutate(x = case_when(
    udalerria == "OREXA" ~ xend * 1.015,
    udalerria == "LAGUARDIA" ~ xend * 0.955
    ),
    y = case_when(
      udalerria == "OREXA" ~ yend * 0.99995,
      udalerria == "LAGUARDIA" ~ yend * 0.9985
    ))



koloreak <- c("eaj_pnv" = "#1d803d", "pp" = "#0154a2", "eh_bildu" = "#08a3a7", "pse_ee_psoe" = "#e12023", "vox" = "#5abf33", "sumar" = "#e61455")
tipografia <- "Open Sans"

# ggplot() +
#   annotate(geom = "spoke", x = unique(emaitzak2$angelua)[3], y = unique(emaitzak2$angelua)[3], angle = unique(emaitzak2$angelua)[3], radius = unique(emaitzak2$angelua)[3], color = as.factor(unique(emaitzak2$angelua)[3]), arrow = arrow()) +
#   annotate(geom = "text", x = unique(emaitzak2$angelua)[3], y = unique(emaitzak2$angelua)[3], label = unique(emaitzak2$angelua)[3]) +
#   coord_equal()


haize_mapa <- ggplot(datuak) +
  geom_sf(data = poligonoak, mapping = aes(geometry = geometry)) +
  # geom_spoke(data = filter(datuak, hurrengoarekiko_aldea >=10), aes(geometry = geometry, color = alderdia, radius = luzera, angle = angelua, linewidth = as.numeric(zabalera) / 2), stat = "sf_coordinates", lineend = "butt", linejoin = "mitre", arrow = arrow(type = "open", length = unit(1, "mm"), angle = 35)) +
  # geom_spoke(data = filter(datuak, hurrengoarekiko_aldea >=10), aes(geometry = geometry, color = alderdia, radius = luzera, angle = angelua, linewidth = as.numeric(zabalera) / 2), stat = "sf_coordinates", lineend = "butt", linejoin = "mitre") +
  # geom_spoke(data = filter(datuak, hurrengoarekiko_aldea < 10), aes(geometry = geometry, color = alderdia, radius = luzera, angle = angelua, linewidth = as.numeric(zabalera) / 2), stat = "sf_coordinates", lineend = "butt", linejoin = "mitre") +
  geom_spoke(aes(geometry = geometry, color = alderdia, radius = luzera, angle = angelua), stat = "sf_coordinates", lineend = "butt", linejoin = "mitre", arrow = arrow(type = "open", length = unit(1, "mm"), angle = 35)) +
  # oharrak
  geom_curve(data = oharrak2, aes(x = x, xend = xend, y = y, yend = yend), color = "grey25", linewidth = 0.25, linetype = "longdash") +
  annotate(geom = "richtext", x = oharrak2$x, y = oharrak2$y, label = c("Donostia", 
                                                                        "Guardian <span style='color: #0154a2'>PP</span>k 0.62 ehuneko-puntuko aldeaz irabazi zuen.<br>Alderdi abertzalek batek irabazi ez zuen udalerri bakarra da.", 
                                                                        "Orexan <span style='color: #08a3a7'>EH Bildu</span>k 100<br>ehuneko-puntuko<br>aldeaz irabazi zuen."), 
           hjust = c(0.5, 1, 0), vjust = c(0.5, 0, 1), fill = NA, label.colour = NA, size = 2.25, color = "grey25") +
  # legenda
  annotate(geom = "rect", xmin = 575000, xmax = 605000, ymin = 4722000, ymax = 4755500, fill = "grey95", color = "grey25") +
  annotate(geom = "text", x = 590000, y = 4750500, label = "Haize-arrosa", size = 5, family = tipografia, fontface = "bold", color = "grey25") +
  annotate(geom = "spoke", x = 590000, y = 4740000, angle = c(0.7853981634, 2.3561944902, 3.926990817, 5.4977871438), radius = c(5000, rep(3500, 3)), color = c("#1d803d", "#08a3a7", "#e12023", "#0154a2"), arrow = arrow(type = "open", length = unit(1, "mm"), angle = 35)) +
  annotate(geom = "text", x = c(594000, 587000, 587000,  593000), y = c(4743750, 4744000, 4737500, 4737500), label = c("EAJ", "EH Bildu", "PSE, Sumar", "PP, Vox"), color = c("#1d803d", "#08a3a7", "#e12023", "#0154a2"), hjust = c(0, 1, 1, 0), vjust = c(0, 1, 0.5, 0.5), family = tipografia, size = 2.5, fontface = "bold") +
  # kuadranteak
  annotate(geom = "segment", x = c(590000, 585000), xend = c(590000, 595000), y = c(4745000, 4740000), yend = c(4735000, 4740000), linewidth = 0.25, color = "grey50", linetype = "dashed") +
  annotate(geom = "text", x = c(590000, 590000, 584000,  596000), y = c(4746000, 4734000, 4740000, 4740000), label = c("Abertzaleak", "Besteak", "Ezkerra", "Eskuina"), hjust = c(0.5, 0.5, 1, 0), vjust = c(0, 1, 0.5, 0.5), family = tipografia, color = "grey50", size = 2) +
  annotate(geom = "richtext", x = 590000, y = 4732000, label = "Geziaren luzerak udal bakoitzean<br>lehen alderdiak bigarrenari<br>ehuneko puntutan<br>ateratzen dion aldea adierazten du.", vjust = 1, size = 2, color = "grey25", fill = NA, label.colour = NA) +
  labs(title = "2024 Eusko Legebiltzarrerako Hauteskundeak", 
       subtitle = "Nora dabil haizea?", 
       caption = glue(
    '**Oharra**: alderdien multzokatzea irizpide pertsonaletatik abiatuta egin da.',
    '<br><br>',
    'Diseinua: Mikel Madina(@neregauzak) | Datuak: euskadi.eus'
  )) +
  scale_color_manual(values = koloreak, aesthetics = c("colour", "fill")) +
  theme_void(base_family = tipografia) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 32, margin = margin(t = 1, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 26, margin = margin(t = 0.5, unit = "cm")),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 0.25, b = 0.5, l = -1, unit = "cm")))
  
# GRAFIKOA A3 TAMAINAN GORDE

ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "haize_mapa.png"),
  plot = haize_mapa,
  device = "png",
  width = 29.7,
  height = 21,
  units = "cm",
  dpi = 300,
  bg = "white"
)


ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "haize_mapa.pdf"),
  plot = haize_mapa,
  device = cairo_pdf,
  width = 29.7,
  height = 21,
  units = "cm",
  bg = "white"
) 

