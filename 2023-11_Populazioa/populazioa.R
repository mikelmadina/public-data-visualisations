# liburutegiak kargatu

library(here)
library(tidyverse)
library(colorspace)
library(ggforce)
library(svglite)
library(systemfonts)

#######################################################
######            DATUAK  KARGATU                ######
#######################################################

pop_urteka_raw <-
  readr::read_csv(
    here("2023-11_Populazioa", "ep10b_20231027-134431.csv"),
    locale = locale(encoding = "ISO-8859-1")
  )

#######################################################
######            DATUAK  PRESTATU              ######
#######################################################

pop_urteka <- pop_urteka_raw %>%
  janitor::clean_names() %>%
  filter(lurralde_eremua == "Euskal AE") %>% 
  pivot_longer(starts_with("x"), names_to = "aldia", values_to = "biztanleak") %>%
  mutate(
    jaiotze_urtea = lubridate::parse_date_time(jaiotze_urtea, "%Y"),
    biztanleak = gsub("-", "0", biztanleak),
    biztanleak = as.numeric(biztanleak),
    aldia = gsub("^x", "", aldia),
    aldia = lubridate::parse_date_time(aldia, "%Y_%m_%d"),
    adina = case_when(jaiotze_urtea < aldia ~ (interval(
      jaiotze_urtea, aldia
    ) / years(1)))
  ) 

### Biztanleriaren adinaren mediana ezagutzeko kalkuluak

pop_urteka_50 <- pop_urteka %>%
  filter(!is.na(adina) &
           lurralde_eremua == "Euskal AE" &
           (aldia == max(aldia) | aldia == min(aldia))) %>% 
  group_by(lurralde_eremua, aldia, sexua) %>%
  reframe(prop = cumsum(biztanleak) / sum(biztanleak),
            adina = adina) %>% 
  ungroup() %>% 
  filter(near(prop, 0.5, tol = 0.01))

### Kalkulatutako medianak testuetan txertatzeko

aldia_min <- lubridate::year(min(filter(pop_urteka_50, sexua == "Emakumeak") %>% select(aldia) %>% pull()))
aldia_max <- lubridate::year(max(filter(pop_urteka_50, sexua == "Emakumeak") %>% select(aldia)%>% pull()))
adina_min_ema <- min(filter(pop_urteka_50, sexua == "Emakumeak") %>% select(adina)%>% pull())
adina_max_ema <- max(filter(pop_urteka_50, sexua == "Emakumeak") %>% select(adina)%>% pull())
adina_min_giz <- min(filter(pop_urteka_50, sexua == "Gizonak") %>% select(adina)%>% pull())
adina_max_giz <- max(filter(pop_urteka_50, sexua == "Gizonak") %>% select(adina)%>% pull())


#######################################################
######                 GRAFIKOA                  ######
#######################################################


kol_emakumeak <- "#a11883"
kol_gizonak <- "#8dab15"

pir_eae <- ggplot(
  data = pop_urteka %>% filter(!is.na(biztanleak) & biztanleak > 5),
  aes(x = biztanleak, y = adina)
  ) +
  
  # medianen marrak
  geom_segment(data = pop_urteka_50 %>% filter(sexua == "Gizonak"),
               aes(x = 0,
                 xend = Inf,
                 y = adina,
                 yend = adina),
               color = colorspace::darken(kol_gizonak, 0.5)) +
  geom_segment(data = pop_urteka_50 %>% filter(sexua == "Emakumeak"),
               aes(x = 0,
                 xend = -Inf,
                 y = adina,
                 yend = adina),
               color = kol_emakumeak) +
  
  # Gizonak barrak
  geom_rect(
    data = . %>% filter(aldia == max(aldia) & sexua == "Gizonak"),
    aes(
      xmin = 0,
      xmax = biztanleak,
      ymin = adina - 0.5,
      ymax = adina + 0.5
    ),
    fill = colorspace::lighten(kol_gizonak, 0.9),
    color = colorspace::lighten(kol_gizonak, 0.9)
  ) +
  
  # Gizonak marrak (azken urtea ezik)
  geom_path(
    data = . %>% filter(sexua == "Gizonak" & aldia != max(aldia)),
    aes(group = aldia,
        alpha = aldia),
    color = colorspace::lighten(kol_gizonak, 0.2),
    linewidth = 0.25
  ) +
  
  # Gizonak marra (azken urtea)
  geom_path(
    data = . %>% filter(sexua == "Gizonak" & aldia == max(aldia)),
    aes(group = aldia),
    color = colorspace::darken(kol_gizonak, 0.5),
    linewidth = 0.5
  ) +
  
  # Emakumeak barrak
  geom_rect(
    data = . %>% filter(aldia == max(aldia) & sexua == "Emakumeak"),
    aes(
      xmin = 0,
      xmax = (biztanleak  * -1),
      ymin = adina - 0.5,
      ymax = adina + 0.5
    ),
    fill = colorspace::lighten(kol_emakumeak, 0.9),
    color = colorspace::lighten(kol_emakumeak, 0.9)
  ) +
  
  # Emakumeak marrak (azken urtea izan ezik)
  geom_path(
    data = . %>% filter(sexua == "Emakumeak" & aldia != max(aldia)),
    aes(x = (biztanleak  * -1),
      group = aldia,
      alpha = aldia),
    color = colorspace::lighten(kol_emakumeak, 0.2),
    linewidth = 0.25
  ) +
  
  # Emakumeak marra (azken urtea)
  geom_path(
    data = . %>% filter(sexua == "Emakumeak" & aldia == max(aldia)),
    aes(x = (biztanleak  * -1),
      group = aldia),
    color = kol_emakumeak,
    linewidth = 0.5
  ) +
  # 
  ggforce::geom_mark_hull(
    data = . %>% filter(sexua == "Emakumeak" & between(adina, 60, 80)),
    aes(biztanleak  * -1),
    concavity = 5,
    color = colorspace::darken(kol_gizonak, 0.5),
    linetype = "dotted",
    linewidth = 1
  ) +
  # 
  ggforce::geom_mark_hull(
    data = . %>% filter(sexua == "Gizonak" & between(adina, 20, 45)),
    concavity = 5,
    color = kol_emakumeak,
    linetype = "dotted",
    linewidth = 1
  ) +
  
  # Lehen eta azken urteetako adinen medianei buruzko testu-oharrak
  annotate(geom = "text", 
           x = c(-13500, -11500, 12500, 10000), 
           y = c(49,42, 46, 39), 
           label = c(str_glue("{aldia_max}","an emakumeen %50ek\n","{adina_max_ema} urte edo gehiago zituen"),
                     str_glue("{aldia_min}","ean emakumeen %50ek\n","{adina_min_ema} urte edo gehiago zituen"),
                     str_glue("{aldia_max}","an gizonen %50ek\n","{adina_max_giz} urte edo gehiago zituen"),
                     str_glue("{aldia_min}","ean gizonen %50ek\n","","{adina_min_giz} urte edo gehiago zituen")),
           color = c(rep(kol_emakumeak,2), rep(colorspace::darken(kol_gizonak, 0.5),2)),
           family = "Open Sans", 
           hjust = c(rep(0,2), rep(1,2))) +
  
  # Medianen marren gainean urtea testuz adierazteko
  annotate(geom = "text", 
           x = c(-Inf, -Inf, Inf, Inf), 
           y = c(50, 43, 47, 40), 
           label = rep(c(aldia_max, aldia_min), 2),
           color = c(rep(kol_emakumeak, 2), rep(colorspace::darken(kol_gizonak, 0.5), 2)),
           family = "Open Sans", 
           size = 3,
           hjust = c(rep(0, 2), rep(1, 2))) +
  
  # Ardatz bertikalean urteak bosnaka jartzeko
  annotate(
    geom = "text",
    x = 0,
    y = seq(5, 105, 5),
    label = seq(5, 105, 5),
    family = "Open Sans",
    size = 5
  ) +
  
  # Koloreztatutako eremutik kanpoko marrak nola interpretatu
  annotate(geom = "text",
           x = 17000,
           y = 15,
           label = "Koloreztatutako eremutik kanpo dauden marrek\nadin horretan 2001ean 2023an baino\nbiztanle gehiago zeudela adierazten dute\n(marrak zenbat eta lausoago,\norduan eta denboran aldenduago)",
           family = "Open Sans",
           size = 2.5,
           lineheight = 0.9,
           color = kol_emakumeak) +
  
  # Koloreztatutako eremu barneko marrak nola interpretatu
  annotate(geom = "text",
           x = -17000,
           y = 82,
           label = "Koloreztatutako eremu barnean dauden marrek\nadin horretan 2001ean 2023an baino\nbiztanle gutxiago zeudela adierazten dute.",
           family = "Open Sans",
           size = 2.5,
           lineheight = 0.9,
           color = colorspace::darken(kol_gizonak, 0.5)) +
  
  # Legenda (laukizuzena)
  annotate(geom = "rect",
           xmin = 7500,
           xmax = Inf,
           ymin = 95,
           ymax = 105,
           color = "grey15",
           fill = "white") +
  
  # Legenda (testuak)
  
  annotate(geom = "text",
           x = 8500,
           y = c(103,101,99,97),
           label = c("2023ko emakumezkoen datuak", "2021eko emakumezkoen datuak","2023ko gizonezkoen datuak", "2021eko gizonezkoen datuak"),
           color = c(rep(kol_emakumeak, 2), rep(colorspace::darken(kol_gizonak, 0.5), 2)),
           alpha = rep(c(1, 0.2), 2),
           hjust = 0,
           family = "Open Sans") +
  
  # Sarrera-testua
  annotate(geom = "text",
           x = -20250,
           y = c(105, 97),
           label = c("EUSTATen dauten arabera 2001-2023 urteen artean\nEAEko biztanlegoa %5.65 hazi da,\n2023 urteko 2 196 745 biztanleak aintzat hartuta",
                     "Adinaren araberako banaketak\nargi erakusten du azken hogei urteetan\ngure gizartea zaharkitu egin dela."),
           hjust = 0,
           vjust = 1,
           family = "Open Sans",
           size = 4.5) +

  labs(title = "EAEko biztanlegoa zahartu egin da azken hogei urteotan",
       caption = "Datuak: @Eustat | Lana: Mikel Madina @neregauzak Jorge Camoes @wisevis en irudietan oinarrituta") +
  scale_x_continuous(breaks = seq(-20000, 20000, 5000),
                     labels = abs(seq(-20000, 20000, 5000))) +
  scale_y_continuous(breaks = seq(0, 100, 25), expand = expansion(add = c(0, 0))) +
  theme_void(base_family = "Open Sans") +
  theme(legend.position = "none",
        plot.margin = unit(c(1,1,0.5,1), "cm"),
        plot.title = element_text(size = 28, family = "Open Sans", face = "bold", margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "cm")),
        plot.background = element_rect(fill = colorspace::lighten("#fff3b0", 0.9)),
        plot.caption = element_text(margin = margin(t = 1, r = 0, b = 0.5, l = 0, unit = "cm")),
        axis.line.x = element_line(color = "grey15", linewidth = 0.5),
        axis.ticks.x = element_line(color = "grey15"),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
        axis.ticks.length.x = unit(2, "mm"),
        axis.text.x = element_text(family = "Open Sans", size = 7)
  )

ggsave(
  here("2023-11_Populazioa", "pir_eae.png"),
  plot = pir_eae,
  device = "png",
  width = 11.7,
  height = 16.5
)

