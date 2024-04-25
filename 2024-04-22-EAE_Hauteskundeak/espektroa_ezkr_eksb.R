library(here)
library(tidyverse)
library(systemfonts)
library(ggtext)
library(classInt)
library(glue)

# DATUAK EKARRI

emaitzak <- read_csv2(here("2024-04-22-EAE_Hauteskundeak", "Emaitzak_guztiak.csv"), locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()

# DATUAK PRESTATU

colnames(emaitzak)[1:12] <- emaitzak[1,1:12]

emaitzak2 <- emaitzak |> 
  filter(LH != "LH") |> 
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
  mutate(alderdia = fct_relevel(alderdia, "eaj_pnv", "eh_bildu", "pse_ee_psoe", "pp", "sumar", "vox"),
         errolda = as.numeric(errolda),
         botoak = as.numeric(botoak),
         baliogabeak = as.numeric(baliogabeak),
         baliodunak = as.numeric(baliodunak),
         zuriak = as.numeric(zuriak),
         botoak_hautagaitzei = as.numeric(botoak_hautagaitzei),
         abstentzioa = as.numeric(abstentzioa),
         b_hautagaitzari = as.numeric(b_hautagaitzari),
         percent_b_hautagaitzari = as.numeric(percent_b_hautagaitzari),
         percent_b_baliodunak = as.numeric(percent_b_baliodunak),
         espektroa = case_when(
           alderdia %in% c("eh_bildu", "pse_ee_psoe", "sumar") ~ "Ezkertiarra",
           .default = "Eskuindarra"
         ),
         naziotasuna = case_when(
           alderdia %in% c("eh_bildu", "eaj_pnv") ~ "Abertzalea",
           .default = "Besteak"
         ),
         espektroa_percent = case_when(
           espektroa == "Ezkertiarra" ~ percent_b_hautagaitzari * -1,
           .default = percent_b_hautagaitzari
         ),
         naziotasuna_percent = case_when(
           naziotasuna == "Abertzalea" ~ percent_b_hautagaitzari * -1,
           .default = percent_b_hautagaitzari
         ),
         nat_breaks_errolda = classInt::classify_intervals(errolda, 6, style = "jenks"))

# Udalerriak espektro politikoaren arabera ordenatzeko

espektro_datuak <- emaitzak2 |> 
  filter(espektroa == "Ezkertiarra") |> 
  group_by(udalerria) |> 
  summarise(guztira = sum(espektroa_percent) * -1) |> 
  mutate(udalerria = fct_reorder(udalerria, guztira))
  

# GRAFIKOA SORTZEKO BESTE OSAGAIAK KALKULATU

# geom_rect-etako erpinak kalkulatu: xmin, xmax, ymin, ymax

y_ardatzerako_datuak <- emaitzak2 |> 
  filter(alderdia == "eaj_pnv") |> 
  mutate(udalerria = fct_relevel(as.factor(udalerria), levels(espektro_datuak$udalerria))) |> 
  arrange(udalerria) |> 
  mutate(altuera = 10 ^ (as.numeric(nat_breaks_errolda) / 3),
         ymax = cumsum(altuera) + 2 * row_number(),
         ymin = ymax - altuera,
         etiketarako_y = (ymax + ymin) / 2) |> 
  select(udalerria, ymax, ymin, etiketarako_y, altuera)


x_ardatzerako_datuak <- emaitzak2 |> 
    mutate(udalerria = fct_relevel(as.factor(udalerria), levels(espektro_datuak$udalerria)),
           alderdia = fct_relevel(as.factor(alderdia), "eh_bildu", "pse_ee_psoe", "sumar", "eaj_pnv", "pp", "vox")) |> 
    arrange(udalerria) |> 
    group_by(udalerria, espektroa) |> 
    arrange(alderdia) |> 
    mutate(
      prob = abs(espektroa_percent),
      xmax = cumsum(prob),
      xmin = xmax - prob
      ) |> 
    ungroup() |> 
    mutate(xmax = case_when(
      espektroa == "Ezkertiarra" ~ xmax * -1,
      .default = xmax),
      xmin = case_when(
        espektroa == "Ezkertiarra" ~ xmin * -1,
      .default = xmin)) |> 
  group_by(udalerria) |> 
  mutate(etiketarako_x = min(xmax) - 1)

datuak <- x_ardatzerako_datuak |> 
  left_join(y_ardatzerako_datuak)

# Legenda sortzeko beharrezko datuak

altuerak <- sort(unique(y_ardatzerako_datuak$altuera))
legendarako_ymax <- cumsum(altuerak) + 1150 + 4 * c(1:6)
legendarako_ymin <- legendarako_ymax - altuerak
legenda_etiketa <- (legendarako_ymax + legendarako_ymin) / 2

# Aldagai grafikoak

tipografia <- "Open Sans"

koloreak <- c("eaj_pnv" = "#1d803d", "pp" = "#0154a2", "eh_bildu" = "#08a3a7", "pse_ee_psoe" = "#e12023", "vox" = "#5abf33", "sumar" = "#e61455")


# GRAFIKOA SORTU

p_espektroa <- ggplot(datuak) +
  # barrak
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = alderdia), color = "white", linewidth = 0.1) +
  # udalerriak
  geom_text(data = datuak |> filter(alderdia == "eaj_pnv"), aes(x = etiketarako_x, y = etiketarako_y, label = udalerria), hjust = 1, size = 1.6) +
  # erdia markatzeko marra
  geom_vline(xintercept = 0) +
  # titulu-legendak
  annotate(geom = "richtext", x = -50, y = max(y_ardatzerako_datuak$ymax) + 35, label = glue(
    '<span style="font-size: 18pt;">**Alderdi ezkertiarrak**</span>',
    '<br><br>',
    '<span style = "font-size: 14pt; color: {koloreak["eh_bildu"]}">**EH Bildu**</span> | ',
    '<span style = "font-size: 14pt; color: {koloreak["pse_ee_psoe"]}">**PSE**</span> | ',
    '<span style = "font-size: 14pt; color: {koloreak["sumar"]}">**Sumar**</span>'
  ), family = tipografia) +
  annotate(geom = "richtext", x = 50, y = max(y_ardatzerako_datuak$ymax) + 35, label = glue(
    '<span style="font-size: 18pt;">**Alderdi eskuindarrak**</span>',
    '<br><br>',
    '<span style = "font-size: 14pt; color: {koloreak["eaj_pnv"]}">**EAJ**</span> | ',
    '<span style = "font-size: 14pt; color: {koloreak["pp"]}">**PP**</span> | ',
    '<span style = "font-size: 14pt; color: {koloreak["vox"]}">**Vox**</span>'
  ), family = tipografia) +
  # barren altuera azaltzeko legenda
  annotate(geom = "text", x = 80, y = max(legendarako_ymax) + 20, label = "Erroldatutakoak", size = 5, family = tipografia, fontface = "bold", hjust = 1) +
  annotate(geom = "rect", xmin = 60, xmax = 80, ymin = legendarako_ymin, ymax = legendarako_ymax, fill = "grey85", color = "black") +
  annotate(geom = "text", x = 59, y = legenda_etiketa, label = levels(emaitzak2$nat_breaks_errolda), hjust = 1, size = 2) +
  # Testuak
  labs(title = "2024 Eusko Legebiltzarrerako Hauteskundeak",
       subtitle = "Udalerrietako espektro politikoa",
       caption = glue(
         '**Oharra**: alderdien multzokatzea irizpide pertsonaletatik abiatuta egin da. Erroldaren klasifikazioa Jenks-en *haustura naturalak* delako algoritmoa erabilita kalkulatu da.',
         '<br><br>',
         'Diseinua: Mikel Madina(@neregauzak) | Datuak: euskadi.eus'
       )) +
  # Eskalak
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25), labels = c("%75", "%50", "%25", "", "%25", "%50", "%75")) + 
  scale_y_continuous(expand = expansion(add = c(2,30))) +
  scale_color_manual(values = koloreak, aesthetics = c("colour", "fill")) +
  # Txantiloia
  theme_void(base_family = tipografia) +
  theme(legend.position = "none",
        plot.title = element_text(size = 36, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(t = 1, b = 1.5, unit = "cm")),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 1, unit = "cm")),
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.1, linetype = "dashed"),
        axis.text.x = element_text(size = 9, margin = margin(2, unit = "mm")),
        plot.margin = margin(t = 1, b = 1, r = 1, l = 1, unit = "cm"))

# GRAFIKOA A2 TAMAINAN GORDE

ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "espektroa.png"),
  plot = p_espektroa,
  device = "png",
  width = 42,
  height = 59.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)


ggsave(
  here("2024-04-22-EAE_Hauteskundeak", "espektroa.pdf"),
  plot = p_espektroa,
  device = cairo_pdf,
  width = 42,
  height = 59.4,
  units = "cm",
  bg = "white"
) 


