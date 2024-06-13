library(here)
library(tidyverse)
library(ggforce)
library(ggtext)
library(geomtextpath)
library(systemfonts)
library(svglite)

tipo <- "Proxima Nova"

data_raw <- read_csv(here("2024-03-DVS-survey", "set5_challenges.csv"))

data <- data_raw |> 
  select(set5_id, DataVizRoles_collapsed, starts_with("Challenges")) |> 
  pivot_longer(matches("t_[A-Z]"), names_to = "Challenge", values_to = "response") |> 
  mutate(response = fct_relevel(factor(response), "No impact",
                                "Minor impact",
                                "Moderate impact",
                                "Significant impact",
                                "[^not chosen]",
                                "[\\unfinished]"))

####################################################################################
###                                  BRANCHES                                    ###
####################################################################################

branches <- data |> 
  group_by(Challenge, response) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(challenge_label = case_when(
            Challenge == "ChallengesImpact_LackTime" ~ "Lack of<br>time", 
            Challenge == "ChallengesImpact_NonVizActivity" ~ "Too much effort spent<br>on non-viz activity", 
            Challenge == "ChallengesImpact_AccessingData" ~ "Accessing<br>data", 
            Challenge == "ChallengesImpact_LackMentorship" ~ "Lack of<br>mentorship", 
            Challenge == "ChallengesImpact_DataVizNotRespected" ~ "Not enough respect<br>for data visualization", 
            Challenge == "ChallengesImpact_DataVolume" ~ "Data<br>volume", 
            Challenge == "ChallengesImpact_InfoOverload" ~ "Information<br>overload", 
            Challenge == "ChallengesImpact_LearningNewToolsEtc" ~ "Learning new<br>tools/approaches", 
            Challenge == "ChallengesImpact_LackCollaboration" ~ "Lack of<br>collaboration", 
            Challenge == "ChallengesImpact_ToolsTechLimits" ~ "Technical limitations<br>of the tools",  
            Challenge == "ChallengesImpact_LowDataLiteracy" ~ "Low<br>data literacy", 
            Challenge == "ChallengesImpact_LackTechSkill" ~ "Lack of<br>technical skill", 
            Challenge == "ChallengesImpact_LackDesignExpertise" ~ "Lack of<br>design expertise", 
            Challenge == "ChallengesImpact_Other__" ~ "Other<br>challenges",
            .default = NA
          ),
  Challenge = fct_relevel(
           Challenge,
           "ChallengesImpact_LackTime",
           "ChallengesImpact_NonVizActivity",
           "ChallengesImpact_AccessingData",
           "ChallengesImpact_LackMentorship",
           "ChallengesImpact_DataVizNotRespected",
           "ChallengesImpact_DataVolume",
           "ChallengesImpact_InfoOverload",
           "ChallengesImpact_LearningNewToolsEtc",
           "ChallengesImpact_LackCollaboration",
           "ChallengesImpact_ToolsTechLimits",
           "ChallengesImpact_LowDataLiteracy",
           "ChallengesImpact_LackTechSkill",
           "ChallengesImpact_LackDesignExpertise",
           "ChallengesImpact_Other__"
         ),
         level_of_impact = case_when(
           response %in% c("Minor impact", "Moderate impact", "Significant impact") ~ "Some kind of impact",
           response %in% c("No impact", "[^not chosen]") ~ "No impact",
           stringr::str_detect(response,  "unfinished") ~ "No challenge selected",
           .default = ""
         ),
         level_of_impact = forcats::fct_relevel(level_of_impact, "Some kind of impact", "No impact", "No challenge selected"))

branches_points <- tribble(
  ~point, ~x, ~y, ~type, ~color,
  "A", 0, 0, "paso", "#2db1a4",
  "B", 0, 0.7, "paso",  "#2db1a4",
  "C", -0.025, 1, "paso", "#2db1a4",
  "D", -0.3, 1.2, "paso", "#1E6E0F",
  "E", -0.2, 0.5, "paso", "#2db1a4",
  "F", 0, 1, "paso", "#2db1a4",
  "G", 0.4, 1.325, "paso", "#9f5f9c",
  "H", 0.5, 0.95, "paso", "#773110",
  "I", 0.75, 1.5, "paso", "#9f5f9c",
  "J", 0.15, 1.65, "paso", "#153252",
  "K", 0, -0.2, "paso", "#dcb22a",
  "aa", 0, 0.4, "atractor", "#2db1a4",
  "bb", -0.13, 1.2, "atractor", "#1E6E0F",
  "cc", -0.1, 0.8, "atractor", "#2db1a4",
  "dd", 0.75, 1.25, "atractor", "#773110",
  "ee", 0.45, 1.8, "atractor", "#153252",
  "ff", 0.25, 1.25, "atractor", "#9f5f9c",
  "gg", -0.3, 1.35, "atractor", NA
)

branches_paths <- tribble(
  ~response, ~order, ~point,
  "No impact", 1, "A",
  "No impact", 2, "aa",
  "No impact", 3, "B",
  "No impact", 4, "C",
  "No impact", 5, "bb",
  "No impact", 6, "D",
  "[^not chosen]", 1, "B", 
  "[^not chosen]", 2, "cc", 
  "[^not chosen]", 3, "E",
  "Moderate impact", 1, "A",
  "Moderate impact", 2, "F",
  "Moderate impact", 3, "G",
  "Moderate impact", 4, "ff",
  "Moderate impact", 5, "I",
  "Minor impact", 1, "G",
  "Minor impact", 2, "dd",
  "Minor impact", 3, "H",
  "Significant impact", 1, "G",
  "Significant impact", 2, "ee",
  "Significant impact", 3, "J",
  "[\\unfinished]", 1, "A",
  "[\\unfinished]", 2, "K"
)

branches_and_points <- branches_paths |> 
  left_join(branches_points)

data <- branches |> 
  mutate(
    Challenge = fct_relevel(
      Challenge,
      "ChallengesImpact_LackTime",
      "ChallengesImpact_NonVizActivity",
      "ChallengesImpact_AccessingData",
      "ChallengesImpact_LackMentorship",
      "ChallengesImpact_DataVizNotRespected",
      "ChallengesImpact_DataVolume",
      "ChallengesImpact_InfoOverload",
      "ChallengesImpact_LearningNewToolsEtc",
      "ChallengesImpact_LackCollaboration",
      "ChallengesImpact_ToolsTechLimits",
      "ChallengesImpact_LowDataLiteracy",
      "ChallengesImpact_LackTechSkill",
      "ChallengesImpact_LackDesignExpertise",
      "ChallengesImpact_Other__"
    )
  ) |> 
  full_join(branches_and_points) |> 
  group_by(Challenge, response) |> 
  arrange(Challenge, response, desc(order)) |> 
  mutate(divisor = row_number()) |> 
  arrange(Challenge, response, order) |> 
  mutate(linewidth = max(n) / divisor)

####################################################################################
###                                  SPIRALS                                     ###
####################################################################################


generate_spiral <- function(initial_x, initial_y, length, steps = 100, Challenge, response, response_color) {
  theta <- 2 * pi * seq(0, 10, length.out = steps)
  radius <- (steps * theta) / (2 * pi * 12500)
  x <- initial_x + radius * cos(theta)
  y <- initial_y + radius * sin(theta)
  data.frame(x, y, Challenge, response, response_color)
}

spirals_origin_points <- data |> 
  group_by(response) |> 
  filter(order == max(order))

argList <- list(
  spirals_origin_points$x,
  spirals_origin_points$y,
  sample(c(10:15), nrow(spirals_origin_points), replace = TRUE),
  spirals_origin_points$n + 35,
  spirals_origin_points$Challenge,
  spirals_origin_points$response,
  spirals_origin_points$color
)

spirals <- pmap_dfr(argList, generate_spiral) |> 
  group_by(Challenge, response) |> 
  slice(36:n())

####################################################################################
###                                   CHART                                      ###
####################################################################################


p <-
    ggplot(data = data, aes(
      x = x,
      y = y,
      group = interaction(Challenge, response)
    )) +
    ggforce::geom_bspline2(
      aes(linewidth = linewidth, color = I(color)),
      n = 100,
      lineend = "round"
    ) +
  geom_point(data = spirals, aes(fill = I(response_color)), shape = 21, alpha = 0.5, size = 2, color = "grey75") +
  ggtext::geom_richtext(
      data = data |> group_by(response) |> filter(order == max(order) &
                                                   response == "Significant impact"),
      aes(x = 0.085, y = 0.075, label = paste0("<b>",as.numeric(Challenge), " (", n,")</b><br>", challenge_label)),
      vjust = 0,
      hjust = 0,
      family = tipo,
      fill = NA, 
      label.color = NA
    )  +
    geom_textpath(
      data = spirals |> filter(Challenge == "ChallengesImpact_LackTime"),
      aes(x = x, y = y, label = response, color = I(response_color)),
      size = 3,
      text_only = TRUE,
      fontface = "bold",
      hjust = 1,
      vjust = 1.75,
      family = tipo
    ) +
    geom_mark_hull(
      data = spirals,
      aes(
        group = 1,
        filter = Challenge == "ChallengesImpact_LackTime" &
          response %in% c("Minor impact", "Moderate impact", "Significant impact")
      ),
      expand = unit(7.5, "mm"),
      concavity = 3,
      color = "grey50",
      linetype = "dotted"
    ) +
    geom_mark_hull(
      data = spirals,
      aes(
        group = 1,
        filter = Challenge == "ChallengesImpact_LackTime" &
          response %in% c("No impact", "[^not chosen]")
      ),
      expand = unit(6, "mm"),
      concavity = 8,
      color = "grey50",
      linetype = "dotted"
    ) +
    annotate(
      geom = "segment",
      x = -0.25,
      xend = 0.25,
      y = 0,
      yend = 0,
      linewidth = 1.5,
      lineend = "round",
      color = "grey25"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.5, 0.5))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    theme(legend.position = "none") +
    scale_linewidth(range = c(1, 5)) +
    scale_size(range = c(5, 30)) +
    facet_wrap(vars(Challenge), ncol = 3) +
    theme_void() +
    theme(legend.position = "none",
          strip.text = element_blank()) +
    coord_equal()
  
  
  # A2
  ggsave(here("2024-03-DVS-survey", paste0("ign_",janitor::make_clean_names(Sys.time()),".svg")),
         p,
         device = svglite,
         height =16.5 * 2,
         width =11.7 * 2)
  

####################################################################################
###                                STACKED BARS                                  ###
####################################################################################
  
all_levels_bars <- branches |> 
    group_by(Challenge, level_of_impact) |> 
    summarise(n = sum(n)) |> 
    ggplot(aes(x = n, y = fct_rev(Challenge), fill = fct_rev(level_of_impact))) +
    geom_col() +
    scale_fill_manual(values = c("white",  "#dcb22a", "#278f5a", "#453231")) +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_y_discrete(expand = expansion(add = c(0,0)), labels = unique(branches$challenge_label)) +
    theme_minimal(base_family = tipo) +
    theme(legend.position = "none",
          axis.text.y = element_markdown(family = tipo),
          axis.title = element_blank())
  
ggsave(here("2024-03-DVS-survey", "all_levels_bars.svg"),
         all_levels_bars,
         device = svglite,
         width = 81,
         height = 116,
         units = "mm")
  
some_impact_bars <- branches |> 
    filter(response %in% c("Minor impact", "Moderate impact", "Significant impact")) |> 
    ggplot(aes(n, forcats::fct_rev(Challenge), fill = forcats::fct_rev(response)), alpha = 0.5) +
    geom_col(position = "fill") + 
    geom_vline(xintercept = 0.5) +
    scale_fill_manual(values = c("#153252", "#9f5f9c", "#773110")) +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_y_discrete(expand = expansion(add = c(0,0)), labels = unique(branches$challenge_label)) +
    theme_minimal(base_family = tipo) +
    theme(legend.position = "none",
          axis.text.y = element_markdown(family = tipo),
          axis.title = element_blank())
  
ggsave(here("2024-03-DVS-survey", "some_impact_bars.svg"),
         some_impact_bars,
         device = svglite,
         width = 81,
         height = 116,
         units = "mm")
