################
##  Analysis  ##
################

####  Startup  ####
library(tidyverse)

rm(list = ls())



####  Load Data  ####
df <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")



####  Analysis  ####
## Set luck threshold (in SDs)
luck_threshold = .5

## Create new variables
df <- df %>%
  mutate(preference_pp = (rti_pred_pp - rti_pred_abc) / sd(rti_actual),
         preference_abc = (rti_pred_abc - rti_pred_pp) / sd(rti_actual),
         preferred_condition = case_when(
           preference_pp > luck_threshold ~ "Project Personality",
           preference_abc > luck_threshold ~ "ABC Project",
           TRUE ~ "No Preference"
         ),
         luck = case_when(
           preferred_condition != "No Preference" & preferred_condition == condition ~ "Lucky",
           preferred_condition != "No Preference" & preferred_condition != condition ~ "Unlucky",
           preferred_condition == "No Preference" ~ "No Luck"
         ))

df %>%
  select(condition, matches("^rti"), matches("^pref"), luck) %>%
  group_by(luck) %>%
  summarize(n = n(),
            rti = mean(rti_actual))

## Visualize RTI predictions
df %>%
  group_by(condition) %>%
  arrange(condition, -rti_actual) %>%
  mutate(participant = row_number(),
         preferred_condition = case_when(
           rti_pred_pp < rti_pred_abc ~ "Project Personality",
           rti_pred_abc < rti_pred_pp ~ "ABC Project"),
         preferred_rti = case_when(
           rti_pred_pp < rti_pred_abc ~ rti_pred_pp,
           rti_pred_abc < rti_pred_pp ~ rti_pred_abc),
         ) %>%
  ungroup() %>%
  select(participant, condition, matches("^preferred"), matches("^rti")) %>%
  ggplot() +
    geom_point(aes(participant, rti_actual), color = "#F9B650") +
    geom_point(aes(participant, preferred_rti, color = preferred_condition)) +
    geom_segment(aes(x = participant, xend = participant, 
                     y = rti_pred_abc, yend = rti_pred_pp,
                     color = preferred_condition)) +
    scale_color_discrete(name = NULL) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = "RTI") +
    coord_flip() +
    facet_wrap(~ condition) +
    theme_minimal() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top")


