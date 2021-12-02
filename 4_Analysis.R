################
##  Analysis  ##
################

####  Startup  ####
library(tidyverse)
library(ggpubr)

rm(list = ls())



####  Load Data  ####
df <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")



####  Analysis  ####
## Set luck threshold (in SDs)
luck_threshold = .5

## Create new variables
# RTI is already in SD units, per 1_Data Preparation.R
df <- df %>%
  mutate(preference_pp = rti_pred_pp - rti_pred_abc,
         preference_abc = rti_pred_abc - rti_pred_pp,
         preferred_condition = case_when(
           preference_pp > luck_threshold ~ "Project Personality",
           preference_abc > luck_threshold ~ "ABC Project",
           TRUE ~ "No Preference"
         ),
         preferred_condition_force = case_when(
           preference_pp > 0 ~ "Project Personality",
           preference_abc > 0 ~ "ABC Project",
           TRUE ~ "No Preference"
         ),
         luck = case_when(
           preferred_condition != "No Preference" & preferred_condition == condition ~ "Lucky",
           preferred_condition != "No Preference" & preferred_condition != condition ~ "Unlucky",
           preferred_condition == "No Preference" ~ "No Luck"
         ),
         luck_force = case_when(
           preferred_condition_force != "No Preference" & preferred_condition_force == condition ~ "Lucky",
           preferred_condition_force != "No Preference" & preferred_condition_force != condition ~ "Unlucky",
           preferred_condition_force == "No Preference" ~ "No Luck"
         ))

df %>%
  filter(condition != "Control") %>%
  group_by(luck) %>%
  summarize(n = n(),
            rti = mean(rti_actual))

df %>%
  filter(condition != "Control") %>%
  group_by(luck_force) %>%
  summarize(n = n(),
            rti = mean(rti_actual))

## Tests
temp <- df %>%
  filter(luck %in% c("Lucky", "Unlucky"))
t.test(temp$rti_actual ~ temp$luck)

temp <- df %>%
  filter(luck_force %in% c("Lucky", "Unlucky"))
t.test(temp$rti_actual ~ temp$luck_force)  

## Visualize RTI predictions
df %>%
  filter(condition == "Project Personality") %>%
  pivot_longer(matches("^rti_pred")) %>%
  ggplot(aes(rti_actual, value)) +
    geom_point() +
    stat_cor() +
    facet_wrap(~ name) +
    ggtitle("Correlations Between Predicted RTI and Actual RTI",
            "Project Personality Participants Only")

df %>%
  filter(condition == "ABC Project") %>%
  pivot_longer(matches("^rti_pred")) %>%
  ggplot(aes(rti_actual, value)) +
    geom_point() +
    stat_cor() +
    facet_wrap(~ name) +
    ggtitle("Correlations Between Predicted RTI and Actual RTI",
            "ABC Project Participants Only")

df %>%
  filter(condition != "Control") %>%
  ggplot(aes(rti_pred_abc, rti_pred_pp)) +
    geom_point() +
    stat_cor() +
    ggtitle("Correlation Between Predicted RTI in Both Groups")

df %>%
  filter(condition != "Control") %>%
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
    scale_y_continuous(name = "RTI", breaks = -4:2) +
    coord_flip() +
    facet_wrap(~ condition) +
    theme_minimal() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top")
