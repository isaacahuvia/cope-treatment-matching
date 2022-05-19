########################
##  Data Preparation  ##
########################

####  Startup  ####
library(tidyverse)
library(readr)

rm(list = ls())



####  Load Data  ####
raw <- read_csv("S:\\COPE\\Data\\cleaned_cope_data.csv")
config <- read_csv("S:\\COPE\\Code\\cope-treatment-matching\\config.csv") %>%
  mutate(across(matches("feature"), as.logical)) %>%
  drop_na()



####  Prepare Data  ####
## Clean raw data
df <- raw %>%
  # Select variables
  select(condition, 
         all_of(config$variable),
         f1_cdi_mean) %>%
  # Recode variables
  mutate(condition = case_when(
    condition == 0 ~ "Control",
    condition == 1 ~ "Project Personality",
    condition == 2 ~ "ABC Project"
  )) %>%
  # Calculate RTI
  drop_na(b_cdi_mean, f1_cdi_mean) %>%
  group_by(condition) %>%
  mutate(rti = (f1_cdi_mean - b_cdi_mean) / sd(b_cdi_mean)) %>%
  ungroup() %>%
  select(-f1_cdi_mean)

pp <- df %>% 
  filter(condition == "Project Personality")

abc <- df %>%
  filter(condition == "ABC Project")

full_sample <- df %>%
  filter(condition %in% c("Project Personality", "ABC Project"))



####  Save Data  ####
saveRDS(pp, file = "S:\\COPE\\Data\\Prediction\\Project Personality Model-Ready.rds")
saveRDS(abc, file = "S:\\COPE\\Data\\Prediction\\ABC Project Model-Ready.rds")
saveRDS(full_sample, file = "S:\\COPE\\Data\\Prediction\\Full Sample.rds")
