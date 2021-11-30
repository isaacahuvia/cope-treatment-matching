########################
##  Data Preparation  ##
#######################

####  Startup  ####
library(tidyverse)
library(readr)



####  Load Data  ####
raw <- read_csv("S:\\COPE\\Data\\cleaned_cope_data.csv")
config <- read_csv("S:\\COPE\\Code\\cope-treatment-matching\\config.csv") %>%
  mutate(across(matches("model$"), as.logical)) %>%
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
    condition == 0 ~ "Placebo",
    condition == 1 ~ "Project Personality",
    condition == 2 ~ "ABC Project"
  )) %>%
  # Calculate RTI
  mutate(rti = f1_cdi_mean - b_cdi_mean) %>%
  select(-f1_cdi_mean) %>%
  drop_na(rti)

pp <- df %>% 
  filter(condition == "Project Personality")

abc <- df %>%
  filter(condition == "ABC Project")



####  Save Data  ####
saveRDS(pp, file = "S:\\COPE\\Data\\Prediction\\Project Personality Model-Ready.rds")
saveRDS(abc, file = "S:\\COPE\\Data\\Prediction\\ABC Project Model-Ready.rds")
