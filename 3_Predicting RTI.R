######################
##  Predicting RTI  ##
######################

####  Startup  ####
library(tidyverse)
library(tidymodels)

rm(list = ls())



####  Load Data  ####
pp_train <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Project Personality Train Set.rds")
pp_test <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Project Personality Test Set.rds")
abc_train <- readRDS(file = "S:\\COPE\\Data\\Prediction\\ABC Project Train Set.rds")
abc_test <- readRDS(file = "S:\\COPE\\Data\\Prediction\\ABC Project Test Set.rds")

pp_workflow <- readRDS("S:\\COPE\\Data\\Prediction\\Optimized Project Personality Workflow.rds")
abc_workflow <- readRDS("S:\\COPE\\Data\\Prediction\\Optimized ABC Project Workflow.rds")



####  Prepare Data  ####
## Pull predictions
pp_test_pp_pred <- pp_workflow %>%
  fit(pp_train) %>%
  predict(pp_test) %>%
  pull(.pred)
pp_test_abc_pred <- abc_workflow %>%
  fit(abc_train) %>%
  predict(pp_test) %>%
  pull(.pred)

abc_test_pp_pred <- pp_workflow %>%
  fit(pp_train) %>%
  predict(abc_test) %>%
  pull(.pred)
abc_test_abc_pred <- abc_workflow %>%
  fit(abc_train) %>%
  predict(abc_test) %>%
  pull(.pred)


## Combine data and predictions into one dataset
pp_to_combine <- pp_test %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = pp_test_pp_pred,
         rti_pred_abc = pp_test_abc_pred)

abc_to_combine <- abc_test %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = abc_test_pp_pred,
         rti_pred_abc = abc_test_abc_pred)

df <- rbind(pp_to_combine, abc_to_combine)



####  Save  ####
saveRDS(df, file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")
