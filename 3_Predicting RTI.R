######################
##  Predicting RTI  ##
######################

####  Startup  ####
library(tidyverse)
library(tidymodels)
library(performance)

rm(list = ls())



####  Load Data  ####
pp_train <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Project Personality Train Set.rds")
pp_test <- readRDS(file = "S:\\COPE\\Data\\Prediction\\Project Personality Test Set.rds")
abc_train <- readRDS(file = "S:\\COPE\\Data\\Prediction\\ABC Project Train Set.rds")
abc_test <- readRDS(file = "S:\\COPE\\Data\\Prediction\\ABC Project Test Set.rds")

control <- readRDS("S:\\COPE\\Data\\Prediction\\Control Data.rds")

pp_workflow <- readRDS("S:\\COPE\\Data\\Prediction\\Optimized Project Personality Workflow.rds")
abc_workflow <- readRDS("S:\\COPE\\Data\\Prediction\\Optimized ABC Project Workflow.rds")



####  Prepare Data  ####
## Fit and test assumptions: Project Personality model with Project Personality training set
pp_model <- pp_workflow %>%
  fit(pp_train)

pp_diagnostics <- pp_model %>%
  augment(pp_train) %>%
  mutate(resid = .pred - rti) 

# Check homoscedasticity
pp_diagnostics %>%
  ggplot(aes(.pred, resid)) +
    geom_point() +
    geom_smooth() +
    ggtitle("Residuals by Fitted Values for Project Personality")
  
# Check normally distributed residuals
pp_diagnostics %>%
  ggplot() +
    geom_qq(aes(sample = resid)) +
    geom_qq_line(aes(sample = resid)) +
    ggtitle("QQ Plot of Residuals for Project Personality")


## Fit and test assumptions: ABC model with ABC training set
abc_model <- abc_workflow %>%
  fit(abc_train)

abc_diagnostics <- abc_model %>%
  augment(abc_train) %>%
  mutate(resid = .pred - rti) 

# Check homoscedasticity
abc_diagnostics %>%
  ggplot(aes(.pred, resid)) +
    geom_point() +
  geom_smooth() +
  ggtitle("Residuals by Fitted Values for the ABC Project")

# Check normally distributed residuals
abc_diagnostics %>%
  ggplot() +
    geom_qq(aes(sample = resid)) +
    geom_qq_line(aes(sample = resid)) +
    ggtitle("QQ Plot of Residuals for the ABC Project")


## Use models to make predictions in test sets
pp_model_predicts_pp <- pp_model %>%
  predict(pp_test) %>%
  pull(.pred)
abc_model_predicts_pp <- abc_model %>%
  predict(pp_test) %>%
  pull(.pred)

pp_model_predicts_abc <- pp_model %>%
  predict(abc_test) %>%
  pull(.pred)
abc_model_predicts_abc <- abc_model %>%
  predict(abc_test) %>%
  pull(.pred)


## Combine data and predictions into one dataset
pp_to_combine <- pp_test %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = pp_model_predicts_pp,
         rti_pred_abc = abc_model_predicts_pp)

abc_to_combine <- abc_test %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = pp_model_predicts_abc,
         rti_pred_abc = abc_model_predicts_abc)

df <- bind_rows(pp_to_combine, abc_to_combine)



####  Save  ####
saveRDS(df, file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")
