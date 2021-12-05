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
## Fit and test assumptions
pp_fit <- pp_workflow %>%
  fit(pp_train)

pp_diagnostics <- pp_fit %>%
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

abc_fit <- abc_workflow %>%
  fit(abc_train)

abc_diagnostics <- abc_fit %>%
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


## Pull predictions
pp_test_pp_pred <- pp_fit %>%
  predict(pp_test) %>%
  pull(.pred)
pp_test_abc_pred <- abc_fit %>%
  predict(pp_test) %>%
  pull(.pred)

abc_test_pp_pred <- pp_fit %>%
  predict(abc_test) %>%
  pull(.pred)
abc_test_abc_pred <- abc_fit %>%
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

control_to_combine <- control %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = NA,
         rti_pred_abc = NA)

df <- bind_rows(pp_to_combine, abc_to_combine, control_to_combine)




####  Save  ####
saveRDS(df, file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")

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

control_to_combine <- control %>%
  rename(rti_actual = rti) %>%
  mutate(rti_pred_pp = NA,
         rti_pred_abc = NA)

df <- bind_rows(pp_to_combine, abc_to_combine, control_to_combine)




####  Save  ####
saveRDS(df, file = "S:\\COPE\\Data\\Prediction\\Analysis-Ready.rds")
