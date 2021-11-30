#################
##  Data Prep  ##
#################

####  Startup  ####
library(tidyverse)
library(tidymodels)
library(readr)

set.seed(53288)



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


## Split training and test sets
pp_split <- initial_split(pp, prop = 3/4)
abc_split <- initial_split(abc, prop = 3/4)

pp_train <- training(pp_split)
pp_test <- testing(pp_split)

abc_train <- training(abc_split)
abc_test <- testing(abc_split)



####  Create workflow sets  ####
## Create recipes
# With one-hot encoding
pp_recipe_one_hot <-
  recipe(rti ~ ., data = pp_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

# Without one-hot encoding
pp_recipe_none_hot <-
  recipe(rti ~ ., data = pp_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())


## Create models
model_xg <- boost_tree() %>% 
  set_mode("regression") %>%
  set_engine("xgboost")

model_rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("regression") %>%
  set_engine("randomForest")

model_mix <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

model_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


## Workflow set
set <- 
  workflow_set(
    preproc = list(
      basic = pp_recipe_none_hot, 
      one_hot = pp_recipe_one_hot
      ),
    models = list(
      xgboost = model_xg,
      randForest = model_rf,
      mixture = model_mix,
      OLS = model_lm
      ),
    cross = TRUE
  )


## Create resampling methods
folds <- vfold_cv(pp_train, v = 10)
save <- control_resamples(save_pred = T)


## Try them out!
out <- 
  set %>% 
  # The first argument is a function name from the {{tune}} package
  # such as `tune_grid()`, `fit_resamples()`, etc.
  workflow_map("tune_grid", resamples = folds, control = save, grid = 5, 
               metrics = metric_set(rmse), verbose = TRUE)

rank_results(out, rank_metric = "rmse")

autoplot(out)
autoplot(out, id = "one_hot_mixture", metric = "rmse")

collect_predictions(out, summarize = T) #see also select_best, metric


## Only the best
best_parameters <- 
  out %>% 
  extract_workflow_set_result("one_hot_mixture") %>% 
  select_best(metric = "rmse")
best_parameters

best <-
  out %>%
  extract_workflow("one_hot_mixture") %>%
  finalize_workflow(best_parameters)

pp_test_with_predictions <- best %>%
  fit(pp_train) %>%
  augment(pp_test)


## TO DO: 
# Split into the following files:
# 1. Data Pre-processing (common to both intervention groups)
# 2. Project Personality model identification and optimization
# 3. ABC Project model identification and optimization
# 4. Predicting RTI for both interventions' test sets with both models
# 5. Analysis!
