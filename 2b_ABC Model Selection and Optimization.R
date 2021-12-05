########################################
##  Model Selection and Optimization  ##
##             ABC Project            ##
########################################

####  Startup  ####
library(tidyverse)
library(tidymodels)
library(doParallel)
library(tictoc)

set.seed(53288)

rm(list = ls())



####  Load Data  ####
abc_data <- readRDS("S:\\COPE\\Data\\Prediction\\ABC Project Model-Ready.rds")



####  Modeling  ####
## Split training and test sets
abc_split <- initial_split(abc_data, prop = 3/4)
abc_train <- training(abc_split)
abc_test <- testing(abc_split)



####  Create workflow sets  ####
## Create recipes
# We're going to mess with a few factors to get different recipes, including:
# Whether or not to remove item-level symptom variables 
# step_rm(matches("^b_..._[0-9]"))
# Whether or not to remove detailed gender variables
# step_rm(matches("^b_dem_gender") & !matches("man_boy|woman_girl"))
# Whether or not to one-hot dummies
# step_dummy(all_nominal_predictors(), one_hot = TRUE)
# step_dummy(all_nominal_predictors(), one_hot = FALSE)
# What near-zero variance cutoff to use
# step_nzv(all_predictors(), options = list(freq_cut = 95/5, unique_cut = 10))
# step_nzv(all_predictors(), options = list(freq_cut = 99/1, unique_cut = 5))
# What imputation to use
# step_impute_knn(all_predictors())
# step_impute_median(all_predictors())

# In the future, we should also investigate:
# Whether or not to use natural splines (for variables with non-linear relationships with RTI)
# Whether to include feature-engineered variables, including
# ZIP code information
# nchar of responses (or presence of response)
# Whether or not to transform variables to be more symmetrical
# Confirm that step_rm works the same as changing roles

recipe_base <-
  recipe(rti ~ ., data = abc_train) %>%
  step_rm(condition)

# Recipe with fewest variables
recipe_fewest <- recipe_base %>%
  step_rm(matches("^b_..._[0-9]")) %>%
  step_rm(matches("^b_dem_gender") & !matches("man_boy|woman_girl")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_impute_knn(all_predictors())

# Recipe with more demographic variables
recipe_dem <- recipe_base %>%
  step_rm(matches("^b_..._[0-9]")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_impute_knn(all_predictors())

# Recipe with more symptom variables
recipe_sym <- recipe_base %>%
  step_rm(matches("^b_dem_gender") & !matches("man_boy|woman_girl")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_impute_knn(all_predictors())

# Recipe with most variables
recipe_most <- recipe_base %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_impute_knn(all_predictors())


## Create models
# Random forest
model_rf <- rand_forest() %>% 
  set_mode("regression") %>%
  set_engine("randomForest")

# Regularized least squares
model_rls <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# OLS linear regression
model_ols <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# K-nearest neighbors
model_knn <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")


## Workflow set
set <- 
  workflow_set(
    list(
      fewest = recipe_fewest,
      dem = recipe_dem,
      sym = recipe_sym,
      most = recipe_most
    ),
    models = list(
      RF = model_rf,
      RLS = model_rls,
      OLS = model_ols,
      KNN = model_knn
    ),
    cross = TRUE
  )


## Test and rank workflows
# Using parallel processing
tic()
cores <- detectCores(logical = F)
cluster <- makeCluster(cores)
registerDoParallel(cluster)
clusterEvalQ(cluster, {library(tidymodels)})

out <- set %>% 
  workflow_map(
    "tune_grid", 
    resamples = vfold_cv(abc_train, v = 10), 
    grid = 10, 
    metrics = metric_set(rmse, rsq), 
    verbose = TRUE
  )

stopCluster(cluster)
toc()

ranking <- rank_results(out, rank_metric = "rmse")
ranking

autoplot(out)
autoplot(out, id = ranking$wflow_id[1], metric = "rmse")


## Only the best
best_parameters <- out %>% 
  extract_workflow_set_result(ranking$wflow_id[1]) %>% 
  select_best(metric = "rmse")
best_parameters

best_workflow <- out %>%
  extract_workflow(ranking$wflow_id[1]) %>%
  finalize_workflow(best_parameters)



####  Save  ####
saveRDS(abc_train, file = "S:\\COPE\\Data\\Prediction\\ABC Project Train Set.rds")
saveRDS(abc_test, file = "S:\\COPE\\Data\\Prediction\\ABC Project Test Set.rds")
saveRDS(best_workflow, "S:\\COPE\\Data\\Prediction\\Optimized ABC Project Workflow.rds")
