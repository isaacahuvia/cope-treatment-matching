########################################
##  Model Selection and Optimization  ##
##            ABC Project             ##
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
# With one-hot encoding
abc_recipe_one_hot <-
  recipe(rti ~ ., data = abc_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% #tune() instead?
  step_nzv(all_predictors()) %>% #Tune with freq_cut(), unique_cut()?
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

# Without one-hot encoding
abc_recipe_none_hot <-
  recipe(rti ~ ., data = abc_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())


## Create models
# Random forest
model_rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
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


## Workflow set
set <- 
  workflow_set(
    preproc = list(
      one_hot = abc_recipe_one_hot,
      none_hot = abc_recipe_none_hot
    ),
    models = list(
      random_forest = model_rf,
      regularized_least_squares = model_rls,
      ordinary_least_squares = model_ols
    ),
    cross = TRUE
  )


## Create resampling methods
resampling_method <- vfold_cv(abc_train, v = 10)
save_predictions <- control_resamples(save_pred = T)


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
    resamples = resampling_method, 
    control = save_predictions, 
    grid = 5, 
    metrics = metric_set(rmse), 
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

best_workflow <-
  out %>%
  extract_workflow(ranking$wflow_id[1]) %>%
  finalize_workflow(best_parameters)



####  Save  ####
saveRDS(abc_train, file = "S:\\COPE\\Data\\Prediction\\ABC Project Train Set.rds")
saveRDS(abc_test, file = "S:\\COPE\\Data\\Prediction\\ABC Project Test Set.rds")
saveRDS(best_workflow, "S:\\COPE\\Data\\Prediction\\Optimized ABC Project Workflow.rds")
