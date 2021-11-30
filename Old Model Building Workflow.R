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

# find <- function(x) names(raw)[grepl(x, names(raw)) & !grepl("_tim_", names(raw))]
# find("sitbi")
# find("sret")
# find("eeds")
# find("covid")
# find("bstad")



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


## Create recipes
# Machine learning recipes
pp_recipe_ml <-
  recipe(rti ~ ., data = pp_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

abc_recipe_ml <-
  recipe(rti ~ ., data = abc_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

# Linear model recipes
pp_recipe_lm <-
  recipe(rti ~ ., data = pp_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

abc_recipe_lm <-
  recipe(rti ~ ., data = abc_train) %>%
  update_role(condition, new_role = "group") %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_impute_knn(all_predictors())

# Test a recipe to make sure it produces data
# pp_recipe_ml %>% 
#   prep(verbose = TRUE) %>% 
#   bake(new_data = NULL)


## Create models
# Try: 
# - Linear regression (linear_reg)
# - boosted trees (boost_tree)
# - Random forest (rand_forest)
# - K-nearest neighbor (nearest_neighbor)
# - Single layer neural network (mlp)
# - Partial least squares (pls)
# Machine learning models
model_xg <- boost_tree() %>% 
  set_mode("regression") %>%
  set_engine("xgboost")

model_rf <- rand_forest() %>% 
  set_mode("regression") %>%
  set_engine("randomForest")

model_knn <- nearest_neighbor() %>% 
  set_mode("regression") %>%
  set_engine("kknn") 

model_nn <- mlp() %>% 
  set_mode("regression") %>%
  set_engine("nnet")

model_lasso <- linear_reg(penalty = .1, mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

model_ridge <- linear_reg(penalty = .1, mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

model_elastic <- linear_reg(penalty = .1, mixture = .5) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

model_tune <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# OLS linear regression model
model_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


## Create workflows
pp_workflow_xg <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_xg)

pp_workflow_rf <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_rf)

pp_workflow_knn <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_knn)

pp_workflow_nn <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_nn)

pp_workflow_lasso <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_lasso)

pp_workflow_ridge <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_ridge)

pp_workflow_elastic <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_elastic)

pp_workflow_tune <-
  workflow() %>%
  add_recipe(pp_recipe_ml) %>%
  add_model(model_tune)

pp_workflow_lm <- 
  workflow() %>%
  add_recipe(pp_recipe_lm) %>%
  add_model(model_lm)


## Create resampling methods
folds <- vfold_cv(pp_train, v = 10)


## Tune models as relevant
mixture_param <- parameters(penalty(), mixture())
grid <- grid_regular(mixture_param, levels = c(10, 10))

# Add for fun
grid <- grid %>%
  rbind(tibble(
    penalty = c(1.1, 1.25, 1.5, 2, 3, 4, 5),
    mixture = 0
  ))

# Set up parallel processing
# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# clusterEvalQ(cl, {library(tidymodels)})

tuned <- tune_grid(
  pp_workflow_tune,
  resamples = folds,
  grid = grid,
)

# stopCluster(cl)

# Performance
show_best(tuned, metric = "rmse")
collect_metrics(tuned) %>% arrange(-penalty)

collect_metrics(tuned) %>% 
  filter(.metric == "rmse") %>%
  mutate(mixture = format(mixture)) %>% 
  ggplot(aes(x = penalty, y = mean, col = mixture)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  geom_vline(xintercept = 1, color = "purple", lty = "dotted")

# Get best
best <- select_best(tuned, metric = "rmse")


## Fit to training data (with resampling, saving predictions)
save_predictions <- control_resamples(save_pred = T)

pp_fit_xg <- 
  pp_workflow_xg %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_rf <- 
  pp_workflow_rf %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_knn <- 
  pp_workflow_knn %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_nn <- 
  pp_workflow_nn %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_lasso <- 
  pp_workflow_lasso %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_ridge <- 
  pp_workflow_ridge %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_elastic <- 
  pp_workflow_elastic %>%
  fit_resamples(folds, control = save_predictions)

pp_fit_tuned <- pp_workflow_tune %>% 
  finalize_workflow(best) %>% 
  fit_resamples(folds, control = save_predictions)

pp_fit_lm <- 
  pp_workflow_lm %>%
  fit_resamples(folds, control = save_predictions)


## Model performance
pp_fit_xg %>%
  collect_metrics(summarize = T)

pp_fit_rf %>%
  collect_metrics(summarize = T)

pp_fit_knn %>%
  collect_metrics(summarize = T)

pp_fit_nn %>%
  collect_metrics(summarize = T)

pp_fit_lasso %>%
  collect_metrics(summarize = T)

pp_fit_ridge %>%
  collect_metrics(summarize = T)

pp_fit_elastic %>%
  collect_metrics(summarize = T)

pp_fit_tuned %>%
  collect_metrics(summarize = T)

pp_fit_lm %>%
  collect_metrics(summarize = T)



## Predict test data
pp_metrics_ml <- pp_fit_ml %>% 
  collect_metrics(summarize = TRUE)

pp_predictions_ml <- pp_fit_ml %>% 
  collect_predictions(summarize = TRUE)

pp_metrics_lm <- pp_fit_lm %>% 
  collect_metrics(summarize = TRUE)

pp_predictions_lm <- pp_fit_lm %>% 
  collect_predictions(summarize = TRUE)


## Metrics
pp_metrics_ml
cor(pp_train$rti, pp_predictions_ml$.pred)
plot(pp_train$rti, pp_predictions_ml$.pred)

pp_metrics_lm
cor(pp_train$rti, pp_predictions_lm$.pred)
plot(pp_train$rti, pp_predictions_lm$.pred)


