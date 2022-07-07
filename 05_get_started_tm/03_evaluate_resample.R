## Tidy Models Evaluate with Resampling -----

# From https://www.tidymodels.org/start/resampling/

# 01 Packages ----

require(tidymodels)
require(modeldata)

# 02 Data ----

data(cells, package = "modeldata")

cells |>
  count(class) |>
  mutate(prop = n / sum(n))

# 03 Split into Train/Test  ----

set.seed(123)

cell_split <-
  initial_split(
    cells |>
      select(-case),
    strata = class
  )

cell_train <- training(cell_split)
cell_test <- testing(cell_split)

cell_train |>
  count(class) |>
  mutate(prop = n / sum(n))

cell_test |>
  count(class) |>
  mutate(prop = n / sum(n))

# 04 Modeling -----

rf_mod <-
  rand_forest(trees = 1000) |>
  set_engine("ranger") |>
  set_mode("classification")

set.seed(234)

rf_fit <-
  rf_mod |>
  fit(class ~ ., data = cell_train)

rf_fit

# 05 Estimating Performance ----

# Using:
# - ROC
# - Accuracy

# 05.01 Doin' it wrong ----

rf_training_pred <-
  predict(rf_fit, cell_train) |>
  bind_cols(predict(rf_fit, cell_train, type = "prob")) |>
  bind_cols(
    cell_train |>
      select(class)
  )

rf_training_pred |>
  roc_auc(truth = class, .pred_PS)

rf_training_pred |>
  accuracy(truth = class, .pred_class)

rf_testing_pred <-
  predict(rf_fit, cell_test) |>
  bind_cols(predict(rf_fit, cell_test, type = "prob")) |>
  bind_cols(cell_test |> select(class))

rf_testing_pred |>
  roc_auc(truth = class, .pred_PS)

rf_testing_pred |>
  accuracy(truth = class, .pred_class)

# In Tidymodels for CV

# Each Fold Has
# - Analysis set aka training set with 90% of data to train model
# - Assessment set aka testing set with 10% of data to assess model

# 06 Fit Model with Resampling ----

set.seed(345)

folds <- vfold_cv(cell_train, v = 10)

folds

# Can Resample
# - Model Specification with formula or recipe
# - Workflow that bundles a model specification & formula/recipe

# Workflow

rf_wf <-
  workflow() |>
  add_model(rf_mod) |>
  add_formula(class ~ .)

set.seed(456)

rf_fit_rs <-
  rf_wf |>
  fit_resamples(folds)

rf_fit_rs
collect_metrics(rf_fit_rs)

rf_testing_pred |>
  roc_auc(truth = class, .pred_PS)

rf_testing_pred |>
  accuracy(truth = class, .pred_class)
