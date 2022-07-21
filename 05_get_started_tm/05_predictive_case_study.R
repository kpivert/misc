## Tidy Models Predictive Modeling Case Study ----

# From: https://www.tidymodels.org/start/case-study/

# 01 Packages -----

require(tidymodels)
require(readr)
require(vip)

theme_set(
  theme_minimal()
)

# 02 Data -----

hotels <-
  read_csv('https://tidymodels.org/start/case-study/hotels.csv') |>
  mutate(
    across(where(is.character), as.factor)
  )

dim(hotels)
hotels |> glimpse()

# 03 Business Problem -----

# Build a model to predict which actual hotel stays included children and/or babies,
# and which did not. Our outcome variable children is a factor variable with two levels:

hotels |>
  count(children) |>
  mutate(Pct = n / sum(n))

# Big class imbalance. Can use:
# - upsample()
# - downsample()
# - {themis}

# 04 Data Splitting and Resampling -----

set.seed(123)

splits <-
  initial_split(
  hotels,
  strata = children
)

hotel_other <- training(splits)
hotel_test <- testing(splits)

hotel_other |>
  count(children) |>
  mutate(Pct = n / sum(n))

hotel_test |>
  count(children) |>
  mutate(Pct = n / sum(n))

# Instead of resampling, create a validation dataset

set.seed(234)

val_set <-
  validation_split(
    hotel_other,
    strata = children,
    prop = 0.8
  )

# 05 Penalized Logistic Regression ----

# glmnet:
# Fits generalized linear model via penalized maximum likelihood

args(logistic_reg)

lr_mod <-
  logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

# 06 Create Recipe ----

holidays <-
  c("AllSouls", "AshWednesday", "ChristmasEve",
    "Easter", "GoodFriday", "NewYearsDay",
    "PalmSunday"
    )

lr_recipe <-
  recipe(children ~ ., data = hotel_other) |>
  step_date(arrival_date) |>
  step_holiday(arrival_date, holidays = holidays) |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_rm(arrival_date)

# 07 Create Workflow ----

lr_workflow <-
  workflow() |>
  add_model(lr_mod) |>
  add_recipe(lr_recipe)

lr_workflow

# 08 Create Grid for Tuning ----

lr_reg_grid <-
  tibble(
    penalty = 10^seq(-4, -1, length.out = 30)
  )

lr_reg_grid |>
  top_n(5)

lr_reg_grid |>
  slice_max(
    n = 5,
    penalty
    )
# 09 Train and Tune Model -----

# ls_res <-
#   lr_workflow |>
#   tune_grid(
#     val_set,
#     grid = lr_reg_grid,
#     control  = control_grid(save_pred = TRUE),
#     metrics = metric_set(roc_auc)
#   )


lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# 10 Review AUC ----

lr_plot <-
  lr_res |>
  collect_metrics() |> 
  ggplot( 
    aes(
      x = penalty, 
      y = mean
    )
  ) +
  geom_point() +
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot

# 11 Pick Best Models -----

top_models <- 
  lr_res |> 
  show_best("roc_auc", n = 15) |> 
  arrange(penalty)

top_models


lr_plot +
  geom_vline(
    xintercept = top_models |> slice(12) |> pull(penalty)
  ) +
  geom_vline(
    xintercept = top_models |> slice(11) |> pull(penalty),
    linetype = "dotted"
  )

lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(12)

lr_best

lr_auc <- 
  lr_res |> 
  collect_predictions(parameters = lr_best) |> 
  roc_curve(children, .pred_children) |> 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

# 12 Tree-Based Ensemble ----

cores <- parallel::detectCores()

cores

rf_mod <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(), 
    trees = 1000
  ) |> 
  set_engine(
    "ranger",
    num.threads = cores
  ) |> 
  set_mode("classification")

rf_recipe <- 
  recipe(
    children ~ ., 
    data = hotel_other
  ) |> 
  step_date(arrival_date) |> 
  step_holiday(arrival_date) |> 
  step_rm(arrival_date)

rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)

rf_mod

extract_parameter_set_dials(rf_mod)

set.seed(345)

rf_res <- 
  rf_workflow |> 
  tune_grid(
    val_set,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

rf_res |> 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res |> 
  select_best(metric = "roc_auc")

rf_best

rf_res |> 
  collect_predictions()

rf_auc <- 
  rf_res |> 
  collect_predictions(parameters = rf_best) |> 
  roc_curve(children, .pred_children) |> 
  mutate(model = "Random Forest")

bind_rows(rf_auc, lr_auc) |> 
  ggplot(
    aes(
      x = 1 - specificity, 
      y = sensitivity,
      col = model
    )
  ) +
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_colour_viridis_d(option = "plasma", end = .6)

# 13 Last Fit -----

# Last Model
last_rf_mod <- 
  rand_forest(
    mtry = 8, 
    min_n = 7,
    trees = 1000
  ) |> 
  set_engine(
    "ranger",
    num.threads = cores,
    importance = "impurity"
  ) |> 
  set_mode("classification")

# Last Workflow
last_rf_workflow <- 
  rf_workflow |> 
  update_model(last_rf_mod)

# Last Fit
set.seed(345)

last_rf_fit <- 
  last_rf_workflow |> 
  last_fit(splits)

last_rf_fit |> 
  collect_metrics()

last_rf_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20)

last_rf_fit |> 
  collect_predictions() |> 
  roc_curve(children, .pred_children) |> 
  autoplot()
