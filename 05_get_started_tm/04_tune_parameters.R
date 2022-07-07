## Tidy Models Tune Model Parameters ----

# From: https://www.tidymodels.org/start/tuning/

# Hyperparameters---Parameters that cannot be learned from data set during model training
# e.g., mtry = number of predictors sampled at splits in tree-based models

# 01 Packages -----

require(tidymodels)
require(rpart)
require(rpart.plot)
require(vip)

# 02 Data -----

data(cells, package = "modeldata")

# 03 Vars to Tune and Train/Test Split ----

# Hyperparameters for Tuning Decision Trees:
# - Complexity parameter -> cost_complexity
#   - Prunes tree by adding cost/penalty for more complex trees
# - Max Tree Depth -> tree_depth

set.seed(123)

cell_split <- initial_split(
  cells |>
    select(-case),
  strata = class
)

cell_train <- training(cell_split)
cell_test <- testing(cell_split)

# 04 Hyperparameter Tuning ----

tune_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) |>
  set_engine("rpart") |>
  set_mode("classification")

tune_spec

# Set Up Grid Search

tree_grid <-
  grid_regular(
    cost_complexity(),
    tree_depth(),
    levels = 5
  )

tree_grid |>
  count(tree_depth)

set.seed(234)

cell_folds <- vfold_cv(cell_train)

cell_folds

# 05 Tuning with a Grid ----

# There are several options for building the object for tuning:
# - Tune a model specification along with a recipe or model, or
# - Tune a workflow() that bundles together a model specification
#   and a recipe or model preprocessor.

# Using Workflow

set.seed(345)

tree_wf <-
  workflow() |>
  add_model(tune_spec) |>
  # If there is preprocessing, then use add_recipe()
  add_formula(class ~ .)

tree_res <-
  tree_wf |>
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )

tree_res

# 06 Assess Metrics -----

tree_res |>
  collect_metrics()

tree_res |>
  collect_metrics() |>
  mutate(tree_depth = factor(tree_depth)) |>
  ggplot(
    aes(
      x = cost_complexity,
      y = mean,
      color = tree_depth
      )
    ) +
  geom_line(
    size =  1.2,
    alpha = 0.6
  ) +
  geom_point(
    size = 2
    ) +
  facet_wrap(
    ~ .metric,
    scales = "free",
    nrow = 2
     ) +
  scale_x_log10(
    labels = scales::label_number()
  ) +
  scale_color_viridis_d(
    option = "plasma",
    begin = 0.9,
    end = 0
  ) +
  theme_minimal()

# 06 Pick Best Hyperparameter Values ----

tree_res |>
  show_best("accuracy")

# 07 Automatically Pull Best Model Values Into Model -----

best_tree <- tree_res |>
  select_best("accuracy")

best_tree

# 08 Finalize Model with Best Hyperparameters -----

final_wf <-
  tree_wf |>
  finalize_workflow(best_tree)

final_wf

# 09 Fit Final Model to Training and Assess on Fit -----

final_fit <-
  final_wf |>
  last_fit(cell_split)

final_fit |>
  collect_metrics()

final_fit |>
  collect_predictions() |>
  roc_curve(class, .pred_PS) |>
  autoplot()

# 10 Extract Final Model for Predicting ----

final_tree <- extract_workflow(final_fit)

final_tree

final_tree |>
  extract_fit_engine() |>
  rpart.plot(roundint = FALSE)


# 11 Variable Importance Plot -----

final_tree |>
  extract_fit_parsnip() |>
  vip()

# Available Hyperparameters to tune

args(decision_tree)
args(logistic_reg)
