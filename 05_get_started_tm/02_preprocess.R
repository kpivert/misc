## Tidy Models Preprocessing ----

# From: https://www.tidymodels.org/start/recipes/

# 01 Packages ----

library(tidymodels)      # for the recipes package, along with the rest of tidymodels

# Helper packages
library(nycflights13)    # for flight data
# library(skimr)           # for variable summaries

# 02 Munge Data ----

set.seed(123)

flight_data <- 
  flights |> 
  mutate(
    arr_delay = if_else(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = lubridate::as_date(time_hour)
  ) |> 
  inner_join(
    weather, 
    by = c("origin", "time_hour")
    )  |> 
  select(
    dep_time, flight, origin, 
    dest, air_time, distance, 
    carrier, date,
    arr_delay, time_hour
  ) |> 
  na.omit() |> 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )

flight_data |> 
  count(arr_delay) |> 
  mutate(pct = n / sum(n))

flight_data |> 
  glimpse()

flight_data |> 
  skimr::skim(dest, carrier)

# 03 Split Data ----

set.seed(222)

data_split <- initial_split(flight_data, prop = 3/4)

train_data <- training(data_split)
test_data <- testing(data_split)

# 04 Recipes and Roles ----

# Initiate a Recipe & Change Roles for non-predictive Vars

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) |> 
  # Very Cool. Retains the data, but converts to an ID to help interpret results, 
  # Won't be used in developing model
  update_role(flight, time_hour, new_role = "ID")

summary(flights_rec) # Also very cool. Provides Current Vars and their Roles

# 05 Feature Engineering -----

flight_data |> 
  distinct(date) |> 
  mutate(numeric_date = as.numeric(date))

test_data |> 
  distinct(dest) |> 
  anti_join(train_data)

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) |> 
  update_role(flight, time_hour, new_role = "ID") |> 
  # Adds two factor columsn: day of week and month
  step_date(date, features = c("dow", "month")) |>
  # Holiday Flag and remove original date col
  step_holiday(
    date, 
    holidays = timeDate::listHolidays("US"), 
    keep_original_cols = FALSE
  ) |> 
  step_dummy(all_nominal_predictors()) |> 
  # ALWAYS PERFORM ZERO VARIANCE AFTER DUMMYING !!!!
  step_zv(all_predictors())

# 06 Fit Model with Recipe ----

lr_mod <- 
  logistic_reg() |> 
  set_engine("glm")

# 07 Use Workflow to Pair Model & Recipe ----

## Process Recipe with Training Set ----
## Apply Recipe to Training Set ----
## Apply Recipe to Testing Set ----

flights_wflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(flights_rec)

flights_wflow

flights_fit <- 
  flights_wflow |> 
  fit(data = train_data)

# Can use extract_fit_parsnip() to extract model object 
# Can use extract_recipe() to extract recipe 

flights_fit |> 
  extract_fit_parsnip() |> 
  tidy()

# 08 Predictions -----

predict(flights_fit, test_data)

flights_aug <- 
  augment(flights_fit, test_data)

flights_aug |> 
  glimpse()

flights_aug |> 
  select(
    arr_delay,
    time_hour, 
    .pred_class, 
    .pred_on_time
  )

# 09 Model Evaluation ----

flights_aug |> 
  roc_curve(truth = arr_delay, .pred_late) |> 
  autoplot()

flights_aug |> 
  roc_auc(truth = arr_delay, .pred_late)


# 10 Using All Predictors ----

workflows::add_formula()