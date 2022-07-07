## Tidy Models Get Started ----

# From: https://www.tidymodels.org/start/models/

# 01 Packages ----

require(tidymodels)
require(readr)
require(broom.mixed)
require(dotwhisker)
require(rstanarm)
require(extrafont)
loadfonts(quiet = TRUE)

# 02 Data ----

urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") |> 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) |> 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

urchins

urchins |> 
  skimr::skim()

# 03 Viz ----

theme_set(
  theme_minimal(
    base_family = "Roboto"
  )
)

ggplot(
  urchins, 
  aes(
    x = initial_volume, 
    y = width, 
    group = food_regime,
    col = food_regime
    )
  ) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = 0.7)

# 04 Build Model -----

# Build a model allowing two-way interactions 

width ~ initial_volume * food_regime


linear_reg()

linear_reg() |> 
  set_engine("keras")

# Build Linear Regression Model with lm

lm_mod <- linear_reg()

# From here, the model can be estimated or trained using the fit() function:

lm_fit <- 
  lm_mod |> 
  fit(width ~ initial_volume * food_regime, data = urchins)

lm_fit

tidy(lm_fit)

tidy(lm_fit) |> 
  dwplot(
    dot_args = list(size = 2, color = "purple"),
    whisker_args = list(color = "#000000"),
    vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2)
  )

# 05 Predict -----

new_points <- expand_grid(
  initial_volume = 20, 
  food_regime = c("Initial", "Low", "High")
  ) |> 
  mutate(
    food_regime = forcats::as_factor(food_regime)
  )

mean_pred <- predict(
  lm_fit, 
  new_data = new_points
)

mean_pred

conf_int_pred <- predict(
  lm_fit, 
  new_data = new_points,
  type = "conf_int"
)

conf_int_pred

plot_data <- 
  new_points |> 
  bind_cols(mean_pred) |> 
  bind_cols(conf_int_pred)

plot_data

ggplot(
  plot_data, 
  aes(
    x = food_regime
    )
  ) +
  geom_point(
    aes(y = .pred)
  ) +
  geom_errorbar(
    aes(
      ymin = .pred_lower,
      ymax = .pred_upper
    ),
    width = 0.2
  ) +
  labs(
    y = "Urchin Size"
  )

# 06 Bayesian Model with RStanarm ----

# set prior distribution

prior_dist <- student_t(df = 1)

set.seed(123)

# make parsnip model

bayes_mod <- 
  linear_reg() |> 
  set_engine(
    "stan",
    prior_intercept = prior_dist,
    prior = prior_dist
  )

# train the model

bayes_fit <- 
  bayes_mod |> 
  fit(width ~ initial_volume * food_regime, data = urchins)

print(bayes_fit, digits = 5)

tidy(bayes_fit, conf.int = TRUE)

bayes_plot_data <- 
  new_points |> 
  bind_cols(predict(bayes_fit, new_data = new_points)) |> 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(
  bayes_plot_data,
  aes(
    x = food_regime
    )
  ) +
  geom_point(aes(y = .pred), color = "purple") +
  geom_errorbar(
    aes(
      ymin = .pred_lower, 
      ymax = .pred_upper
    ), 
    width = 0.2
  ) +
  labs(
    y = "Urchin Size",
    title = "Bayesian Model with t(1) Prior Distribution"
  )

ggplot(urchins,
       aes(initial_volume, width)) +      # returns a ggplot object 
  geom_jitter() +                         # same
  geom_smooth(method = lm, se = FALSE) +  # same                    
  labs(x = "Volume", y = "Width")         # etc

