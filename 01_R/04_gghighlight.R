require(tidyverse)
library(ggforce)
library(ggridges)
require(gghighlight)
ggplot(mutate(df_marital, marital = fct_rev(marital)), aes(x = age, y = ..count..)) +
  geom_density_line(data = select(df_marital, -marital), aes(fill = "all people surveyed  "), color = "transparent") +
  geom_density_line(aes(fill = "highlighted group"), color = "transparent") +
  facet_wrap(~marital, nrow = 1) +
  scale_x_continuous(name = "age (years)", limits = c(15, 98), expand = c(0, 0)) +
  scale_y_continuous(name = "count", expand = c(0, 0)) +
  scale_fill_manual(
    values = c("#b3b3b3a0", "#2b8cbed0"),
    name = NULL,
    guide = guide_legend(direction = "horizontal")
  ) +
  coord_cartesian(clip = "off") +
  theme_dviz_hgrid() +
  theme(
    axis.line.x = element_blank(),
    strip.text = element_text(size = 14, margin = margin(0, 0, 0.2, 0, "cm")),
    legend.position = "bottom",
    legend.justification = "right",
    legend.margin = margin(4.5, 0, 1.5, 0, "pt"),
    legend.spacing.x = grid::unit(4.5, "pt"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.box.spacing = grid::unit(0, "cm")
  )


ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_histogram(bins = 50) +
  gghighlight() +
  facet_wrap(~ Species)

df <- read_feather("03_reports/carnegie_gl/name_instrument.feather")

df <- df %>% 
  count(inst) %>% 
  arrange(desc(n)) %>% 
  filter(n > 500) 

df <- expand_grid(
  x = c("Europe", "Africa", "North America"),
  y = c("Trombone", "Trumpet", "Tuba")
  ) %>% 
  mutate(
    num = runif(9, 100, 400)
  )

ggplot(
  df,
  aes(
    x = y, 
    fill = x,
    y = num
  )
  ) +
    geom_col() +
    gghighlight() +
  facet_wrap(~ x)

ggplot(
  filter(df, x == "Africa"),
  aes(
    x = y, 
    fill = x,
    y = num
  )
) +
  geom_col() +
  gghighlight() +
  facet_wrap(~ x)
\