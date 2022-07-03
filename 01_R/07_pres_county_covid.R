
# * 2016 Election Data and COVID-19 Cases  --------------------------------

# Election Data

# MIT Election Data and Science Lab, 2018, 
# "County Presidential Election Returns 2000-2016", 
# https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V6, 
# UNF:6:ZZe1xuZ5H2l4NUiSRcRf8Q== [fileUNF]

# COVID Data
# https://github.com/nytimes/covid-19-data
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv

# * Load Packages ---------------------------------------------------------

require(tidyverse)
require(extrafont)
loadfonts(quiet = TRUE)


# * Presidential Elections by County --------------------------------------

df <- read_csv(
  here::here("00_data", "countypres_2000-2016.csv")
)

c_df <- read_csv(
  here::here("00_data", "covid")
)

# * Focus on 2016 ---------------------------------------------------------

skimr::skim(df)

df <- df %>% 
  filter(year == 2016) %>% 
  mutate(county = str_to_lower(county)) %>% # for merging with county data
  group_by(FIPS) %>% 
  filter(candidatevotes == max(candidatevotes))  %>% # county winners
  ungroup()

# 12 Counties Where NO Max vote

test <- df %>% 
  group_by(FIPS) %>% 
  filter(candidatevotes == max(candidatevotes)) 

anti_join(df, test, by = "FIPS")

# * County Shape Files ----------------------------------------------------

fips <- map_data("county")

df %>% 
  left_join(fips, by = c("county" = "subregion")) %>% 
  ggplot(
    aes(
      x = long, 
      y = lat)
  ) +
  geom_polygon(
    aes(
      group = group,
      fill = party
    )
  ) +
  coord_map(
    "albers",
    at0 = 45.5,
    lat1 = 29.5
  ) +
  scale_fill_manual(
    values = c("republican" = "red", "democrat" = "blue")
  ) +
  theme_void()

ggplot(fips, aes(long, lat)) +
  coord_map() +
  geom_polygon(aes(group = group))




