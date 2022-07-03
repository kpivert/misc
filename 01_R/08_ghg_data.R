
# * Attempt to Better Understand GHG Data ---------------------------------


# * Load Packages ---------------------------------------------------------

require(tidyverse)


# * Load Data -------------------------------------------------------------

df <- read_csv(
  file = "/Users/kurtispivert/Desktop/GHG_Inventory_Output.csv"
)

skimr::skim(df)

glimpse(df)

df %>% filter(is.na(Value))

head(df)
View(df)

df %>% 
  relocate(
    Year
  ) %>% 
  arrange(
    Year, 
    Sector,  
    Subsector, 
    Subcategory,
    Variable
  )



# * Take Two: Revised Data  -----------------------------------------------

require(tidyverse)
require(readxl)


df <- read_excel(
  path = "/Users/kurtispivert/Documents/02_personal/02_ghg/00_test_data.xlsx",
  sheet = "01",
  n_max = 79
) %>% 
  mutate(Value = as.numeric(Value))

skimr::skim(df)

df %>% 
  filter(
    Sector == "Commercial Energy Use",
    Units == "MTCO2e"
    ) %>% 
  summarize(
    tot_carb = sum(Value, na.rm = TRUE)
  )
