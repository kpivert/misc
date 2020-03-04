
# * Tod Paper Viz ---------------------------------------------------------


# * Load Packages ---------------------------------------------------------

require(sf)
require(mapview)
require(tidyverse)
require(ggmap)

# * Load Data -------------------------------------------------------------

df <- tibble::tribble(
                            ~Newspaper,                  ~Owner,             ~City, ~State,    ~Daily,   ~Sunday,
                           "USA Today",               "Gannett",      "Washington",   "DC",   1400190,    728675,
             "The Wall Street Journal",             "News Corp",        "New York",   "NY",   1008398,   1003155,
                  "The New York Times",    "New York Times Co.",        "New York",   "NY",    514986,    893666,
                   "Los Angeles Times", "(Patrick Soon-Shiong)",     "Los Angeles",   "CA",    444233,    655276,
                       "New York Post",             "News Corp",        "New York",   "NY",    407655,    398389,
                     "Chicago Tribune",               "Tribune",         "Chicago",   "IL",    282710,    506668,
                    "The Boston Globe",          "(John Henry)",          "Boston",   "MA",    256340,    320852,
            "Minneapolis Star Tribune",         "(Glen Taylor)",     "Minneapolis",   "MN",    249111,    371417,
                 "The Washington Post",          "(Jeff Bezos)",      "Washington",   "DC",    230867,    371525,
           "The Philadelphia Inquirer",     "Lenfest Institute",    "Philadelphia",   "PA",    230721,    293237,
                   "The Seattle Times",      "(Blethen family)",         "Seattle",   "WA",    200170,    248000,
                             "Newsday",       "(Patrick Dolan)",        "Melville",   "NY",    185302,    220277,
             "San Francisco Chronicle",                "Hearst",   "San Francisco",   "CA",    181055,    204332,
                     "The Denver Post",                   "MNG",          "Denver",   "CO",    173692,    240562,
                   "Houston Chronicle",                "Hearst",         "Houston",   "TX",    170406,    227700,
                     "Tampa Bay Times",     "Poynter Institute",  "St. Petersburg",   "FL",    158199,    246023,
                     "The Star-Ledger",               "Advance",          "Newark",   "NJ",    153293,    174934,
             "The Dallas Morning News",             "A.H. Belo",          "Dallas",   "TX",    149195,    177410,
                 "New York Daily News",               "Tribune",        "New York",   "NY",    147917,    181486,
            "Honolulu Star-Advertiser",           "Black Press",        "Honolulu",   "HI",    147204,    150881,
                    "Orlando Sentinel",               "Tribune",         "Orlando",   "FL",    118262,    120932,
              "Cleveland Plain Dealer",               "Advance",       "Cleveland",   "OH",    117248,    182730,
                    "Arizona Republic",               "Gannett",         "Phoenix",   "AZ",    116403,    178932,
         "The San Diego Union-Tribune", "(Patrick Soon-Shiong)",       "San Diego",   "CA",    116247,    180219,
                    "Kansas City Star",             "McClatchy",     "Kansas City",   "MO",    111860,    124239,
          "South Florida Sun-Sentinel",               "Tribune", "Fort Lauderdale",   "FL",    110316,    139629,
              "St. Paul Pioneer Press",                   "MNG",        "St. Paul",   "MN",    107766,    170663,
                   "Chicago Sun-Times",       "Sun-Times Media",         "Chicago",   "IL",    102934,    109353,
                     "Virginian-Pilot",               "Tribune",         "Norfolk",   "VA",    101648,    104998,
                    "The Mercury News",                   "MNG",        "San Jose",   "CA",    100827,    146102,
        "Atlanta Journal-Constitution",                   "Cox",         "Atlanta",   "GA",     99546,    159760,
             "St. Louis Post-Dispatch",                   "Lee",       "St. Louis",   "MO",     97697,    133279,
                  "Portland Oregonian",               "Advance",        "Portland",   "OR",     96194,    126917,
             "Pittsburgh Post-Gazette",                 "Block",      "Pittsburgh",   "PA",     92854,    121071,
                    "Hartford Courant",               "Tribune",        "Hartford",   "CT",     92813,    102847,
                      "Sacramento Bee",             "McClatchy",      "Sacramento",   "CA",     92123,    129571,
           "Arkansas Democrat Gazette",                 "WEHCO",     "Little Rock",   "AR",     91454,     95688,
            "Las Vegas Review-Journal",     "(Sheldon Adelson)",       "Las Vegas",   "NV",     89068,    108800,
                    "The Miami Herald",             "McClatchy",           "Miami",   "FL",     88261,    119460,
          "Milwaukee Journal Sentinel",               "Gannett",       "Milwaukee",   "WI",     87517,    134565,
                  "Detroit Free Press",               "Gannett",         "Detroit",   "MI",     86116,    174459,
                  "Omaha World-Herald",                   "Lee",           "Omaha",   "NE",     81793,     93093,
          "New Orleans Times-Picayune",        "(John Georges)",     "New Orleans",   "LA",     81638,    104562,
                   "Columbus Dispatch",               "Gannett",        "Columbus",   "OH",     79622,    104507,
              "Orange County Register",                   "MNG",       "Santa Ana",   "CA",     78699,    165303,
                      "East Bay Times",                   "MNG",         "Oakland",   "CA",     78254,    123725,
              "Allentown Morning Call",               "Tribune",       "Allentown",   "PA",     76003,     79938
        )


# * Viz  ------------------------------------------------------------------

df %>% 
  group_by(Owner) %>% 
  summarize(
    `Total Daily` = sum(Daily)
  ) %>% 
  ggplot(
    aes(
      x = fct_reorder(Owner, `Total Daily`),
      y= `Total Daily`
    )
  ) +
  geom_col(
    width = .4
  ) + 
  coord_flip()

locations_df <- unique(df$City) %>%
  enframe() %>% 
  select(value)

locations_df <- mutate_geocode(
  locations_df,
  value
)


# * Save Geocoded Data ----------------------------------------------------

write_rds(
  locations_df, 
  path = here::here(
    "00_data",
    "paper-viz-geocoded.rds"
  )
)  



# * Viz -------------------------------------------------------------------


locations_df <- locations_df %>% 
  left_join(
    .,
    df,
    by = c("value" = "City")
  ) 

locations_sf <- st_as_sf(
  locations_df, 
  coords = c("lon", "lat"),
  crs = 4326
  )

mapview(locations_sf)

locations_sf  

map <- get_googlemap(center = "united states")
ggplot  



