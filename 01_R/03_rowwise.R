
# * Row-Wise Operations in dplyr ------------------------------------------

library(dplyr, warn.conflicts = FALSE)

df <- tibble(x = 1:2, y = 3:4, z = 4:5)
df %>% rowwise()

df %>% mutate(m = mean(c(x, y, z)))

df %>% rowwise() %>% mutate(m = mean(c(x, y, z)))

df <- tibble(name = c("Mara", "Hadley"), x = c(9, 1), y = 3:4, z = 4:5)

df %>% 
  rowwise() %>% 
  summarise(m = mean(c(x, y, z)))

df %>% 
  rowwise(name) %>% 
  summarise(m = mean(c(x, y, z)))

df <- tibble(id = 1:6, w = runif(6), x = runif(6), y = runif(6), z = runif(6)) %>% 
  mutate(across(everything(), ~ round(.x, 3)))

df <- expand.grid(mean = c(-1, 0, 1), sd = c(1, 10, 100))

df <- df %>% 
  rowwise() %>% 
  mutate(data = list(rnorm(10, mean, sd)))
#> # A tibble: 9 x 3
#> # Rowwise: 
#>    mean    sd data      
#>   <dbl> <dbl> <list>    
#> 1    -1     1 <dbl [10]>
#> 2     0     1 <dbl [10]>
#> 3     1     1 <dbl [10]>
#> 4    -1    10 <dbl [10]>
#> # … with 5 more rows


df <- rowwise(tribble(
  ~rng,     ~params,
  "runif",  list(n = 10), 
  "rnorm",  list(n = 20),
  "rpois",  list(n = 10, lambda = 5),
))

df <- df %>% 
  mutate(data = list(do.call(rng, params)))
#> # A tibble: 3 x 3
#> # Rowwise: 
#>   rng   params           data      
#>   <chr> <list>           <list>    
#> 1 runif <named list [1]> <dbl [10]>
#> 2 rnorm <named list [1]> <dbl [20]>
#> 3 rpois <named list [2]> <int [10]>