dat$`Online Resource`[1:5]
part_2[1:5]

test_case <- dat %>% 
  filter(`Online Resource` != "No Listing") %>% 
  select(`Online Resource`) %>% 
  filter(str_detect(`Online Resource`, "\\'c\\(")) %>% 
  as_vector()

part_2 <- str_extract(
  test_case,
  "target.+"
)

test_case %>% 
  mutate(
    `Online Resource` = case_when(
      str_detect(`Online Resource`, "\\'c\\(") == TRUE ~  str_c(
        "<a href = '",
        str_extract(
          `Online Resource`,
          "(?<=c\\()(.*?)(?=,)"
        ) %>% 
          str_sub(2, -2), 
        "' ",
        str_extract(
          `Online Resource`,
          "target.+"
        )),
      TRUE ~ `Online Resource`
      )
    ) %>% 
  View()


test_case <- dat %>% 
  filter(`Online Resource` != "No Listing") %>% 
  select(`Online Resource`) %>% 
  filter(str_detect(`Online Resource`, "\\'c\\(")) 



str_c(
  "<a href = '",
  str_extract(
  test_case,
  "(?<=c\\()(.*?)(?=,)"
) %>% 
  str_sub(2, -2), 
"' ",
str_extract(
  test_case,
  "target.+"
))



part_1 <- str_extract(
  test_case,
  "([^,]+)"
) 

part_1 <- str_replace(
  part_1,
     pattern = "c\\(",
    ""
  ) 

  str_replace(part_1,
    pattern = "\\",
    ""
  )

a <- str_view_all(
  test_case$`Online Resource`, 
  pattern = "\\'c"
)
 
str_view_all(
  test_case$`Online Resource`,
  pattern = "http"
)
 
  mutate(`Online Resource` = gsub(`Online Resource`, "c(\", ")) %>% print(n =65)



  dat %>% 
  mutate(
    `Online Resource` = case_when(
      str_detect(`Online Resource`, "^https://id.loc.gov") == TRUE ~ 
        str_c(
          "<a href ='",
          `Online Resource`,
          "' target='_blank'>Library of Congress</a>" 
        ),
      TRUE ~ `Online Resource`
    )) %>% 
    select(`Online Resource`) %>% 
  filter(str_detect(`Online Resource`, "id.loc")) 


dat %>% filter(str_detect(`Online Resource`, "dbpedia.org/")) %>% 
  select(`Online Resource`) %>% View()
