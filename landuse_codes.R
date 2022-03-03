# get land use codes
library(tidyverse)
library(rvest)

land_use_codes <- 
  read_html("https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description") %>% 
  html_table() %>% 
  first() %>% 
  filter(str_detect(X1, "[0-9]")) %>% 
  mutate(X1 = as.integer(X1), 
         X2 = str_split(X2, "-",2)) %>% 
  unnest_wider(X2, names_sep = "_") %>% 
  set_names("Code", "Label", "Description") %>% 
  mutate(across(-Code, str_trim))
