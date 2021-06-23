# Loading data, filtering for creating the pennsylvania sf
# Store sf in output folder
library(tidyverse)
library(lubridate)
library(sf)

gan_sf <- read_csv("data/GaN2020.csv") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

states <- st_read("data/us_states/cb_2018_us_state_5m.shp")
state_geom <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)
gan_state <- gan_sf %>% st_filter(state_geom)

write_rds(gan_state, "output/gan_penn.rds")

