library(tidyverse)
library(sf)
library(osmdata)
states <- st_read("raw_data/us_states/cb_2018_us_state_5m.shp")
penn_border <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)
penn_buffer <- penn_border %>% st_buffer(10000) # buffer by 10 km


bb <- st_bbox(penn_buffer)

query <- 
  opq(bb, timeout = 60) |> 
  add_osm_feature("highway", "motorway")
ways <- osmdata_sf(query)

ggplot() +
  geom_sf(data = ways$osm_lines, colour = "grey", size = 1) +
  geom_sf(data = penn_border, fill = "transparent") +
  scale_colour_viridis_c(trans = "log10", na.value = "transparent", begin = 1, end = 0) +
  theme_minimal(base_size = 14) +
  labs(
    title    = "Highways extracted from OpenStreetMaps", 
    subtitle = "Pennsylvania"
  )

ggsave("img/raw_highway.png", width = 10, height = 6)
