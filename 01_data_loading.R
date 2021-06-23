library(tidyverse)
library(lubridate)
library(sf)
library(osmenrich)
library(gstat)

gan_sf <- read_csv("https://www.globeatnight.org/2020data/GaN2020.csv") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

states <- st_read("data/us_states/cb_2018_us_state_5m.shp")
state_geom <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)
gan_state <- gan_sf %>% st_filter(state_geom)


gan_state <- 
  gan_state %>% 
  rowwise() %>% 
  mutate(LimitingMag = na_if(LimitingMag, -9999),
         sun_elev = st_sun_elevation(LocalDate, LocalTime, geometry))
  

gan_state %>% 
  ggplot() + 
  geom_sf(data = state_geom) +
  geom_sf(aes(colour = LimitingMag)) +
  scale_colour_gradient(low = "#DDDD88", high = "#121233", guide = FALSE) +
  labs(colour = "Darkness") +
  theme(legend.position = "top")


# quick regression thingy
res <- residuals(lm(LimitingMag ~ CloudCover + sun_elev, data = gan_state))
gan_state[as.numeric(names(res)),"res"] <- res
gan_state <- st_as_sf(gan_state)
  

gan_state %>% 
  mutate(resid = res) %>% 
  ggplot() + 
  geom_sf(data = state_geom) +
  geom_sf(aes(colour = resid)) +
  labs(colour = "Darkness") +
  scale_color_gradient2() +
  theme(legend.position = "top")



gan_state_enriched <- 
  gan_state %>% 
  enrich_osm(
    name = "streets",
    key = "highway", 
    type = "lines",
    r = 500, 
    kernel = "gaussian"
  )


gan_state_grid <- st_make_grid(state_geom, cellsize = .1)
