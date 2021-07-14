# preprocessing large satellite data for storing in data folder
library(tidyverse)
library(sf)
library(stars)

raster_data <- read_stars("data/VNL_v2_npp_2020_global_vcmslcfg_c202101211500.median.tif", proxy = TRUE)

states <- st_read("data/us_states/cb_2018_us_state_5m.shp")
state_geom <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)

# enlarge region
state_geom_buffered <- state_geom %>% st_buffer(10000) # buffer by 10 km

# crop to pennsylvania region and save
raster_data <- raster_data %>% st_crop(state_geom_buffered)
write_stars(raster_data, dsn = "data/median_radiance_2020.tif")

# bonus: create plot
ggplot() + 
  geom_stars(data = raster_data, downsample = 2) + 
  geom_sf(data = state_geom, fill = "transparent", size = 1, colour = "white") +
  scale_fill_viridis_c(trans = "log10", na.value = "transparent") +
  theme_minimal() +
  labs(fill = "Median radiance")

ggsave("img/radiance.png")


