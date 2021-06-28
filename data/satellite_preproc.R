# preprocessing large satellite data for storing in data folder
library(tidyverse)
library(sf)
library(stars)

raster_data <- read_stars("data/VNL_v2_npp_2020_global_vcmslcfg_c202101211500.median.tif", proxy = TRUE)

states <- st_read("data/us_states/cb_2018_us_state_5m.shp")
state_geom <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)

# crop to pennsylvania region and save
raster_data <- raster_data %>% st_crop(state_geom)
write_stars(raster_data, dsn = "data/median_radiance_2020.tif")

# bonus: create plot
ggplot() + 
  geom_sf(data = state_geom) +
  geom_stars(data = raster_data) + 
  scale_fill_viridis_c(trans = "log10", na.value = "transparent") +
  theme_minimal() +
  labs(fill = "Median radiance")

ggsave("img/radiance.png")
