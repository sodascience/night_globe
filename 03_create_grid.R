# making prediction grid for pennsylvania
library(tidyverse)
library(sf)

# create metric projected state geometry for pennsylvania
penn_proj <- 
  st_read("raw_data/us_states/cb_2018_us_state_5m.shp") %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_geometry() %>% 
  st_transform(32717) # metric projection

# create a 5x5km grid
grid_penn <- st_make_grid(
  x        = penn_proj, 
  cellsize = c(5000, 5000) # in meters, because metric projection
)

# select only intersection of the grid, 
# we don't want cells outside pennsylvania
grid_penn <- st_intersection(
  x = grid_penn,
  y = penn_proj
)

# set default crs and transform to sf
grid_pred <- grid_penn %>% st_transform(4326) %>% st_sf()

# plot
ggplot() +
  geom_sf(data = penn_proj, size = 1, fill = "#BAFADA", colour = "black") +
  geom_sf(data = grid_pred, fill = "transparent") + 
  theme_minimal() + 
  labs(title = "Prediction grid with 5 x 5 km cells", subtitle = "CRS EPSG:32717")

ggsave("img/grid_pred.png", width = 8, height = 6)

# store in data folder
write_rds(grid_pred, "data/grid_pred.rds")