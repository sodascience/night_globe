# making prediction grid for pennsylvania
library(tidyverse)
library(sf)

# create metric projected state geometry for pennsylvania
state_proj <- 
  st_read("data/us_states/cb_2018_us_state_5m.shp") %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_geometry() %>% 
  st_transform(32717) # metric projection

# create a 5x5km grid
grid_penn <- st_make_grid(
  x        = state_proj, 
  cellsize = c(5000, 5000) # in meters, because metric projection
)

# select only intersection of the grid, 
# we don't want cells outside pennsylvania
grid_penn <- st_intersection(
  x = grid_penn,
  y = state_proj
)

# set default crs and transform to sf
grid_penn <- grid_penn %>% st_transform(4326) %>% st_sf()

# store in data folder
write_rds(grid_penn, "output/grid_penn.rds")

