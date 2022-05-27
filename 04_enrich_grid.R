# Enrich the prediction grid with covariates
# - Moon illumination set to 0
# - Land use in grid cell
# Last edited 20220302 by @vankesteren

# Libraries
library(tidyverse)
library(lubridate)
library(osmenrich)
library(sf)
library(stars)
library(pbapply)

# load the data
grid_pred <- read_rds("data/grid_pred.rds")
landuse <- read_stars("data/land_use_2019.tif") %>% mutate(land_use_2019.tif = na_if(land_use_2019.tif, ""))


# Sun and moon elevation ----
# add moon illumination to dataset, as well as CloudCover
grid_pred <- grid_pred %>% mutate(moon_illumination = 0, CloudCover = "clear")


# Land use ----
# Land use features in grid cell
landuse_props <- pbsapply(st_geometry(grid_pred), function(cell) landuse %>% st_crop(cell) %>% table() %>% prop.table())

landuse_tbl <- 
  as_tibble(t(landuse_props)[,-1], .name_repair = "minimal") %>% 
  set_names(function(num) paste0("landtype_", num)) %>%
  select(-landtype_21)

grid_pred <- bind_cols(grid_pred, landuse_tbl)

# plot to show what we just did
ggplot() + 
  geom_stars(data = landuse, downsample = 2) + 
  geom_sf(data = grid_pred, fill = "transparent", colour = "black") +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  theme_minimal() +
  labs(
    fill = "Land type",
    x = "",
    y = "",
    title = "Grid cell observation enrichment",
    subtitle = "Using cells of 25 m²"
  )

ggsave("img/grid_enrich_landuse.png", width = 8, height = 6)


# Motorways from openstreetmaps ----
# take centroids of cells, and enrich using same procedure as before
sf_osm <- st_centroid(grid_pred) %>% select(geometry)

# 1 km radius
sf_osm <- sf_osm %>%
  enrich_osm(
    name = "motorway_1km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km radius
sf_osm <- sf_osm %>%
  enrich_osm(
    name = "motorway_10km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km radius
sf_osm <- sf_osm %>%
  enrich_osm(
    name = "motorway_25km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# then we combine with the grid
tbl_osm <- as_tibble(sf_osm) %>% select(starts_with("motorway")) 
grid_pred <- bind_cols(grid_pred, tbl_osm)


# write to output folder
write_rds(grid_pred, "data/grid_enriched.rds")
