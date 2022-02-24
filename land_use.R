# test for pennsylvania land use
library(tidyverse)
library(sf)
library(stars)
library(httr)
library(curl)
library(pbapply)

# get pennsylvania
states <- st_read("data/us_states/cb_2018_us_state_5m.shp")
state_geom <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)

# enlarge region
state_geom_buffered <- state_geom %>% st_buffer(10000) # buffer by 10 km

# get land use
wms_url <- parse_url("https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?service=WMS")
wms_url$query <- list(
  request = "GetMap",
  CRS = "EPSG:4326",
  bbox = paste(unname(st_bbox(state_geom_buffered)), collapse = ","),
  width = 5000,
  height = 5000,
  layers = "NLCD_2019_Land_Cover_L48",
  format = "image/geotiff"
)

tif_loc <- curl_download(build_url(wms_url), "data/landuse.tif", quiet = FALSE)

landuse <- read_stars("data/landuse.tif", proxy = TRUE)

landuse_s <- st_as_stars(landuse, downsample = 10L)



ggplot() + 
  geom_stars(data = landuse, downsample = 10L) + 
  geom_sf(data = state_geom, fill = "transparent", size = 1, colour = "white") +
  theme_minimal()


grid_penn <- read_rds("output/grid_penn.rds")

droplevels(landuse) %>% st_crop(st_geometry(grid_penn)[1]) %>% plot
landuse_grid <- aggregate(droplevels(landuse), st_geometry(grid_penn)[1], FUN = \(x) list(prop.table(table(x)))) %>% st_as_sf()
landuse_grid

droplevels(landuse_s) %>% st_intersects(x) %>% plot

res <- sapply(st_geometry(grid_penn), \(x) droplevels(landuse_s) %>% st_crop(x) %>% table %>% prop.table() %>% unname())

grid_penn_landuse <- grid_penn %>% bind_cols(landuse = as_tibble(t(res)))
grid_penn_landuse

write_rds(grid_penn_landuse, "output/grid_penn_landuse.rds")

plt <- grid_penn_landuse %>% 
  pivot_longer(starts_with("V")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = value), color = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal()

ggsave(plot = plt, "img/landuse_features.png", width = 12, height = 9)
