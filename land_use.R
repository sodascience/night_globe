# test for pennsylvania land use
library(tidyverse)
library(sf)
library(stars)
library(httr)
library(curl)

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

landuse <- read_stars("data/landuse.tif")

ggplot() + 
  geom_stars(data = landuse, downsample = 2) + 
  geom_sf(data = state_geom, fill = "transparent", size = 1, colour = "white") +
  theme_minimal()


gan_penn <- read_rds("output/gan_penn_sunmoon.rds")

landuse %>%
  aggregate(st_geometry(state_geom)[1], \(x) prop.table(table(x)))
  