# Script to load and preprocess all needed resources
# we will generally use CRS 4326 for all datasets
# output all files in data/ folder
# (c) ODISSEI Social Data Science team
# Last edited: 20220302 by @vankesteren
library(tidyverse)
library(sf)
library(stars)
library(httr)
library(curl)

# Region of interest ----
# Obtain the border of Pennsylvania
states <- st_read("raw_data/us_states/cb_2018_us_state_5m.shp")
penn_border <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)
penn_buffer <- penn_border %>% st_buffer(10000) # buffer by 10 km

# Globe at night data ----
# the observations from the globe at night project in 2020
# only in Pennsylvania
gan_cols <- cols(
  ID              = col_integer(),
  ObsType         = col_factor(),
  ObsID           = col_integer(),
  Latitude        = col_double(),
  Longitude       = col_double(),
  `Elevation(m)`  = col_double(),
  LocalDate       = col_date(format = ""),
  LocalTime       = col_time(format = ""),
  UTDate          = col_date(format = ""),
  UTTime          = col_time(format = ""),
  LimitingMag     = col_double(),
  SQMReading      = col_double(),
  SQMSerial       = col_character(),
  CloudCover      = col_factor(levels = c("clear", "1/4 of sky", "1/2 of sky", "over 1/2 of sky"), ordered = TRUE),
  Constellation   = col_factor(),
  SkyComment      = col_character(),
  LocationComment = col_character(),
  Country         = col_factor()
)

# store data
gan <- 
  read_csv("raw_data/GaN2020.csv", col_types = gan_cols) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_filter(penn_border) %>% 
  rename(sky_brightness = LimitingMag) %>% 
  mutate(sky_brightness = ifelse(sky_brightness < 0, NA, sky_brightness))

# NB: sometimes "country" is not US, but the coordinates do show the US.
write_rds(gan, "data/gan.rds")


# Raw data plot
ggplot() +
  geom_sf(data = gan, aes(colour = sky_brightness)) +
  geom_sf(data = penn_border, fill = "transparent") +
  scale_colour_viridis_c(trans = "log10", na.value = "transparent", begin = 1, end = 0) +
  theme_minimal(base_size = 14) +
  labs(
    colour   = "Sky brightness", 
    title    = "Globe at Night observations in 2020", 
    subtitle = "Pennsylvania"
  )
ggsave("img/raw_gan.png", width = 10, height = 6)


# Radiance data ----
# This is the data that we will transform into skyglow for use as "gold standard". 
radiance_raster <- read_stars("raw_data/VNL_v2_npp_2020_global_vcmslcfg_c202101211500.median.tif", proxy = TRUE)

# crop to pennsylvania region and save
radiance_penn <- radiance_raster %>% st_crop(penn_buffer)
write_stars(radiance_penn, dsn = "data/median_radiance_2020.tif")

# Raw data plot
ggplot() + 
  geom_stars(data = radiance_penn, downsample = 2) + 
  geom_sf(data = penn_border, fill = "transparent", size = 1, colour = "white") +
  scale_fill_viridis_c(trans = "log10", na.value = "transparent") +
  theme_minimal(base_size = 14) +
  labs(
    fill     = "Median radiance",
    title    = "Satellite-measured radiance in 2020",
    subtitle = "Pennsylvania"
  )

ggsave("img/raw_radiance.png", width = 10, height = 6)


# Landuse data ----
# Load landuse data in Pennsylvania as a stars object
# for classes see https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description

# download the landuse data using wms service
wms_url <- parse_url("https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?service=WMS")
wms_url$query <- list(
  request = "GetMap",
  CRS     = "EPSG:4326",
  bbox    = paste(unname(st_bbox(penn_buffer)), collapse = ","),
  width   = 2000,
  height  = 2000,
  layers  = "NLCD_2019_Land_Cover_L48",
  format  = "image/geotiff"
)
tif_loc <- curl_download(build_url(wms_url), "raw_data/landuse.tif", quiet = FALSE)

landuse_raster <- read_stars("raw_data/landuse.tif")
landuse_penn <- 
  landuse_raster %>% 
  st_crop(penn_buffer) %>% 
  mutate(landuse.tif = droplevels(landuse.tif))

write_stars(landuse_penn, dsn = "data/land_use_2019.tif")

# Raw data plot
ggplot() + 
  geom_stars(data = landuse_penn, downsample = 2) + 
  geom_sf(data = penn_border, fill = "transparent", size = 1, colour = "white") +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  theme_minimal(base_size = 14) +
  labs(
    fill = "Land type",
    x = "",
    y = "",
    title = "Land use in 2019",
    subtitle = "Pennsylvania"
  )

ggsave("img/raw_landuse.png", width = 8, height = 6)
