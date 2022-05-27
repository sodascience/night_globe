# Enrich the GaN dataset with covariates
# - Sun information and moon elevation & illumination
# - Land use in area around observation
# Last edited 20220302 @peterlugtig & @vankesteren

# Libraries
library(tidyverse)
library(lubridate)
library(sf)
library(osmenrich)
library(suncalc)
library(stars)
library(pbapply)

# load the data
gan <- read_rds("data/gan.rds")
landuse <- read_stars("data/land_use_2019.tif") %>% mutate(land_use_2019.tif = na_if(land_use_2019.tif, ""))


# Sun and moon elevation ----
# prepare dataset for input to suncalc package
suncalc_data <- tibble(
  date = paste(gan$UTDate, gan$UTTime),
  lat  = st_coordinates(gan)[,2],
  lon  = st_coordinates(gan)[,1]
)

# add sun and moon features to the dataset
gan <-
  gan %>% 
  mutate(
    # sun azimuth in radians (direction along the horizon, measured from south to west)
    # e.g. 0 is south and pi * 3/4 is northwest
    sun_azimuth   = getSunlightPosition(data = suncalc_data)$azimuth,
    # sun altitude above the horizon in radians, 
    # e.g. 0 at the horizon and pi/2 at the zenith (straight over your head)
    sun_altitude  = getSunlightPosition(data = suncalc_data)$altitude,
    # illuminated fraction of the moon; varies from 0.0 (new moon) to 1.0 (full moon)
    moon_fraction = getMoonIllumination(date = suncalc_data$date)$fraction,
    # moon altitude above the horizon in radians
    # e.g. 0 at the horizon and pi/2 at the zenith (straight over your head)
    moon_altitude = getMoonPosition(data = suncalc_data)$altitude,
    # compute moon illumination feature
    moon_illumination = ifelse(moon_altitude > 0, moon_fraction, 0)
  )


# Land use ----
# Land use features in 2.821km radius around obs
# Prediction grid will be 5*5km -> area 25km²
# 25 = pi*r^2 -> r = sqrt(25/pi) km
gan_circles <- 
  gan %>% 
  st_geometry() %>% 
  st_buffer(sqrt(25/pi)*1000)

landuse_props <- pbsapply(gan_circles, function(circ) landuse %>% st_crop(circ) %>% table() %>% prop.table())

landuse_tbl <- 
  as_tibble(t(landuse_props)[,-1], .name_repair = "minimal") %>% 
  set_names(function(num) paste0("landtype_", num)) %>%
  select(-landtype_21)

gan <- bind_cols(gan, landuse_tbl)

# plot to show what we just did
ggplot() + 
  geom_stars(data = landuse, downsample = 2) + 
  geom_sf(data = gan_circles, fill = "transparent", colour = "black") +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  theme_minimal() +
  labs(
    fill = "Land type",
    x = "",
    y = "",
    title = "Globe at Night observation enrichment",
    subtitle = "Using circles of 25 m²"
  )


ggsave("img/gan_enrich_landuse.png", width = 8, height = 6)

# Motorways from openstreetmaps ----
# use the osmenrich package to add motorways to the dataset
# 1 km radius
gan <- gan %>%
  enrich_osm(
    name = "motorway_1km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km radius
gan <- gan %>%
  enrich_osm(
    name = "motorway_10km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km radius
gan <- gan %>%
  enrich_osm(
    name = "motorway_25km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# write to output folder
write_rds(gan, "data/gan_enriched.rds")
