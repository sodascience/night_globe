# sun and moon elevation
# to use to check for brightness of sun -> delete those obs
# and for moon brightness -> use as covariate
# 04 July 2021
# Peter Lugtig & @vankesteren


# Libraries
library(tidyverse)
library(lubridate)
library(sf)
library(suncalc)

# load the data
gan_penn <- read_rds("output/gan_penn.rds")

# prepare dataset for input to suncalc package
suncalc_data <- tibble(
  date = paste(gan_penn$UTDate, gan_penn$UTTime),
  lat  = st_coordinates(gan_penn)[,2],
  lon  = st_coordinates(gan_penn)[,1]
)

# add sun and moon features to the dataset
gan_penn_sunmoon <-
  gan_penn %>% 
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
    moon_illumination = ifelse(moonaltitude > 0, moonfraction, 0)
  )

# write to output folder
write_rds(gan_penn_sunmoon, "output/gan_penn_sunmoon.rds")
