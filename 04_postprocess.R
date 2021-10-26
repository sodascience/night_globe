# post-process data 
# select columns, accurately code NAs, etc
# output final dataset to output folder
library(tidyverse)
library(sf)

# read data
gan_sf <- read_rds("output/gan_penn_geoenrich.rds")

# post process
gan_sf_final <- 
  gan_sf %>% 
  mutate(
    LimitingMag = na_if(LimitingMag, -9999),
    CloudCover  = factor(CloudCover, 
                         levels = c("clear", "1/4 of sky", "1/2 of sky", "over 1/2 of sky"), 
                         ordered = TRUE),
    Constellation = factor(Constellation)
  ) %>% 
  select(sky_brightness = LimitingMag, cloud_cover = CloudCover, obs_date = UTDate, 
         obs_time = UTTime, elevation_mtr = `Elevation(m)`, sun_altitude, 
         moon_illumination, starts_with("motorway"), starts_with("buildings"),
         starts_with("trunk"),starts_with("primary"),starts_with("secondary"),
         starts_with("tertiary"),geometry)

# save data
write_rds(gan_sf_final, "output/gan_penn_processed.rds")
