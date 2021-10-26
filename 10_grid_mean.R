library(tidyverse)
library(sf)

#pas 10 aan, commit 10, pull 10 in main

# Load data
gan_grid_means <- read_rds("output/gan_grid_geoenrich.rds")
gan_penn_means <- read_rds("output/gan_penn_processed.rds")

# Extract needed info from df to make it easier
pnts  <- gan_penn_means$geometry
boxes <- gan_grid_means$geometry

# Add col to df denoting in which box the observation falls
gan_penn_means$wthn <- as.integer(st_within(pnts, boxes))

# Now also the na's have got a wthn column, so throw those away
no_na <- gan_penn_means %>% 
  filter(!is.na(sky_brightness))

# Check in how many boxes the observations are
sort(unique(no_na$wthn)) #183

# Calculate means within boxes
means <- no_na %>%
  group_by(wthn) %>%
  summarise(grid_mean = mean(sky_brightness, na.rm = T)) 

# Get the means to their respective grid cells and put NA in all others
gan_grid_means$geom2 <- st_centroid(gan_grid_means$geometry)
gan_grid_means$wthn <- as.integer(st_within(gan_grid_means$geom2, gan_grid_means$geometry))
gan_grid_means <- st_join(gan_grid_means, means)

# Check if there are no more values than means
length(gan_grid_means$grid_mean[!is.na(gan_grid_means$grid_mean)]) #183!
