library(tidyverse)
library(sf)
library(gstat)


# Load data
gan_grid <- read_rds("output/gan_grid_geoenrich.rds")
gan_penn <- read_rds("output/gan_penn_processed.rds")

# Extract needed info from df to make it easier
pnts  <- gan_penn$geometry
boxes <- gan_grid$geometry

# Add col to df denoting in which box the observation falls
gan_penn$wthn <- as.integer(st_within(pnts, boxes))
try <- as.integer(st_within(pnts, boxes))
table(try)

# Now also the na's have got a wthn column, so throw those away
no_na <- gan_penn %>% 
  filter(!is.na(sky_brightness))

# Check in how many boxes the observations are
unique(no_na$wthn) #183

# Calculate means within boxes
means <- no_na %>%
  group_by(wthn) %>%
  summarise(grid_mean = mean(sky_brightness, na.rm = T)) 

# Get the means to their respective grid cells and put NA in all others

gan_grid$geom2 <- st_centroid(gan_grid$geometry)
gan_grid$wthn <- as.integer(st_within(gan_grid$geom2, gan_grid$geometry))
gan_grid$grid_mean[gan_grid$wthn %in% means$wthn] <- means$grid_mean

# Check if there are no more values than means
length(gan_grid$grid_mean[!is.na(gan_grid$grid_mean)]) #183!


