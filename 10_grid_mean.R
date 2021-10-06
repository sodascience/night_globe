library(tidyverse)
library(sf)
library(gstat)

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
gan_grid_means$grid_mean[gan_grid_means$wthn %in% means$wthn] <- means$grid_mean

# Check if there are no more values than means
length(gan_grid_means$grid_mean[!is.na(gan_grid_means$grid_mean)]) #183!

# With observations
gan_grid_means %>% 
  mutate(pred = grid_pred_cc$var1.pred, var = grid_pred_cc$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = gan_grid_means$grid_mean)) + 
  geom_sf(data = gan_penn_means, mapping = aes(colour = sky_brightness)) +
  labs(fill = "grid_mean") +
  scale_fill_viridis_c(direction = -1, limits = c(1, 7)) + #put them on the same scale
  scale_colour_viridis_c(direction = -1)

# Only the averaged squares in the grid, without the observations
gan_grid_means %>% 
  mutate(pred = grid_pred_cc$var1.pred, var = grid_pred_cc$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = gan_grid_means$grid_mean)) +
  labs(fill = "grid_mean") +
  scale_fill_viridis_c(direction = -1, limits = c(1, 7)) + #scale needs to be like this, otherwise some boxes stay grey
  scale_colour_viridis_c(direction = -1)
