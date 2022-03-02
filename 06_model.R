# Modeling script
# Estimate model on GaN data
# Predict for the grid
# Compare to remote sensed skyglow

library(tidyverse)
library(sf)
library(gstat)

# load data
gan       <- read_rds("data/gan_enriched.rds")
grid_pred <- read_rds("data/grid_enriched.rds")
skyglow   <- read_rds("data/grid_skyglow.rds")

# Model 1: Naïve model - just means of obs in each cell
brightness_means <- 
  gan %>% 
  group_by(grid_cell = st_within(gan, grid_pred)) %>%
  summarise(sky = mean(sky_brightness, na.rm = TRUE)) %>% 
  unnest(grid_cell) %>% 
  select(grid_cell, sky)

pred_model1 <- grid_pred
pred_model1$est <- NA
pred_model1[brightness_means$grid_cell, "est"] <- brightness_means$sky

ggplot() +
  geom_sf(data = pred_model1, aes(fill = est)) +
  scale_fill_viridis_c(na.value = "transparent", direction = -1) +
  theme_minimal() +
  labs(
    title = "Naïve predictions"
    
  )
