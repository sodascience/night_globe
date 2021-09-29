library(tidyverse)
library(sf)
library(gstat)


# Load data
gan_grid <- read_rds("output/gan_grid_geoenrich.rds")
gan_penn <- read_rds("output/gan_penn_processed.rds")

gan_penn <- gan_penn %>% filter(!is.na(sky_brightness))

# kriging
# Create an empirical variogram 
emp_vario <- variogram(
  sky_brightness~1, 
  gan_penn
)

# Estimate a spherical model for the variogram
mod_vario <- fit.variogram(
  emp_vario, 
  vgm(psill = 1, "Sph", nugget = 1), 
  fit.method = 1
)

# perform kriging
grid_pred <- krige(
  formula   = sky_brightness ~ 1,
  locations = gan_penn %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario,
  nmax      = 200,
  debug.level = -1
)
grid_pred


gan_grid %>% 
  mutate(pred = grid_pred$var1.pred, var = grid_pred$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = pred)) + 
  geom_sf(data = gan_penn, mapping = aes(colour = sky_brightness)) +
  scale_fill_viridis_c(direction = -1, limits = c(2, 4.5)) +
  scale_colour_viridis_c(direction = -1)


# kriging with cloud cover
# Create an empirical variogram 
emp_vario_cc <- variogram(
  sky_brightness~cloud_cover, 
  gan_penn
)

# Estimate a spherical model for the variogram
mod_vario_cc <- fit.variogram(
  emp_vario_cc, 
  vgm(psill = 1.5, "Sph", nugget = 1), 
  fit.method = 2
)

# perform kriging
grid_pred_cc <- krige(
  formula   = sky_brightness ~ cloud_cover,
  locations = gan_penn %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry), cloud_cover = "clear"), 
  model     = mod_vario,
  nmax      = 200,
  debug.level = -1
)
grid_pred_cc


gan_grid %>% 
  mutate(pred = grid_pred_cc$var1.pred, var = grid_pred_cc$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = pred)) + 
  geom_sf(data = gan_penn, mapping = aes(colour = sky_brightness)) +
  scale_fill_viridis_c(direction = -1, limits = c(2, 4.5)) +
  scale_colour_viridis_c(direction = -1)
