# Model creation script

library(tidyverse)
library(sf)
library(gstat)
library(glmnet)

source("01_data_loading.R" )
source("02_sun_elevation.R" )
source("03_geoenrichment.R" )
source("04_postprocess.R" )
 
# Load data
gan_grid <- read_rds("output/gan_grid_geoenrich.rds")
gan_penn <- read_rds("output/gan_penn_processed.rds")

gan_penn <- gan_penn %>% filter(!is.na(sky_brightness))

# first try, just an LM
test1 <- lm(sky_brightness ~ factor(cloud_cover, ordered=FALSE)+sun_altitude+moon_illumination,data=gan_penn)
summary(test1)
# add the geoms
qplot(gan_penn$buildings_10km,gan_penn$sky_brightness)
qplot(gan_penn$buildings_10km)
test2 <- lm(sky_brightness ~ cloud_cover+sun_altitude+moon_illumination
            +motorway_1km+motorway_10km+motorway_25km
            +log(1+buildings_1km)+log(1+buildings_10km)+log(1+buildings_20km)
            ,data=gan_penn)
summary(test2)
# just add the close buildings
test3 <- lm(sky_brightness ~ cloud_cover+sun_altitude+moon_illumination
            +motorway_10km 
            +buildings_10km 
            ,data=gan_penn)
summary(test3) 

# use a lasso, the geoms are correlated
#define response variable
gan_penn_complete <- gan_penn[ !is.na(gan_penn$sky_brightness),]
y <- gan_penn_complete$sky_brightness

#define matrix of predictor variables
x <-  st_drop_geometry(gan_penn_complete)
x <-   data.matrix(x[, c('cloud_cover', 'sun_altitude', 'moon_illumination',
                         'motorway_1km', 'motorway_10km', 'motorway_25km',
                         'buildings_1km', 'buildings_10km', 'buildings_20km')]) 


cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

x <-   data.matrix(x[, c('cloud_cover', 'sun_altitude', 'moon_illumination',
                              'motorway_1km', 'motorway_10km', 'motorway_25km',
                              'buildings_1km', 'buildings_10km', 'buildings_20km')]) 

 
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


# A model without kriging
emp_vario_empty <- variogram(
  sky_brightness~1, 
  gan_penn
)
# Estimate a spherical model for the variogram
mod_vario_empty <- fit.variogram(
  emp_vario_empty, 
  vgm(psill = 1, "Sph", nugget = 1), 
  fit.method = 2
)
grid_pred_empty <- krige(
  formula   = sky_brightness ~ 1,
  locations = gan_penn %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry), cloud_cover = "clear"), 
  model     = mod_vario_empty,
  nmax      = 1,
  debug.level = -1
)
grid_pred_empty
gan_grid %>% 
  mutate(pred = grid_pred_empty$var1.pred, var = grid_pred_empty$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = pred)) + 
  geom_sf(data = gan_penn, mapping = aes(colour = sky_brightness)) +
  scale_fill_viridis_c(direction = -1, limits = c(1, 7)) +
  scale_colour_viridis_c(direction = -1)
######################

# A model with covariates
emp_vario_cov <- variogram(sky_brightness~motorway_1km+motorway_10km+motorway_25km+
                             buildings_1km+buildings_10km+buildings_20km,
                           gan_penn
)
plot(emp_vario_cov)
# Estimate a spherical model for the variogram
mod_vario_cov <- fit.variogram(
  emp_vario_cov, 
  vgm(psill = 1.5, "Sph", nugget = 1), 
#  fit.method = 2
)

# simple model for now. Needs to be replaced with the prediction data in gan_grid
grid_pred_cov <- krige(
  formula   = sky_brightness ~cloud_cover +motorway_1km,
  locations = gan_penn %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry), cloud_cover = "clear",
                                  motorway_1km=0), 
  model     = mod_vario_cov,
  nmax      = 100,
  debug.level = -1
)

# PL: how to predict the newdata?

grid_pred_cov <- krige(
  formula   = sky_brightness ~cloud_cover +motorway_1km+motorway_10km+motorway_25km+
            buildings_1km+buildings_10km+buildings_20km,
  locations = gan_penn %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry), cloud_cover = "clear",
                                  motorway_1km=0,motorway_10km=0,motorway_25km=0,
                                    buildings_1km=0,buildings_10km=0,buildings_20km=0), 
  model     = mod_vario_cov,
  nmax      = 100,
  debug.level = -1
)


grid_pred_cov
gan_grid %>% 
  mutate(pred = grid_pred_cov$var1.pred, var = grid_pred_cov$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = pred)) + 
  geom_sf(data = gan_penn, mapping = aes(colour = sky_brightness)) +
  scale_fill_viridis_c(direction = -1) +
  scale_colour_viridis_c(direction = -1)

##########################

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
  model     = mod_vario_cc,
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
