# Modeling script
# Estimate models on GaN data
# Predict for the grid
# Store the results
library(tidyverse)
library(sf)
library(gstat)
library(ggpointdensity)
library(patchwork)
library(stars)

# Data loading ----
gan       <- read_rds("data/gan_enriched.rds")
grid_pred <- read_rds("data/grid_enriched.rds")
skyglow   <- read_rds("data/grid_skyglow.rds")

# jitter the points a tiny amount for kriging
set.seed(45)
gan_jitter <- gan %>% st_jitter(0.00001)

# colnames for later use
cn <- colnames(gan)

# Model fitting ----
## Model 1: Naïve model ----
# This model is just the means of the observations in each cell
# When a cell has no observations, use the overall mean

brightness_means <- 
  gan %>% 
  group_by(grid_cell = st_within(gan, grid_pred)) %>%
  summarise(sky = mean(sky_brightness, na.rm = TRUE)) %>% 
  unnest(grid_cell) %>% 
  select(grid_cell, sky)

pred_model1 <- grid_pred
pred_model1$est <- NA
pred_model1[brightness_means$grid_cell, "est"] <- brightness_means$sky
pred_model1 <- pred_model1 %>% mutate(est = replace_na(est, mean(est, na.rm = TRUE)))

## Model 2: kriging ----
# Standard kriging without any covariates

emp_vario <- variogram(sky_brightness~1, gan %>% filter(!is.na(sky_brightness)), width = 5)
mod_vario <- fit.variogram(emp_vario, vgm(psill = 1, "Mat", nugget = 1), fit.method = 1)
write_rds(mod_vario, "data/model_fits/mod_vario.rds")
krige_pred <- krige(
  formula   = sky_brightness ~ 1,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario,
  debug.level = -1
)

pred_model2 <- 
  grid_pred %>% 
  mutate(est = krige_pred$var1.pred, 
         var = krige_pred$var1.var)


## Model 3: land-use regression ----
# Standard ols regression with land-use covariates
# also using moon illumination & cloud cover as a predictor

frm_lur <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
fit_model3 <- lm(frm_lur, gan)
write_rds(fit_model3, "data/model_fits/mod3.rds")
prd3 <- predict(fit_model3, newdata = grid_pred, se.fit = TRUE)
pred_model3 <- grid_pred %>% mutate(est = prd3$fit, var = prd3$se.fit^2)


## Model 4: Universal kriging with LUR ----
# Universal Kriging with the land-use covariates
# also using moon illumination & cloud cover as a predictor

emp_vario_lur <- variogram(frm_lur, gan %>% filter(!is.na(sky_brightness)), width = 5)
mod_vario_lur <- fit.variogram(emp_vario_lur, vgm(psill = 0, "Mat", nugget = 1))
write_rds(mod_vario_lur, "data/model_fits/mod_vario_lur.rds")
krige_pred_lur <- krige(
  formula   = frm_lur,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_lur,
  debug.level = -1
)

pred_model4 <- 
  grid_pred %>% 
  mutate(est = krige_pred_lur$var1.pred, 
         var = krige_pred_lur$var1.var)


## Model 5: Using OSM covariates only ----
# Standard ols regression with motorways as covariate
# also using moon illumination & cloud cover as a predictor

frm_osm <- sky_brightness ~ moon_illumination + CloudCover + motorway_10km
fit_model5 <- lm(frm_osm, gan)
write_rds(fit_model5, "data/model_fits/mod5.rds")
prd5 <- predict(fit_model5, newdata = grid_pred, se.fit = TRUE)
pred_model5 <- grid_pred %>% mutate(est = prd5$fit, var = prd5$se.fit^2)


## Model 6: Universal kriging with OSM covariates ----
# Universal Kriging with motorway as covariate
# also using moon illumination & cloud cover as a predictor

emp_vario_osm <- variogram(frm_osm, gan %>% filter(!is.na(sky_brightness)), width = 5)
mod_vario_osm <- fit.variogram(emp_vario_osm, vgm(psill = 1, "Mat", nugget = 1), fit.method = 1)
write_rds(mod_vario_osm, "data/model_fits/mod_vario_osm.rds")
krige_pred_osm <- krige(
  formula   = frm_osm,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_osm,
  debug.level = -1
)

pred_model6 <- 
  grid_pred %>% 
  mutate(est = krige_pred_osm$var1.pred, 
         var = krige_pred_osm$var1.var)


## Model 7: Using both LUR and OSM ----
# Standard ols regression with motorways and land-use variables as covariates
# also using moon illumination & cloud cover as a predictor

frm_all <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover + motorway_10km +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
fit_model7 <- lm(frm_all, gan)
write_rds(fit_model7, "data/model_fits/mod7.rds")
prd7 <- predict(fit_model7, newdata = grid_pred, se.fit = TRUE)
pred_model7 <- grid_pred %>% mutate(est = prd7$fit, var = prd7$se.fit^2)

## Model 8: Universal kriging with both LUR and OSM ----
# Universal Kriging with motorways and land-use variables as covariates
# also using moon illumination & cloud cover as a predictor

emp_vario_all <- variogram(frm_all, gan %>% filter(!is.na(sky_brightness)), width = 5)
mod_vario_all <- fit.variogram(emp_vario_all, vgm(psill = .9, "Mat", nugget = 1), fit.method = 1)
write_rds(mod_vario_all, "data/model_fits/mod_vario_all.rds")
krige_pred_all <- krige(
  formula   = frm_all,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_all,
  debug.level = -1
)

pred_model8 <- 
  grid_pred %>% 
  mutate(est = krige_pred_all$var1.pred, 
         var = krige_pred_all$var1.var)

write_rds(pred_model8, "data/model_fits/pred_model8.rds")

# Output results ----
# create single sf with all model predictions (negative because inverted scale)
# also includes external skyglow measures
pred_sf <- 
  skyglow %>% 
  mutate(
    est_Naive_No    = -pred_model1$est,
    est_Naive_Yes   = -pred_model2$est,
    est_Landuse_No  = -pred_model3$est,
    est_Landuse_Yes = -pred_model4$est,
    est_OSM_No      = -pred_model5$est,
    est_OSM_Yes     = -pred_model6$est,
    est_Both_No     = -pred_model7$est,
    est_Both_Yes    = -pred_model8$est,
    log_skyglow     = log(skyglow),
    log_radiance    = log(radiance)
  )

write_rds(pred_sf, "data/pred_sf.rds")

# Plotting ----
## All models ----
# Function to plot predictions on map
pred_map <- function(pred_sf, title, subtitle) {
  ggplot() +
    geom_sf(data = pred_sf, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "transparent", direction = -1, guide = "none", limits = c(0, 8)) +
    theme_minimal(base_size = 14) +
    labs(title = title, subtitle = subtitle)
}

# Create plots
plt_1 <- pred_map(pred_sf = pred_model1, title = "Naïve predictions",               subtitle = "Model 1")
plt_2 <- pred_map(pred_sf = pred_model2, title = "Kriging predictions",             subtitle = "Model 2")
plt_3 <- pred_map(pred_sf = pred_model3, title = "Land-use regression predictions", subtitle = "Model 3")
plt_4 <- pred_map(pred_sf = pred_model4, title = "Land-use kriging predictions",    subtitle = "Model 4")
plt_5 <- pred_map(pred_sf = pred_model5, title = "OSM regression predictions",      subtitle = "Model 5")
plt_6 <- pred_map(pred_sf = pred_model6, title = "OSM kriging predictions",         subtitle = "Model 6")
plt_7 <- pred_map(pred_sf = pred_model7, title = "Full regression predictions",     subtitle = "Model 7")
plt_8 <- pred_map(pred_sf = pred_model8, title = "Full kriging predictions",        subtitle = "Model 8")

# Save all plots
ggsave(
  file = "img/model_predictions.png", 
  plot = (plt_1 + plt_2) / (plt_3 + plt_4) / (plt_5 + plt_6) / (plt_7 + plt_8),
  width = 10, height = 16
)

## Model 8 ----
# Detailed image of plot 8 for in the paper
states <- st_read("raw_data/us_states/cb_2018_us_state_5m.shp")
penn_border <- states %>% filter(NAME == "Pennsylvania") %>% st_geometry() %>% st_transform(4326)
ggplot() +
  geom_sf(data = pred_model8, aes(fill = pmin(pmax(est, 0), 8)), colour = "transparent") +
  geom_sf(data = penn_border, fill = "transparent") +
  scale_fill_viridis_c(na.value = "#440154FF", direction = -1, limits = c(0, 8)) +
  theme_minimal(base_size = 14) +
  labs(title = "Full kriging model predictions", fill = "Predicted skyglow")
ggsave(file = "img/pred_full_model.png", width = 10, height = 6)

# plot difference between p7 and p8
pred_model8 %>% 
  mutate(est = pred_model7$est - est) %>%
  ggplot() +
  geom_sf(aes(fill = est), colour = "transparent") +
  geom_sf(data = penn_border, fill = "transparent") +
  scale_fill_gradient2(high = scales::muted("red"), 
                       low = scales::muted("blue")) +
  theme_minimal(base_size = 14) +
  labs(title = "Kriging effect", 
       subtitle = "Difference between full models with and without kriging",
       fill = "Prediction difference\n(no kriging - kriging)")
ggsave(file = "img/kringing_effect.png", width = 10, height = 6)
