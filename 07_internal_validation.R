# internal validation script
# Leave-one-out cross-validation for all the models
# to compare their generalization performance

library(tidyverse)
library(sf)
library(gstat)
library(xtable)

# Data loading ----
set.seed(45)
gan_jitter <- read_rds("data/gan_enriched.rds") %>% st_jitter(0.00001)
cn <- colnames(gan_jitter)

# Perform LOOCV (takes long!) ----
# only for kriging models

# LOOCV model 2
mod_vario <- read_rds("data/model_fits/mod_vario.rds")
loocv_mod2 <- krige.cv(
  formula   = sky_brightness ~ 1,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  model     = mod_vario
)
write_rds(loocv_mod2, "data/cv_results/mod2.rds")

# LOOCV model 4
mod_vario_lur <- read_rds("data/model_fits/mod_vario_lur.rds")
frm_lur <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
loocv_mod4 <- krige.cv(
  formula   = frm_lur,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  model     = mod_vario_lur
)
write_rds(loocv_mod4, "data/cv_results/mod4.rds")

# LOOCV model 6
frm_osm <- sky_brightness ~ moon_illumination + CloudCover + motorway_10km
mod_vario_osm <- read_rds("data/model_fits/mod_vario_osm.rds")
loocv_mod6 <- krige.cv(
  formula   = frm_osm,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  model     = mod_vario_osm
)
write_rds(loocv_mod6, "data/cv_results/mod6.rds")

# LOOCV model 8
frm_all <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover + motorway_10km +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
mod_vario_all <- read_rds("data/model_fits/mod_vario_all.rds")
loocv_mod8 <- krige.cv(
  formula   = frm_all,
  locations = gan_jitter %>% filter(!is.na(sky_brightness)), 
  model     = mod_vario_all
)
write_rds(loocv_mod8, "data/cv_results/mod8.rds")

# Results from disk in table ----
# Function for LOOCV of lm model (no kriging)
# From https://stackoverflow.com/a/48114343 
loocv_lm <- function(fit){
  h <- lm.influence(fit)$h
  mean((residuals(fit) / (1 - h))^2)
}

# For kriging models, use LOOCV object
mse_mod2 <- mean(read_rds("data/cv_results/mod2.rds")$residual^2, na.rm = TRUE)
mse_mod3 <- loocv_lm(read_rds("data/model_fits/mod3.rds"))
mse_mod4 <- mean(read_rds("data/cv_results/mod4.rds")$residual^2, na.rm = TRUE)
mse_mod5 <- loocv_lm(read_rds("data/model_fits/mod5.rds"))
mse_mod6 <- mean(read_rds("data/cv_results/mod6.rds")$residual^2, na.rm = TRUE)
mse_mod7 <- loocv_lm(read_rds("data/model_fits/mod7.rds"))
mse_mod8 <- mean(read_rds("data/cv_results/mod8.rds")$residual^2, na.rm = TRUE)

tibble(
  Model = 2:8, 
  LOOCVMSE = c(mse_mod2, mse_mod3, mse_mod4, mse_mod5, mse_mod6, mse_mod7, mse_mod8)
)

