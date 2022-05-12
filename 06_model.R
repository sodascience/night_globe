# Modeling script
# Estimate model on GaN data
# Predict for the grid
# Compare to remote sensed skyglow

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

# Function for LOOCV of lm model ---
# From Stack Overflow https://stackoverflow.com/a/48114343 
loocv <- function(fit){
  h <- lm.influence(fit)$h
  mean((residuals(fit) / (1 - h))^2)
} # mean squared error 

# Model fitting ----
## Model 1: Naïve model ----
# just means of obs in each cell
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

(plt_mod1 <-
  ggplot() +
  geom_sf(data = pred_model1, aes(fill = est), colour = "transparent") +
  scale_fill_viridis_c(na.value = "transparent", direction = -1, guide = "none", limits = c(1, 7)) +
  theme_minimal() +
  labs(title = "Naïve predictions", subtitle = "Model 1"))

## Model 2: kriging ----
# Create an empirical variogram 
emp_vario <- variogram(sky_brightness~1, gan %>% filter(!is.na(sky_brightness)))

# Estimate a spherical model for the variogram
mod_vario <- fit.variogram(emp_vario, vgm(psill = 1, "Sph", nugget = 1), fit.method = 1)

# perform kriging
krige_pred <- krige(
  formula   = sky_brightness ~ 1,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.00001), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario,
  debug.level = -1
)

krige_pred_gan <- krige.cv(
  formula   = sky_brightness ~ 1,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.00001), 
  model     = mod_vario,
  debug.level = -1
)

krige_pred
pred_model2 <- 
  grid_pred %>% 
  mutate(est = krige_pred$var1.pred, 
         var = krige_pred$var1.var)

(plt_mod2 <- 
  ggplot() +
  geom_sf(data = pred_model2, aes(fill = est), colour = "transparent") +
  scale_fill_viridis_c(na.value = "transparent", direction = -1, guide = "none", limits = c(1, 7)) +
  theme_minimal() +
  labs(title = "Kriging predictions", subtitle = "Model 2"))

## Model 3: land-use regression ----
# also using moon illumination & cloud cover as a predictor
cn <- colnames(gan)
frm <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
fit_model3 <- lm(frm, gan)
prd3 <- predict(fit_model3, newdata = grid_pred, se.fit = TRUE)
pred_model3 <- grid_pred %>% mutate(est = prd3$fit, var = prd3$se.fit^2)

(plt_mod3 <- 
  ggplot() +
  geom_sf(data = pred_model3, aes(fill = est), colour = "transparent") +
  scale_fill_viridis_c(na.value = "#440154FF", direction = -1, limits = c(1, 7), guide = "none") +
  theme_minimal() +
  labs(title = "Land-use regression predictions", subtitle = "Model 3"))

## Model 4: Universal kriging with LUR ----
emp_vario_lur <- variogram(frm, gan %>% filter(!is.na(sky_brightness)))
mod_vario_lur <- fit.variogram(emp_vario_lur, vgm(psill = 1, "Sph", nugget = 1), fit.method = 1)
krige_pred_lur <- krige(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.001), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_lur,
  debug.level = -1
)

krige_pred_lur_cv <- krige.cv(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.00001), 
  model     = mod_vario_lur,
  debug.level = -1
)

pred_model4 <- 
  grid_pred %>% 
  mutate(est = krige_pred_lur$var1.pred, 
         var = krige_pred_lur$var1.var)

(plt_mod4 <- 
    ggplot() +
    geom_sf(data = pred_model4, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, guide = "none", limits = c(1, 7)) +
    theme_minimal() +
    labs(title = "Land-use kriging predictions", subtitle = "Model 4"))

## Model 5: Using OSM covariates only ----
# Motorway variable
frm <- sky_brightness ~ moon_illumination + CloudCover + motorway_10km
fit_model5 <- lm(frm, gan)
prd5 <- predict(fit_model5, newdata = grid_pred, se.fit = TRUE)
pred_model5 <- grid_pred %>% mutate(est = prd5$fit, var = prd5$se.fit^2)

(plt_mod5 <- 
    ggplot() +
    geom_sf(data = pred_model5, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, limits = c(1, 7), guide = "none") +
    theme_minimal() +
    labs(title = "OSM regression predictions", subtitle = "Model 5"))

## Model 6: Universal kriging with OSM covariates ----
# Motorway variable
frm <- sky_brightness ~ moon_illumination + CloudCover + motorway_10km
emp_vario_osm <- variogram(frm, gan %>% filter(!is.na(sky_brightness)))
mod_vario_osm <- fit.variogram(emp_vario_osm, vgm(psill = 1, "Sph", nugget = 1), fit.method = 1)
krige_pred_osm <- krige(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.001), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_osm,
  debug.level = -1
)

krige_pred_osm_cv <- krige.cv(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.00001), 
  model     = mod_vario_osm,
  debug.level = -1
)

pred_model6 <- 
  grid_pred %>% 
  mutate(est = krige_pred_osm$var1.pred, 
         var = krige_pred_osm$var1.var)

(plt_mod6 <- 
    ggplot() +
    geom_sf(data = pred_model6, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, guide = "none", limits = c(1, 7)) +
    theme_minimal() +
    labs(title = "OSM kriging predictions", subtitle = "Model 6"))


## Model 7: Using both LUR and OSM ----
# Motorway variable + landuse
cn <- colnames(gan)
frm <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover + motorway_10km +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
fit_model7 <- lm(frm, gan)
prd7 <- predict(fit_model7, newdata = grid_pred, se.fit = TRUE)
pred_model7 <- grid_pred %>% mutate(est = prd7$fit, var = prd7$se.fit^2)

(plt_mod7 <- 
    ggplot() +
    geom_sf(data = pred_model7, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, limits = c(1, 7), guide = "none") +
    theme_minimal() +
    labs(title = "Full regression predictions", subtitle = "Model 7"))

## Model 8: Universal kriging with both LUR and OSM ----
# Motorway variable + landuse
cn <- colnames(gan)
frm <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination + CloudCover + motorway_10km +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+")
))
emp_vario_all <- variogram(frm, gan %>% filter(!is.na(sky_brightness)))
mod_vario_all <- fit.variogram(emp_vario_osm, vgm(psill = .9, "Sph", nugget = 1), fit.method = 1)
krige_pred_all <- krige(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.001), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_all,
  debug.level = -1
)

krige_pred_all_cv <- krige.cv(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.00001), 
  model     = mod_vario_all,
  debug.level = -1
)

pred_model8 <- 
  grid_pred %>% 
  mutate(est = krige_pred_all$var1.pred, 
         var = krige_pred_all$var1.var)

(plt_mod8 <- 
    ggplot() +
    geom_sf(data = pred_model8, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, guide = "none", limits = c(1, 7)) +
    theme_minimal() +
    labs(title = "Full kriging predictions", subtitle = "Model 8"))


# Model validation ----
# plot all models
ggsave(
  file = "img/model_predictions.png", 
  plot = (plt_mod1 + plt_mod2) / (plt_mod3 + plt_mod4) / (plt_mod5 + plt_mod6) / (plt_mod7 + plt_mod8),
  width = 10, height = 16
)

# create single sf with all model predictions (negative because inverted scale)
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

# Internal validation: model comparison (TODO)

# Get the residuals 
tibble(Model = 1:7, 
       SSErr = c(mean(krige_pred_gan$residual^2), 
                 loocv(fit_model3),
                 mean(krige_pred_lur_cv$residual^2, na.rm = T),
                 loocv(fit_model5), 
                 mean(krige_pred_osm_cv$residual^2),
                 loocv(fit_model7),
                 mean(krige_pred_all_cv$residual^2, na.rm = T)))
# See how many NA's to determine severity
krige_pred_lur_cv_resid <- as.data.frame(krige_pred_lur_cv$residual)
sum(is.na(krige_pred_lur_cv_resid)) # 2 NA's
krige_pred_all_cv_resid <- as.data.frame(krige_pred_all_cv$residual)
sum(is.na(krige_pred_all_cv_resid)) # 3 NA's

# External validation: compare the predictions to the (log-)skyglow
# visual comparison
pred_sf %>% 
  as_tibble() %>%
  pivot_longer(
    starts_with("est"), 
    names_to = c("Covariates", "Kriging"), 
    names_sep = "_", 
    names_prefix = "est_", 
    values_to = "est"
  ) %>% 
  mutate(Kriging = as_factor(Kriging), Covariates = as_factor(Covariates)) %>% 
  ggplot(aes(y = log_skyglow, x = est)) +
  geom_pointdensity() + 
  geom_smooth(colour = "black", se = FALSE, formula = y ~ x, method = "lm") +
  scale_colour_viridis_c(guide = "none") +
  theme_minimal() +
  facet_grid(rows = vars(Covariates), cols = vars(Kriging), labeller = label_both) +
  xlim(c(-9, 2)) +
  ylim(-1.5, 4.5) +
  labs(x = "Estimate", y = "Log observed skyglow")

ggsave("img/comparison.png", width = 8, height = 6)

# comparing correlation
as_tibble(pred_sf) %>% 
  select(log_skyglow, starts_with("est")) %>% 
  cor(use = "pair", method = "spearman")

cor.test(pred_sf$skyglow, pred_sf$est_Naive_No, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Naive_Yes, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse_No, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse_Yes, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_OSM_No, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_OSM_Yes, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Both_No, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Both_Yes, method = "spearman")

# comparing R²
tibble(
  Model = 1:8, 
  Rsq = c(summary(lm(log_skyglow ~ est_Naive_No, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Naive_Yes, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse_No, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse_Yes, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_OSM_No, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_OSM_Yes, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Both_No, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Both_Yes, na.omit(pred_sf)))$r.squared)
)

# It takes an hour to run this, so save workspace for now
save.image("workspace_model.RData")
