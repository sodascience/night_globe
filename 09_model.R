# Modeling script
# Estimate model on GaN data
# Predict for the grid
# Compare to remote sensed skyglow

library(tidyverse)
library(sf)
library(gstat)
library(ggpointdensity)
library(patchwork)

# load data
gan       <- read_rds("data/gan_enriched.rds")
grid_pred <- read_rds("data/grid_enriched.rds") #landuse
gan_penn <-  read_rds("data/gan_penn_processed.rds") #OSM
skyglow   <- read_rds("data/grid_skyglow.rds")

# merge the two enriched datasets. Merge(landuse,OSM)
gan_compl <- cbind(gan,gan_penn[,c(8:25)])
grid_compl <-cbind(grid_pred,gan_grid[,c(5:17)])

# Model 1: Naïve model ----
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

# Model 2: kriging ----
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

# Model 3: land-use regression ----
# also using moon illumination as a predictor
cn <- colnames(gan)
frm <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination +",
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

# Model 4: Universal kriging with LUR ----
emp_vario_lur <- variogram(frm, gan %>% filter(!is.na(sky_brightness)))
mod_vario_lur <- fit.variogram(emp_vario_lur, vgm(psill = 1, "Sph", nugget = 1), fit.method = 1)
krige_pred_lur <- krige(
  formula   = frm,
  locations = gan %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.001), 
  newdata   = grid_pred %>% mutate(geometry = st_centroid(geometry)), 
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
    labs(title = "Universal kriging predictions", subtitle = "Model 4"))

(plt_mod1 + plt_mod2) / (plt_mod3 + plt_mod4)
ggsave("img/model_predictions_landuse.png", width = 10, height = 8)



############### just the OSM  covariates ####
# A model with motorway covariates
gan_penn2 <- gan_penn %>% filter(!is.na(sky_brightness))
emp_vario_cov <- variogram(sky_brightness~motorway_1km+motorway_25km+motorway_10km, gan_penn2)
plot(emp_vario_cov)
# Estimate a spherical model for the variogram
mod_vario_cov <- fit.variogram(
  emp_vario_cov, 
  vgm(psill = 1.5, "Sph", nugget = 1), 
  #  fit.method = 2
)

# Now krige with the motorways
grid_pred_cov <- krige(
  formula   = sky_brightness ~cloud_cover+motorway_10km,
  locations = gan_penn2 %>% st_jitter(0.00001), 
  newdata   = gan_grid %>% mutate(geometry = st_centroid(geometry), cloud_cover = "clear"),
  model     = mod_vario_cov,
  nmax      = 100,
  debug.level = -1
) # warnings about singular values

pred_model5 <- 
  grid_compl %>% 
  mutate(est = grid_pred_cov$var1.pred, 
         var = grid_pred_cov$var1.var)

# and make a plot
gan_grid %>% 
  mutate(pred = grid_pred_cov$var1.pred, var = grid_pred_cov$var1.var) %>% 
  ggplot() + 
  geom_sf(color = NA, mapping = aes(fill = pred)) + 
  geom_sf(data = gan_penn, mapping = aes(colour = sky_brightness)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 7)) +
  scale_colour_viridis_c(direction = -1)
ggsave("img/OSM clouds + motorways_10km.png")



############### a model with both landuse and OSM ####
# Model 6: land-use and OSM regression without kriging ----
# also using moon illumination as a predictor
cn <- colnames(gan_compl)
frm <- as.formula(paste(
  "sky_brightness ~ 0 + moon_illumination +",
  paste(cn[str_detect(cn, "landtype")], collapse = "+"),
  paste("+ motorway_10km")
))

fit_model6 <- lm(frm, gan_compl)
prd6 <- predict(fit_model6, newdata = grid_compl, se.fit = TRUE)
pred_model6 <- grid_pred %>% mutate(est = prd6$fit, var = prd6$se.fit^2)

(plt_mod6 <- 
    ggplot() +
    geom_sf(data = pred_model6, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, limits = c(1, 7), guide = "none") +
    theme_minimal() +
    labs(title = "Land-use and OSM regression", subtitle = "Model 6"))


# Model 7: Universal kriging with LUR and OSM ----
emp_vario_lur <- variogram(frm, gan_compl %>% filter(!is.na(sky_brightness)))
mod_vario_lur <- fit.variogram(emp_vario_lur, vgm(psill = 1, "Sph", nugget = 1), fit.method = 1)
krige_pred_lur <- krige(
  formula   = frm,
  locations = gan_compl %>% filter(!is.na(sky_brightness)) %>% st_jitter(0.001), 
  newdata   = grid_compl %>% mutate(geometry = st_centroid(geometry)), 
  model     = mod_vario_lur,
  debug.level = -1
)
pred_model7 <- 
  grid_compl %>% 
  mutate(est = krige_pred_lur$var1.pred, 
         var = krige_pred_lur$var1.var)

(plt_mod7 <- 
    ggplot() +
    geom_sf(data = pred_model4, aes(fill = est), colour = "transparent") +
    scale_fill_viridis_c(na.value = "#440154FF", direction = -1, guide = "none", limits = c(1, 7)) +
    theme_minimal() +
    labs(title = "LUR+OSM Universal kriging predictions", subtitle = "Model 7"))


# Model validation ----
# create single sf with all model predictions (negative because inverted scale)
pred_sf <- 
  skyglow %>% 
  mutate(
    est_Naive              = -pred_model1$est,
    est_Krige              = -pred_model2$est,
    est_Landuse            = -pred_model3$est,
    est_Landuse_Krige      = -pred_model4$est,
    est_OSM_Krige          = -pred_model5$est,
    est_Landuse_OSM        = -pred_model6$est,
    est_Landuse_OSM_Krige  = -pred_model7$est,
    log_skyglow            = log(skyglow),
    log_radiance           = log(radiance)
  )

write_rds(pred_sf, "data/pred_sf.rds")

# Internal validation: model comparison (TODO)


# External validation: compare the predictions to the (log-)skyglow
# visual comparison
pred_sf %>% 
  pivot_longer(starts_with("est"), names_to = "model", names_prefix = "est_", values_to = "est") %>% 
  mutate(model = as_factor(model)) %>% 
  ggplot(aes(y = log_skyglow, x = est)) +
  geom_pointdensity() + 
  geom_smooth(colour = "black", se = FALSE, formula = y ~ x, method = "lm") +
  scale_colour_viridis_c(guide = "none") +
  theme_minimal() +
  facet_wrap(~model) +
  xlim(c(-9, 2)) +
  ylim(-1.5, 4.5) +
  labs(x = "Estimate", y = "Log observed skyglow")

ggsave("img/comparison.png", width = 8, height = 6)

# comparing correlation
as_tibble(pred_sf) %>% 
  select(log_skyglow, starts_with("est")) %>% 
  cor(use = "pair", method = "spearman")

cor.test(pred_sf$skyglow, pred_sf$est_Naive, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Krige, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse_Krige, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_OSM_Krige, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse_OSM, method = "spearman")
cor.test(pred_sf$skyglow, pred_sf$est_Landuse_OSM_Krige, method = "spearman")

# comparing R²
tibble(
  Model = c("Naive", "Krige", "Landuse", "Landuse_Krige","OSM","Landuse_OSM","Landuse_OSM_Krige"), 
  Rsq = c(summary(lm(log_skyglow ~ est_Naive, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Krige, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse_Krige, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_OSM_Krige, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse_OSM, na.omit(pred_sf)))$r.squared,
          summary(lm(log_skyglow ~ est_Landuse_OSM_Krige, na.omit(pred_sf)))$r.squared)
)
