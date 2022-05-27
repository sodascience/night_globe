# External validation script
# Take the predictions for the grid
# Compare with the external skyglow metric
library(tidyverse)
library(sf)

# load the prediction grid
pred_sf <- read_rds("data/pred_sf.rds")

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
  labs(x = "Globe at Night prediction (negative)", y = "Log observed skyglow")

ggsave("img/external_validation.png", width = 6, height = 7)


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
