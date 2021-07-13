# Function to compute radiance for 
# transforming remotely sensed radiance to skyglow
# Walker's law: https://iopscience.iop.org/article/10.1086/130142
# intensity \propto distance^{-2.5}

# specifically: log flux \propto log flux - 2.5 * log (distance + 1)
# https://arxiv.org/ftp/astro-ph/papers/0702/0702721.pdf, figure 10
library(tidyverse)
library(sf)
library(stars)
library(progress)
library(patchwork)

radiance_raster <- read_stars("data/median_radiance_2020.tif", proxy = TRUE)
prediction_grid <- read_rds("data/grid_penn.rds")

# function to compute skyglow over a grid based on radiance raster data (stars obj)
# based on walker's (1977) law
skyglow_grid <- function(grid_sf, radiance_raster, downsample = 15, walker = -2.5, max_dist = 100000) {
  radiance_sf     <- st_as_sf(radiance_raster, downsample = downsample)
  log_rad         <- log(radiance_sf[[1]])
  ncell           <- nrow(grid_sf)
  skyglow         <- numeric(ncell)
  points_grid     <- grid_sf %>% st_geometry() %>% st_centroid()
  points_radiance <- radiance_sf %>% st_geometry() %>% st_centroid()
  
  pb <- progress_bar$new(total = ncell, format = ":spin [:bar] :percent :eta")
  for (i in 1:ncell) {
    d <- as.numeric(st_distance(points_grid[i], points_radiance))
    sel <- d < max_dist
    
    # radiance at source * distance in km ^ {-2.5}
    res <- log_rad[sel] + walker * log1p(d[sel] / 1000)
    
    # radiance is additive, so log skyglow is first exponentiated before summation
    skyglow[i] <- sum(exp(res))
    
    pb$tick()
  }
  
  grid_sf %>% mutate(skyglow = skyglow)
}

# compute skyglow grid
sg_grid <- skyglow_grid(prediction_grid, radiance_raster, downsample = 5, max_dist = 150e3)


# compute mean radiance for same grid
rd_grid <- 
  radiance_raster %>% 
  st_as_stars(downsample = 5) %>% 
  aggregate(st_geometry(prediction_grid), mean) %>% 
  st_as_sf() %>% 
  mutate(radiance = median_radiance_2020.tif) %>% 
  select(radiance, geometry)

# write to file
sg_rd_grid <- sg_grid %>% mutate(radiance = rd_grid$radiance)
write_rds(sg_rd_grid, "output/grid_skyglow.rds")


# bonus: plot to compare radiance and skyglow grid
plt_radiance <- 
  ggplot(rd_grid, aes(fill = radiance)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(trans = "log10", limits = c(0.2, 100)) +
  labs(fill = "", title = "Radiance", subtitle = "Mean radiance on 5x5 km grid") +
  theme_minimal()

plt_skyglow <- 
  ggplot(sg_grid, aes(fill = skyglow)) + 
  geom_sf(colour = NA) + 
  scale_fill_viridis_c(trans = "log10", limits = c(0.2, 100)) + 
  labs(fill = "", title = "Skyglow", subtitle = "Walker's law on 5x5 km grid") +
  theme_minimal()

plt_radiance / plt_skyglow

ggsave("img/skyglow.png", width = 6, height = 7)
