# Model creation script

library(tidyverse)
library(sf)

# read data
gan_sf <- read_rds("output/gan_penn_processed.rds")