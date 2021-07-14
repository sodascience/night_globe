# Model creation script

library(tidyverse)
library(sf)

# read data
source( "01_data_loading.R" )
source("02_sun_elevation.R")
source("03_geoenrichment.R")
source("04_postprocess.R" )
 
gan_sf <- read_rds("output/gan_penn_processed.rds")
