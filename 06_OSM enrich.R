# Enriching the data with some geo features
# Store sf in output folder
library(tidyverse)
library(sf)
library(osmenrich)

# Load data
gan_penn <- read_rds("output/gan_penn_sunmoon.rds")

# Highways ----
# All highways receive a 1, 10 or 25 km gaussian kernel
## Motorway ----
# 1km
gan_enrich <- gan_penn %>%
  enrich_osm(
    name = "motorway_1km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "motorway_10km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "motorway_25km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

## Trunk ----
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_1km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_10km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_25km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

## Primary ----
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_1km",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_10km",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_25km",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

## Secondary ----
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_1km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_10km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_25km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

## Tertiary ----
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_1km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_10km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_25km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

## Unclassified ----
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_1km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_10km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )

# 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_25km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )


# Buildings ----
# Buildings receive a 1, 10 or 20 km gaussian kernel
# 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_1km",
    key = "building",
    kernel = "gaussian",
    r = 1000
  )

# 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_10km",
    key = "building",
    kernel = "gaussian",
    r = 10000
  )

# 20km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_20km",
    key = "building",
    kernel = "gaussian",
    r = 20000
  )

# Write output ----
# Store as gan_penn_geoenrich.rds
write_rds(gan_enrich, "output/gan_penn_geoenrich.rds")
