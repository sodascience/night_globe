# Enriching the data with some geo features
# Store sf in output folder
library(tidyverse)
library(sf)
library(remotes)
library(osmenrich)

# Load data
gan_state <- read_rds("output/gan_penn.rds") #naam nog veranderen
st_coordinates(gan_state)

# Make a copy to try things with
gan_copy <- gan_state

# Add highway = motorway for a Gaussian kernel of 1km
gan_enrich <- gan_copy %>%
  enrich_osm(
    name = "motorway_1km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "motorway_10km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "motorway_25km",
    key = "highway",
    value = "motorway",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add highway = trunk for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_1km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_10km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "trunk_25km",
    key = "highway",
    value = "trunk",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add highway = primary for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_1km",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_10km",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "primary_km25",
    key = "highway",
    value = "primary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add highway = secondary for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_1km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_10km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "secondary_25km",
    key = "highway",
    value = "secondary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add highway = tertiary for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_1km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_10km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "tertiary_25km",
    key = "highway",
    value = "tertiary",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add highway = unclassified for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_1km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_10km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 10000
  )
# And for 25km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "unclassified_25km",
    key = "highway",
    value = "unclassified",
    type = "lines",
    kernel = "gaussian",
    r = 25000
  )

# Add the number of buildings for a Gaussian kernel of 1km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_1km",
    key = "building",
    kernel = "gaussian",
    r = 1000
  )
# Now for 10km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_10km",
    key = "building",
    kernel = "gaussian",
    r = 10000
  )
# And for 20km
gan_enrich <- gan_enrich %>%
  enrich_osm(
    name = "buildings_20km",
    key = "building",
    kernel = "gaussian",
    r = 20000
  )

# Store as gan_penn_geoenrich.rds
write_rds(gan_enrich, "output/gan_penn_geoenrich.rds")
