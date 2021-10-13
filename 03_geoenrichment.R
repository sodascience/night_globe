# Enriching the data with some geo features
# Store sf in output folder
library(tidyverse)
library(sf)
library(osmenrich)

## If you have a local instance of the overpass api installed you can uncomment the following line (using port 12345, see below in the installation)
# osmdata::set_overpass_url("http://localhost:12345/api/interpreter")

## To install the local instance, open docker and run the following
## code, which will install the database in Volumes/apfs_part/docker_us_storage
## It will take ~1 hour to install, but will remove the API limitations for the OSM queries
# docker run \
# -e OVERPASS_META=yes \
# -e OVERPASS_MODE=init \
# -e OVERPASS_PLANET_URL=https://download.geofabrik.de/north-america/us/pennsylvania-latest.osm.bz2 \
# -e OVERPASS_DIFF_URL=http://download.openstreetmap.fr/replication/north-america/minute/ \
# -e OVERPASS_RULES_LOAD=10 \
# -v /Volumes/apfs_part/docker_us_storage:/db \
# -p 12345:80 \
# -i -t \
# --name overpass_us \
# wiktorn/overpass-api

# Load data
gan_penn <- read_rds("output/gan_penn_sunmoon.rds")

## Parameters
# Setting up possible distances
rs <- c(1000, 10000, 25000)
# Kernel and type of objects
kernel <- "gaussian"
type <- "lines"

# Mapping of keys to values
key_to_values = list(highway = c("motorway","trunk","primary","secondary","tertiary","unclassified"),
                    building = c(NULL))

gan_enrich <- gan_penn

# Keep track of time (for future performance boosting)
start.time <- Sys.time()

## Enrich for the different combinations (~30 min with a local instance)
for (key in names(key_to_values)) {
  for (value in key_to_values[[key]]) {
    for (r in rs) {
      # Name of column
      if (is.null(value)) { 
        name = paste(key,"_",r/1000,"km",sep="")
      } else {
        name = paste(value,"_",r/1000,"km",sep="")
      }
    
      # Enrich
      gan_enrich <- gan_enrich %>%
        enrich_osm(
          name = name,
          key = key,
          value = value,
          type = type,
          kernel = kernel,
          r = r,
          control=list(timeout = 600) #Adding some extra time for the server to respond
        )
    }
  }
}

# Write output ----
# Store as gan_penn_geoenrich.rds
write_rds(gan_enrich, "output/gan_penn_geoenrich.rds")

# Print time taken
print(Sys.time() - start.time)
