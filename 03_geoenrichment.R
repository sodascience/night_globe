# Enriching the data with some geo features
# Store sf in output folder
library(tidyverse)
library(sf)
library(osmenrich)

# If you are a FSS UU user --> use our own OSM instance and uncomment the second line:
## https://gist.github.com/jgarciab/96d4c654f417201cd363e09ec8f0254b
## osmdata::set_overpass_url("http://localhost:12345/api/interpreter")

# If you are not:
## Low number of queries: nothing is needed
## High number of queries: create your own overpass instance in your computer using docker (run 
## line 41 in your local machine, then uncomment the line above setting the overpass)


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
          control=list(timeout = 600), #Adding some extra time for the server to respond
          exact_distance = FALSE #35sec vs 0.03sec
        )
    }
  }
}

# Write output ----
# Store as gan_penn_geoenrich.rds
write_rds(gan_enrich, "output/gan_penn_geoenrich.rds")

