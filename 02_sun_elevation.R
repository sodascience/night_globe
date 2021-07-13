# sun and moon elevatuion
# to use to check for brightness of sun -> delete those obs
# and for moon brightness -> use as covariate
# 04 July 2021
# Peter Lugtig

# load the data
source("01_data_loading.R")

##################################
#libraries
library(suncalc)
library(stringr)

#example NORT RUN
#getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET") #example
#getSunlightPosition(date = NULL, lat = NULL, lon = NULL,data = NULL, keep = c("altitude", "azimuth"))
#getMoonPosition(date = NULL, lat = NULL, lon = NULL, data = NULL, keep = c("altitude", "azimuth", "distance", "parallacticAngle"))

# change the geom first to separate latitude and longitude
gan_state$geom2 <- substring(gan_state$geometry,3,nchar(gan_state$geometry)-1)
gan_state$latitude <- as.numeric(unlist(strsplit(gan_state$geom2,","))[[1]])
gan_state$longitude <- as.numeric(unlist(strsplit(gan_state$geom2,","))[[2]])
gan_state$geom2 <- NULL
gan_state$LimitingMag[gan_state$LimitingMag==-9999] <- NA
# merge the day and hour
gan_state$Date <- paste(gan_state$UTDate,gan_state$UTTime)

# now get the sun and moon illumination
sundata <- data.frame(date = as.Date(gan_state$Date), # need this to deal with multipke dates in package
                   lat = gan_state$latitude, 
                   lon = gan_state$longitude)
gan_state$sunazimuth <-getSunlightPosition(data = sundata)$azimuth
#  sun azimuth in radians (direction along the horizon, measured from south to west), e.g. 0 is south and Math.PI * 3/4 is northwest
gan_state$sunaltitude <-getSunlightPosition(data = sundata)$altitude
# sun altitude above the horizon in radians, e.g. 0 at the horizon and PI/2 at the zenith (straight over your head)
gan_state$moonfraction <- getMoonIllumination(date = gan_state$Date)$fraction
#  illuminated fraction of the moon; varies from 0.0 (new moon) to 1.0 (full moon)
gan_state$moonaltitude <- getMoonPosition(data = sundata)$altitude
# moon altitude above the horizon in radians

# some chekcs (not run)

#qplot(gan_state$moonfraction)
#qplot(gan_state$moonfraction,gan_state$moonaltitude) #ok, if altitude >0 use fraction as covariate
gan_state$moonillumination[gan_state$moonaltitude>0.0000001] <-gan_state$moonfraction
gan_state$moonillumination[gan_state$moonaltitude<0] <-0
#qplot(gan_state$sunaltitude) # >0 means it is up. we may want to delete >-.10

#qplot(gan_state$sunazimuth)
#qplot(gan_state$sunaltitude,gan_state$sunazimuth)
#qplot(gan_state$sunaltitude,gan_state$LimitingMag)
# https://www.researchgate.net/publication/308906640_Flower_clocks_time_memory_and_time_forgetting/figures?lo=1&utm_source=google&utm_medium=organic

write_rds(gan_state,"gan_state with sun illumination.rds")
