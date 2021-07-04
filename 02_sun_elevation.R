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
rm(sundata)

qplot(gan_state$moonfraction)
qplot(gan_state$moonfraction,gan_state$moonaltitude) #ok, if altitude >0 use fraction as covariate
gan_state$moonillumination[gan_state$moonaltitude>0.0000001] <-gan_state$moonfraction
gan_state$moonillumination[gan_state$moonaltitude<0] <-0
qplot(gan_state$sunaltitude) # >0 means it is up. we may want to delete >-.10

qplot(gan_state$sunazimuth)
qplot(gan_state$sunaltitude,gan_state$sunazimuth)
qplot(gan_state$sunaltitude,gan_state$LimitingMag)
# https://www.researchgate.net/publication/308906640_Flower_clocks_time_memory_and_time_forgetting/figures?lo=1&utm_source=google&utm_medium=organic



################## OLD code #################################
st_sun_elevation <- function(ldate, ltime, point) {
  result <- .sunPosition(
    year  = year(ldate),
    month = month(ldate), 
    day   = day(ldate),
    hour  = hour(ltime),
    min   = minute(ltime), 
    sec   = second(ltime),
    lat   = st_coordinates(point)[2],
    long  = st_coordinates(point)[1]
  )
  
  result$elevation
}


# code from https://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-day-latitude-and-longitude
.sunPosition <- function(year, month, day, hour=12, min=0, sec=0,
                        lat=46.5, long=6.5) {
  twopi <- 2 * pi
  deg2rad <- pi / 180
  
  # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
  month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
  day <- day + cumsum(month.days)[month]
  leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) & 
    day >= 60 & !(month==2 & day==60)
  day[leapdays] <- day[leapdays] + 1
  
  # Get Julian date - 2400000
  hour <- hour + min / 60 + sec / 3600 # hour plus fraction
  delta <- year - 1949
  leap <- trunc(delta / 4) # former leapyears
  jd <- 32916.5 + delta * 365 + leap + day + hour / 24
  
  # The input to the Atronomer's almanach is the difference between
  # the Julian date and JD 2451545.0 (noon, 1 January 2000)
  time <- jd - 51545.
  
  # Ecliptic coordinates
  
  # Mean longitude
  mnlong <- 280.460 + .9856474 * time
  mnlong <- mnlong %% 360
  mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360
  
  # Mean anomaly
  mnanom <- 357.528 + .9856003 * time
  mnanom <- mnanom %% 360
  mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
  mnanom <- mnanom * deg2rad
  
  # Ecliptic longitude and obliquity of ecliptic
  eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
  eclong <- eclong %% 360
  eclong[eclong < 0] <- eclong[eclong < 0] + 360
  oblqec <- 23.439 - 0.0000004 * time
  eclong <- eclong * deg2rad
  oblqec <- oblqec * deg2rad
  
  # Celestial coordinates
  # Right ascension and declination
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
  dec <- asin(sin(oblqec) * sin(eclong))
  
  # Local coordinates
  # Greenwich mean sidereal time
  gmst <- 6.697375 + .0657098242 * time + hour
  gmst <- gmst %% 24
  gmst[gmst < 0] <- gmst[gmst < 0] + 24.
  
  # Local mean sidereal time
  lmst <- gmst + long / 15.
  lmst <- lmst %% 24.
  lmst[lmst < 0] <- lmst[lmst < 0] + 24.
  lmst <- lmst * 15. * deg2rad
  
  # Hour angle
  ha <- lmst - ra
  ha[ha < -pi] <- ha[ha < -pi] + twopi
  ha[ha > pi] <- ha[ha > pi] - twopi
  
  # Latitude to radians
  lat <- lat * deg2rad
  
  # Azimuth and elevation
  el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  
  # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
  az[!cosAzPos] <- pi - az[!cosAzPos]

  
  el <- el / deg2rad
  az <- az / deg2rad
  lat <- lat / deg2rad
  
  return(list(elevation=el, azimuth=az))
} 