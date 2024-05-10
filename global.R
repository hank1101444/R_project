library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(geosphere)
thief = read.csv("thief.csv", header = T, sep=',', na.strings = c("", "NA"))
stations = read.csv("police.csv", header = T, sep=',', na.strings = c("", "NA"))
thief = filter(thief, thief$lat > 24.8 & thief$year > 104)
# fix shit
names(stations)[5] = "lon"
names(stations)[6] = "lat"
stations = filter(stations, row_number() != 104)

# distance calculate
calc_distances <- function(thief, stations) {
  distances <- sapply(1:nrow(thief), function(i) {
    min(distHaversine(matrix(c(thief$lon[i], thief$lat[i]), ncol = 2),
                      matrix(c(stations$lon, stations$lat), ncol = 2)))
  })
  return(distances)
}
thief$distance_to_nearest_station = calc_distances(thief, stations)
