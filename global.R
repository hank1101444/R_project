library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(geosphere)
library(shinydashboard)
library(ggplot2)
library(DT)

thief = read.csv("thief.csv", header = T, sep=',', na.strings = c("", "NA"))
stations = read.csv("police.csv", header = T, sep=',', na.strings = c("", "NA"))
thief = filter(thief, thief$lat > 24.8 & thief$year > 104)
# fix shit
stations$name = trimws(stations$name) #fix " xx分局"
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
#debug thiefs have 1212 station NA
# na_rows <- thief[rowSums(is.na(thief)) > 0, ]
# print(na_rows)
# thief = na.omit(thief)
thief$distance_to_nearest_station = calc_distances(thief, stations)

# demo
# http://140.138.155.243:3838/s1101444/hw2/
