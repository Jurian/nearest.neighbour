

#' @title Combine multiple weather data sets with station information
#' @description This function makes several assumptions about the structure of supplied datasets. Weather data is assumed to be of the form <station, datetime, value>.
#' @param station.file Path to file with station information, e.g. "data/stations.csv"
#' @param weather.files A character vector with paths of weather information, e.g. c("data/precipitation.csv", "data/temperature.csv")
#' @param columns Which columns to take from the merged result, e.g. c("longitude", "latitude", "datetime", "precipitation",  "temperature")
#' @param tz Optional timezone setting, defaults to "UTC"
#' @param tf Optional timeformat setting, defaults to "\%Y\%m\%d
#' @return A datatable created by merging supplied data sets with each other and the station data. Datetime information is converted to unix timestamp
#' @author Jurian Baas
#' @importFrom data.table fread
#' @keywords  \url{http://www.sciamachy-validation.org/climatology/daily_data/selection.cgi}
combine.weather.datasets <- function(station.file, weather.files, columns, tz = "UTC", tf = "%Y%m%d") {

  stations <- load(station.file)
  stations <- data.table::data.table(stations)
  data.table::setkey(stations, "station")

  weather <- base::Reduce(function(a, b) merge(a, b, by = c("station", "datetime"), all = T), lapply(weather.files, function(wt) {
    dt <- data.table::fread(wt)
    data.table::setkey(dt, "station", "datetime")
    return(dt)
  }))

  weather$datetime <- as.numeric(as.POSIXlt(as.character(weather$datetime), format = tf, tz = tz))
  data.table::setkey(weather, "station", "datetime")

  weather <- base::merge(stations, weather, by = "station", all = F)
  weather <- weather[, columns, with = F]
  data.table::setkey(weather, "datetime", "latitude", "longitude")

  return(weather)
}
