get.plant.series <- function() {
  d <- read.csv("final_impianto_series.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}

get.sonic.series <- function() {
  d <- read.csv("final_sonico_series.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}

get.series <- function() {
  d <- get.sonic.series()
  e <- get.plant.series()
  f <- data.frame(Time.Stamp=d$Time.Stamp, Odor.from.Sonic.Met=d$NO2, Odor.from.Plant.Met=e$NO2)
  return(f)
}
