get.plant.series <- function() {
  d <- read.csv("final_impianto_series.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}

