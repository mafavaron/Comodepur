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

series.plot <- function(d) {
  png("final_plots/series_overlapped.png", width=800, height=600)
  plot(d$Time.Stamp, d$Odor.from.Sonic.Met, type="l", col="blue", xlab="", ylab="Odor intensity (OU)")
  lines(d$Time.Stamp, d$Odor.from.Plant.Met, col="red")
  dev.off()
}

series.comparison <- function(d) {
  png("final_plots/series_comparisons.png", width=600, height=600)
  mx <- max(c(d$Odor.from.Sonic.Met, d$Odor.from.Plant.Met))
  plot(d$Odor.from.Plant.Met, d$Odor.from.Sonic.Met, xlim=c(0,mx), ylim=c(0,mx), cex=0.2)
  abline(0,1)
  dev.off()
}
