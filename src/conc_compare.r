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

typical <- function(date, value, delta.t=3600) {
  tm <- as.integer(date) %% 86400
  v  <- aggregate(value, by=list(tm), FUN=mean, na.rm=TRUE)
  v.val <- v$x
  v.tim <- v$Group.1
  tm.tot <- seq(from=0, to=86400-1, by=delta.t)
  m      <- merge(data.frame(tim=v.tim, val=v.val), data.frame(tm.tot=tm.tot), by.x="tim", by.y="tm.tot", all=TRUE)
  names(m) <- c("Time.Stamp", "Value")
  m$Time.Stamp <- m$Time.Stamp / 3600
  return(m)
}

series.typical.day <- function() {
  d <- get.series()
  sonic.typical <- typical(d$Time.Stamp, d$Odor.from.Sonic.Met)
  plant.typical <- typical(d$Time.Stamp, d$Odor.from.Plant.Met)
  png("final_plots/series.typical.png", width=800, height=600)
  plot(plant.typical$Time.Stamp, plant.typical$Value, type="l", col="red", xlab="Hour", ylab="Odour intensity (OU)")
  lines(sonic.typical$Time.Stamp, sonic.typical$Value, col="blue")
  dev.off()
}

process <- function() {
  d <- get.series()
  typical(d$Time.Stamp, d$Odor.from.Sonic.Met)
}
