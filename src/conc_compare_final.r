get.series <- function() {
  d <- read.csv("final_compare/Compare_Final.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  d[is.na(d)] <- NA
  return(d)
}

FB.plot <- function(d) {
  png(file="final_plots/FB_series.png", height=600, width=800)
  plot(d$Time.Stamp, d$FB, type="l",xaxt="n", xlab="", ylab="FB")
  t.min <- as.POSIXct("2018-06-13 00:00:00", tz="UTC")
  t.max <- as.POSIXct("2018-07-18 00:00:00", tz="UTC")
  axis.POSIXct(1, at=seq(from=t.min, to=t.max, by=86400))
  abline( 0.67, 0, col="gray", lwd=2)
  abline(-0.67, 0, col="gray", lwd=2)
  dev.off()
}

NMSE.plot <- function(d) {
  png(file="final_plots/NMSE_series.png", height=600, width=800)
  plot(d$Time.Stamp, d$NMSE, type="l",xaxt="n", xlab="", ylab="NMSE", ylim=c(0,60))
  t.min <- as.POSIXct("2018-06-13 00:00:00", tz="UTC")
  t.max <- as.POSIXct("2018-07-18 00:00:00", tz="UTC")
  axis.POSIXct(1, at=seq(from=t.min, to=t.max, by=86400))
  abline( 6, 0, col="gray", lwd=2)
  dev.off()
}

FAC2.plot <- function(d) {
  png(file="final_plots/FAC2_series.png", height=600, width=800)
  plot(d$Time.Stamp, d$FAC2, type="l",xaxt="n", xlab="", ylab="FAC2")
  t.min <- as.POSIXct("2018-06-13 00:00:00", tz="UTC")
  t.max <- as.POSIXct("2018-07-18 00:00:00", tz="UTC")
  axis.POSIXct(1, at=seq(from=t.min, to=t.max, by=86400))
  abline( 0.3, 0, col="gray", lwd=2)
  dev.off()
}

NAD.plot <- function(d) {
  png(file="final_plots/NAD_series.png", height=600, width=800)
  plot(d$Time.Stamp, d$NAD, type="l",xaxt="n", xlab="", ylab="NAD")
  t.min <- as.POSIXct("2018-06-13 00:00:00", tz="UTC")
  t.max <- as.POSIXct("2018-07-18 00:00:00", tz="UTC")
  axis.POSIXct(1, at=seq(from=t.min, to=t.max, by=86400))
  abline( 0.5, 0, col="gray", lwd=2)
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

spatial.check.mean <- function() {
  d <- read.csv("final_plant_mean.csv")
  e <- read.csv("final_sonic_mean.csv")
  out <- compute.indicators(d, e)
  return(out)
}

spatial.check.p98 <- function() {
  d <- read.csv("final_plant_p98.csv")
  e <- read.csv("final_sonic_p98.csv")
  out <- compute.indicators(d, e)
  return(out)
}


compute.indicators <- function(d, e) {
  ref <- d[,3]
  tst <- e[,3]
  n <- length(tst)
  MSE <- sum(((ref-tst)/n)^2)
  ref.mean <- mean(ref)
  tst.mean <- mean(tst)
  NMSE <- MSE / (tst.mean*ref.mean)
  FB   <- 2*(sum(ref) - sum(tst)) / (sum(ref) + sum(tst))
  ref.log.mean <- mean(log(ref[ref > 0]))
  tst.log.mean <- mean(log(tst[tst > 0]))
  GM <- exp(ref.log.mean - tst.log.mean)
  out <- list(
    MSE  = MSE,
    NMSE = NMSE,
    FB   = FB,
    GM   = GM
  )
  return(out)
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
