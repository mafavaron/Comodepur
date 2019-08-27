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
  
  t <- typical(d$Time.Stamp, d$FAC2)
  png(file="final_plots/FAC2_typical.png", height=600, width=800)
  #plot(t$Time.Stamp, t$Value, type="l", xlim=c(0,24), xlab="Hour", ylab="FAC2")
  plot(t$Time.Stamp, t$Value, type="l",xaxt="n", xlim=c(0,24), xlab="Hour", ylim=c(0,1), ylab="FAC2")
  axis(1, at=seq(from=0, to=24, by=3))
  polygon(x=c(t$Time.Stamp,rev(t$Time.Stamp)), y=c(t$P.25,rev(t$P.75)),col="light grey");
  polygon(x=c(t$Time.Stamp,rev(t$Time.Stamp)), y=c(t$P.45,rev(t$P.55)),col="grey");
  abline( 0.3, 0, col="gray", lwd=2)
  lines(t$Time.Stamp, t$Value, lty=4)
  lines(t$Time.Stamp, t$Median, lwd=3)
  dev.off()
  
  write.csv(t, file="final_plots/FAC2_typical.csv", row.names=FALSE)
  
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
  v.p25 <- aggregate(value, by=list(tm), FUN=quantile, prob=0.25, na.rm=TRUE)
  v.p45 <- aggregate(value, by=list(tm), FUN=quantile, prob=0.45, na.rm=TRUE)
  v.mdn <- aggregate(value, by=list(tm), FUN=median, na.rm=TRUE)
  v.p55 <- aggregate(value, by=list(tm), FUN=quantile, prob=0.55, na.rm=TRUE)
  v.p75 <- aggregate(value, by=list(tm), FUN=quantile, prob=0.75, na.rm=TRUE)
  v.val <- v$x
  v.p25.val <- v.p25$x
  v.p45.val <- v.p45$x
  v.mdn.val <- v.mdn$x
  v.p55.val <- v.p55$x
  v.p75.val <- v.p75$x
  v.tim <- v$Group.1
  tm.tot <- seq(from=0, to=86400-1, by=delta.t)
  m      <- merge(data.frame(tim=v.tim, val=v.val, val.p25=v.p25.val, val.p45=v.p45.val, val.median=v.mdn.val, val.p55=v.p55.val, val.p75=v.p75.val), data.frame(tm.tot=tm.tot), by.x="tim", by.y="tm.tot", all=TRUE)
  names(m) <- c("Time.Stamp", "Value", "P.25", "P.45", "Median", "P.55", "P.75")
  m$Time.Stamp <- m$Time.Stamp / 3600
  m <- rbind(m, data.frame(Time.Stamp=24, Value=m$Value[1], P.25=m$P.25[1], P.45=m$P.45[1], Median=m$Median[1], P.55=m$P.55[1], P.75=m$P.75[1]))
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
  bothNonZero <- (ref > 0) & (tst > 0)
  bothNonZeroIdx <- which(bothNonZero)
  GV <- exp(sum((log(ref[bothNonZeroIdx]) - log(tst[bothNonZeroIdx]))^2) / sum(bothNonZero))
  numWithin <- sum(ref*0.5 <= tst & tst <= 2.*ref)
  numTotal  <- n
  FAC2 <- numWithin / numTotal
  NAD  <- mean(abs(ref - tst)) / (ref.mean + tst.mean)
  out <- list(
    MSE  = MSE,
    NMSE = NMSE,
    FB   = FB,
    GM   = GM,
    GV   = GV,
    FAC2 = FAC2,
    NAD  = NAD
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


conc.contour <- function(file.name, main, plot.file) {
  png(plot.file, width=600, height=600)
  c <- read.csv(file.name)
  x <- unique(sort(c$X))
  y <- unique(sort(c$Y))
  z <- matrix(c$Conc,length(x),length(y),byrow=TRUE)
  contour(x,y,z,main=main, levels=c(0.0001, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1))
  dev.off()
}


plot.contours <- function() {
  conc.contour("./final_compare/final_sonic_mean.csv", "Meteo from ultrasonic anemometer, period mean", "./final_plots/final_sonic_mean.png")
  conc.contour("./final_compare/final_plant_mean.csv", "Meteo from plant station, period mean", "./final_plots/final_plant_mean.png")
  conc.contour("./final_compare/final_sonic_p98.csv", "Meteo from ultrasonic anemometer, period 98th percentile", "./final_plots/final_sonic_p98.png")
  conc.contour("./final_compare/final_plant_p98.csv", "Meteo from plant station, period 98th percentile", "./final_plots/final_plant_p98.png")
}


get.meteo.plant <- function() {
  d <- read.csv("Comodepur_Plant.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}


get.meteo.sonic <- function() {
  d <- read.csv("Comodepur_Sonic.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}


process <- function() {
  d <- get.series()
  typical(d$Time.Stamp, d$Odor.from.Sonic.Met)

}
