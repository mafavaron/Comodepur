library("mixdist")
library("circular")

read.meteo.conv <- function() {
  
  # Get data, as they are
  c <- read.csv2("Meteo_Depurati_Comodepur_Campagna.csv", stringsAsFactors = FALSE)
  
  # Build a numerical time stamp
  day   <- substring(c$Date, first=1, last=2)
  month <- substring(c$Date, first=4, last=5)
  year  <- substring(c$Date, first=7)
  c$Date <- as.POSIXct(paste(year, "-", month, "-", day, " ", c$Time, ":00", sep=""), tz="UTC")
  c$Time <- NULL
  
  # Adjust time stamp, which in original data is posticipated, so that it is
  # anticipated (this way, it can be used to aggregate based on hour, hassle-free)
  c$Date <- c$Date - 5 * 60
  
  # Change speed unit from km/h to m/s
  c$Vel <- c$Vel / 3.6
  
  # Encode wind direction to numeric zero-centered sectors
  new.dir <- numeric(length(c$Dir))
  angles <- seq(from=0.0, to=360.0, length.out = 17)
  angles[17] <- NA  # Will be used to report calms
  sectors <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "---")
  for(i in 1:length(sectors)) {
    to.be.set <- which(c$Dir == sectors[i])
    new.angle <- angles[i]
    new.dir[to.be.set] <- new.angle
  }
  c$Dir <- new.dir
  
  return(c)
}


count.zeros <- function(x) {
  return(sum(x <= 0.0))
}


summarize.vel <- function(c) {
  
  # Compute basic order stats
  quartiles <- fivenum(c$Vel)
  m         <- mean(c$Vel)
  s         <- sd(c$Vel)
  
  # Estimate Weibull distribution parameters assuming locality = 0
  parms <- weibullpar(mean(c$Vel), sd(c$Vel), 0)
  
  # Count speeds exactly equal to zero, and normalize them to number of speeds;
  # express as percent
  zero.percentage <- 100.0 * sum(c$Vel <= 0.0) / length(c$Vel)
  
  # Classify zero-speed data percentage per hours
  tm <- (as.integer(c$Date) %% 86400) %/% 3600
  num.data <- aggregate(rep(1, times=length(c$Vel)), by=list(tm), FUN=sum)$x
  tmes     <- aggregate(tm, by=list(tm), FUN=min)$x
  vel.zero <- 100.*aggregate(c$Vel, by=list(tm), FUN=count.zeros)$x / num.data
  zero.counts <- data.frame(
    hour = tmes,
    zero.fraction = vel.zero
  )
  
  # Yield results
  out <- list(
    quantity    = "Vel (m/s)",
    quartiles   = quartiles,
    mean        = m,
    stdev       = s,
    zero.p      = zero.percentage,
    w.shape     = parms$shape,
    w.scale     = parms$scale,
    zero.counts = zero.counts
  )
  return(out)
  
}


summarize.dir <- function(c) {
  
}


plot.vel.conv <- function(c) {
  
  # Compute Weibull shape and scale parameters given mean and standard deviation
  parms <- weibullpar(mean(c$Vel), sd(c$Vel), 0)

  # Plot histogram, and overlap it with the estimated Weibull density
  png(file="wind_plots/histogram_vel_plus_weibull.png", width=800, height=600)
  hist(c$Vel, freq=FALSE, xlab="Wind speed (m/s)", ylab="Probability density", col="light grey", main="")
  x <- seq(from=min(c$Vel), to=max(c$Vel), length.out=128)
  y <- dweibull(x, parms$shape[1], parms$scale[1])
  y[which(is.infinite(y))] <- NA
  lines(x, y, lwd=2)
  dev.off()
  
  # Plot histogram, and do not overlap it with the estimated Weibull density
  png(file="wind_plots/histogram_vel.png", width=800, height=600)
  hist(c$Vel, freq=FALSE, xlab="Wind speed (m/s)", ylab="Probability density", col="light grey", main="")
  dev.off()
  
  # Plot histogram, and do not overlap it with the estimated Weibull density
  png(file="wind_plots/evolution_vel.png", width=800, height=600)
  plot(c$Date, c$Vel, type="l", xlab="", ylab="Wind speed (m/s)", lwd=2, xaxt="n")
  dt <- as.Date(c$Date)
  dt.min <- min(dt)
  dt.max <- max(dt) + 1
  dates  <- as.POSIXct(seq(from=dt.min, to=dt.max, by=1))
  axis.POSIXct(1, at=dates, labels=TRUE)
  dev.off()
  
  # Plot typical day, with variation (interquartile) bands
  tm <- (as.integer(c$Date) %% 86400) %/% 1800
  tmes     <- aggregate(tm, by=list(tm), FUN=min)$x
  vel.mean <- aggregate(c$Vel, by=list(tm), FUN=mean)$x
  vel.sd   <- aggregate(c$Vel, by=list(tm), FUN=sd)$x
  tmes     <- c(tmes, 48)
  vel.mean <- c(vel.mean, vel.mean[1])
  vel.sd   <- c(vel.sd, vel.sd[1])
  vel.low  <- vel.mean - vel.sd
  vel.low[vel.low < 0] <- 0
  vel.hi   <- vel.mean + vel.sd
  png(file="wind_plots/typicalday_vel.png", width=800, height=600)
  plot(tmes/2, vel.hi, type="l", xaxt="n", xlab="Hour", ylab="Wind speed (m/s)", ylim=c(0,max(vel.hi)))
  axis(1, at=seq(from=0, to=24, by=3))
  lines(tmes/2, vel.mean, lwd=2)
  lines(tmes/2, vel.low)
  dev.off()
  
  # Hourly summary plots
  s <- summarize.vel(c)
  png(file="wind_plots/summary_zeroval_vel.png", width=800, height=600)
  barplot(s$zero.counts$zero.fraction, xlab="Hour", ylab="% Vel = 0", names.arg=seq(0,23,1))
  dev.off()
  
}


plot.dir.conv <- function(c) {
  
  # Circular wind histogram
  l <- 1.2
  x <- as.circular(c$Dir, type="angles", units="degrees", modulo="asis", zero=0, rotation="counter", template="geographics")
  png(file="wind_plots/histogram_dir.png", width=600, height=600)
  rose.diag(x, bins=360)
  dev.off()
  
  # Typical day of wind directions
  positive.dir <- which(c$Dir > 0)
  tm <- (as.integer(c$Date) %% 86400) %/% 1800
  tm.pd <- tm[positive.dir]
  tmes     <- aggregate(tm.pd, by=list(tm.pd), FUN=min)$x
  #dir.mean <- aggregate(x[positive.dir], by=list(tm.pd), FUN=mean.circular, na.rm=TRUE, control.circular=list(units="degrees", template="geographics"))$x
  dir.mean <- aggregate(x[positive.dir], by=list(tm.pd), FUN=mean.circular, na.rm=TRUE)$x
  dir.mean[dir.mean < 0] <- dir.mean[dir.mean < 0] + 360
  plot(tmes/2, dir.mean, cex=0.3, xaxt="n", xlab="Hour", ylab="Wind direction (° from N)", ylim=c(0,360))
  axis(1, at=seq(from=0, to=24, by=3))
  dev.off()
  
}
