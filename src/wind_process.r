library("mixdist")

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
  
  # Yield results
  out <- list(
    quantity  = "Vel (m/s)",
    quartiles = quartiles,
    mean      = m,
    stdev     = s,
    zero.p    = zero.percentage,
    w.shape   = parms$shape,
    w.scale   = parms$scale
  )
  return(out)
  
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
  
}
