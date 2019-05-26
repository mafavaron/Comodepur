library("mixdist")
library("circular")
require("ggplot2")
require("RColorBrewer")

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


read.meteo.sonic <- function() {
  
  # Get data, as they are
  s <- read.csv("soniclib/Comodepur_05min.csv", stringsAsFactors = FALSE)
  s$date <- as.POSIXct(s$date, tz="UTC")
  return(s)
  
}


read.meteo <- function() {
  c <- read.meteo.conv()
  s <- read.meteo.sonic()
  d <- merge(c,s,by.x="Date",by.y="date",all.y=TRUE,all.x=FALSE)
  return(d)
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
  
  # Plot wind speed vs time
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
  
  # Direction vs time plot
  png(file="wind_plots/evolution_dir.png", width=800, height=600)
  plot(c$Date, c$Dir, type="l", xlab="", ylab="Wind direction (° from North)", lwd=2, xaxt="n", ylim=c(0,360))
  dt <- as.Date(c$Date)
  dt.min <- min(dt)
  dt.max <- max(dt) + 1
  dates  <- as.POSIXct(seq(from=dt.min, to=dt.max, by=1))
  axis.POSIXct(1, at=dates, labels=TRUE)
  dev.off()
  
}


plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          #panel.border = element_rect(colour = "blank"),
          panel.grid.major = element_line(colour="grey65"))
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}


comparisons <- function(d) {
  
  # Compare wind roses
  png("wind_plots/rose_sonic.png", width=600, height=600)
  plot.windrose(spd=d$ws, dir=d$wd, spdseq=c(0, 0.5, 1, 2, 4.5, 7), dirres=11.5, countmax=1000)
  dev.off()
  png("wind_plots/rose_conv.png", width=600, height=600)
  plot.windrose(spd=d$Vel, dir=d$Dir, spdseq=c(0, 0.5, 1, 2, 4.5, 7), dirres=11.5, countmax=1000)
  dev.off()
  
}

