# Compare meteorological data

get.sonic.data <- function() {
  d <- read.csv("Comodepur_Sonic.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}

get.conv.data <- function() {
  d <- read.csv("Comodepur_Plant.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp, tz="UTC")
  return(d)
}

compare <- function() {
  
  # Get data
  s <- get.sonic.data()
  c <- get.conv.data()
  
  # Wind speed
  vel.max <- max(c(c$Vel, s$Vel))
  png(file="compare/Vel.png", height=600, width=600)
  plot(c$Vel, s$Vel, cex=0.2, xlab="Conventional",ylab="Sonic", main="Vel (m/s)", xlim=c(0,vel.max), ylim=c(0,vel.max))
  abline(0, 1, lwd=2, col="light blue")
  dev.off()
  
  # Wind direction
  png(file="compare/Dir.png", height=600, width=600)
  plot(c$Dir, s$Dir, cex=0.2, xlab="Conventional",ylab="Sonic", main="Dir (° from North)", xlim=c(0,360), ylim=c(0,360))
  abline(0, 1, lwd=2, col="light blue")
  dev.off()
  
  # Friction velocity
  vel.max <- max(c(c$U.star, s$U.star))
  png(file="compare/Ustar.png", height=600, width=600)
  plot(c$U.star, s$U.star, cex=0.2, xlab="Conventional",ylab="Sonic", main="u* (m/s)", xlim=c(0,vel.max), ylim=c(0,vel.max))
  abline(0, 1, lwd=2, col="light blue")
  dev.off()
  
  # Sensible heat flux
  H0.max <- max(c(c$H0, s$H0))
  png(file="compare/H0.png", height=600, width=600)
  plot(c$H0, s$H0, cex=0.2, xlab="Conventional",ylab="Sonic", main="H0 (m/s)", xlim=c(0,H0.max), ylim=c(0,H0.max))
  abline(0, 1, lwd=2, col="light blue")
  h0.lm <- lm(s$H0~c$H0+1)
  print(summary(h0.lm))
  abline(h0.lm, lwd=2, col="red")
  dev.off()
  
  # Obukhov length
  L.min <- min(c(c$L, s$L))
  L.max <- max(c(c$L, s$L))
  png(file="compare/L.png", height=600, width=600)
  plot(c$L, s$L, cex=0.2, xlab="Conventional",ylab="Sonic", main="L (m/s)", xlim=c(L.min,L.max), ylim=c(L.min,L.max))
  abline(0, 1, lwd=2, col="light blue")
  dev.off()
  
}
