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
  s <- get.sonic.data()
  c <- get.conv.data()
  png(file="compare/Vel.png", height=600, width=600)
  plot(c$Vel, s$Vel, cex=0.2,xlab="Conventional",ylab="Sonic", main="Vel (m/s)", xlab="Conventional", ylab="Sonic", main="Vel (m/s)")
  abline(lm(s$Vel~c$Vel+1), lwd=2, col="light blue")
  dev.off()
}
