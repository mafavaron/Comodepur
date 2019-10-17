series.read <- function() {
  s <- read.csv("SonicTargetSeries_0506059_5072586.csv")
  s$Time.Stamp <- as.POSIXct(s$Time.Stamp, tz="UTC")
  names(s) <- c("Time.Stamp", "OU.s")
  c <- read.csv("PlantTargetSeries_0506059_5072586.csv")
  c$Time.Stamp <- as.POSIXct(c$Time.Stamp, tz="UTC")
  names(c) <- c("Time.Stamp", "OU.c")
  d <- merge(c,s,by="Time.Stamp",all=FALSE)
  return(d)
}

series.stats <- function(d) {
  out <- list(
    c.qnt = fivenum(d$OU.c),
    s.qnt = fivenum(d$OU.s),
    c.mean = mean(d$OU.c),
    s.mean = mean(d$OU.s),
    c.p98 = quantile(d$OU.c, 0.98),
    s.p98 = quantile(d$OU.s, 0.98),
    K.cu  = sum((d$OU.c>0 & d$OU.s<=0)),
    K.uc  = sum((d$OU.c<=0 & d$OU.s>0))
  )
}
