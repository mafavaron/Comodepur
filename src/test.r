test <- function() {
  
  # Get 5-min test data
  d <- read.csv("soniclib/test.csv")
  
  u.m <- mean(d$u)
  v.m <- mean(d$v)
  vel <- sqrt(u.m**2 + v.m**2)
  dir <- 180/pi*atan2(-u.m, -v.m)
  dir[dir < 0] <- dir[dir < 0] + 360.0
  
  # Get hourly data
  e <- read.csv("soniclib/soniclib/20180614.00.csv")
  u.m <- mean(e$u)
  v.m <- mean(e$v)
  vel.e <- sqrt(u.m**2 + v.m**2)
  dir.e <- 180/pi*atan2(-u.m, -v.m)
  dir.e[dir.e < 0] <- dir.e[dir.e < 0] + 360.0
  tke <- 0.5*(var(e$u) + var(e$v) + var(e$w))
  mke <- 0.5*(u.m**2 + v.m**2 + mean(e$w)**2)
  
  out <- list(
    vel = vel,
    dir = dir,
    vel.60 = vel.e,
    dir.60 = dir.e,
    mke = mke,
    tke = tke
  )
  return(out)
  
}
