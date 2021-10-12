
Kr_theta <- function(theta,thetaS,thetaR,n,Ks,f=0.5) {
   m <- 1-(1/n)
   Se <- (theta-thetaR)/(thetaS-thetaR)
   a <- (1-(1-Se^(1/m))^m)^2
   out <- Ks*(Se^f)*a
   return(out)
  }

