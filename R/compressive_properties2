
 compressive_properties2 <- function(particle.density, bulk.density) {
  
   specific.volume <- particle.density/bulk.density
   N <- (-0.007 + 1.196*specific.volume)
   CI <- (-0.182 + 0.212*specific.volume)
   k <- rep(0.042, length(CI))

   out <- data.frame(EV=specific.volume, N=N,CI=CI, k=k)
   return(out)
  }
