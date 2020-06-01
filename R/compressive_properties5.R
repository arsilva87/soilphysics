
compressive_properties5 <- function(water.content, soil="SandyLoam") {

  w <- water.content
  N <- c()
  CI <- c()
  k <- c()
  if (soil=="SandyLoam") { N <- 2.430 - 0.0055*(w-11.2)^2}
  if (soil=="SandyLoam") { CI <- (N-1.572)/17}
  if (soil=="SandyLoam") { k <- CI*(0.119-(0.082*w/17))}

  if (soil=="ClayLoam") { N <- 2.813 - 0.0128*(w-17.4)^2}
  if (soil=="ClayLoam") { CI <- (N-1.557)/26}
  if (soil=="ClayLoam") { k <- CI*(0.119-(0.082*w/26))}

  out <- data.frame(N=N,lambda=CI, kappa=k)
  return(out)

 }
