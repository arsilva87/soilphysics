compressive_properties <- function(water.content, soil = c("Loess", "Calcareous")) {
  w <- water.content
  N <- c()
  CI <- c()
  if (soil=="Loess") { N <- 1.9997 + 0.2629*w - 0.02753*w^2 + 0.00111*w^3 - (1.6*10^-5)*w^4}
  if (soil=="Loess") { CI <- 0.1402 + 0.00192*w + 0.00021*w^2 - (1.3*10^-5)*w^3}

  if (soil=="Calcareous") { N <- 2.4196 + 0.13767*w - 0.02035*w^2 + 0.00121*w^3 - (2.4*10^-5)*w^4}
  if (soil=="Calcareous") { CI <- 0.08051 + 0.03131*w - 0.00502*w^2 + 0.00031*w^3 - (6*10^-6)*w^4}

  out <- data.frame(N=N,CI=CI)
  return(out)
 }
