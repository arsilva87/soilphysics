 compressive_properties3 <- function(bulk.density, matric.suction, soil="SandyLoam") { 

    x <- bulk.density
    y <- log10(matric.suction)

    N1 <- function(x,y) 4.30 - 1.697*x + 0.307*y - 0.064*y^(2) 
    CI1 <- function(x,y) 0.2742 - 0.1749*x + 0.0679*y - 0.0142*y^(2)
    k1 <- function (x,y) 0.056 - 0.044*x + 0.019*y - 0.0033*y^(2)
   
    N2 <- function(x,y) 4.036 - 1.727*x + 0.521*y - 0.10*y^(2)
    CI2 <- function(x,y) 0.137 - 0.158*x + 0.15*y - 0.029*y^(2)
    k2 <- function(x,y) 0.051 - 0.052*x + 0.031*y - 0.006*y^(2)
    
    N <- c()
    CI <- c()
    k <- c()

    if (soil=="SandyLoam") {N <- N1(x=x, y=y)}
    if (soil=="SandyLoam") {CI <- CI1(x=x, y=y)}
    if (soil=="SandyLoam") {k <- k1(x=x, y=y)}

    if (soil=="SandyClayLoam") {N <- N2(x=x, y=y)}
    if (soil=="SandyClayLoam") {CI <- CI2(x=x, y=y)}
    if (soil=="SandyClayLoam") {k <- k2(x=x, y=y)}


    out <- data.frame(N=N,lambda=CI, k=k)
    return(out)

}
