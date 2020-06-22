compressive_properties4 <- function(matric.suction, soil = c("PloughLayer", "PloughPan")) { 

    x <- log10(matric.suction)

    N1 <- function(x) 1.4612 + 0.5080*x - 0.0723*x^2 
    CI1 <- function(x) 0.0233 + 0.0728*x - 0.0110*x^2
    k1 <- function (x) 0.0221*exp(x^-0.4530)
   
    N2 <- function(x) 1.2726 + 0.4223*x - 0.0691*x^2 
    CI2 <- function(x) 0.0112 + 0.0578*x - 0.0105*x^2
    k2 <- function (x) 0.0252*exp(x^-0.5414)
    
    N <- c()
    CI <- c()
    k <- c()

    if (soil=="PloughLayer") {N <- N1(x=x)}
    if (soil=="PloughLayer") {CI <- CI1(x=x)}
    if (soil=="PloughLayer") {k <- k1(x=x)}

    if (soil=="PloughPan") {N <- N2(x=x)}
    if (soil=="PloughPan") {CI <- CI2(x=x)}
    if (soil=="PloughPan") {k <- k2(x=x)}


    out <- data.frame(N=N,lambda=CI, k=k)
    return(out)
}
