hydraulicCutOff2 <- function (theta_R,a1,a2,p1,p2,graph = FALSE,...) {
    A1 <- a1
    A2 <- a2
    h1 <- p1
    h2 <- p2
    wh <- function(x) theta_R + A1 * exp(-x/h1) + A2 * exp(-x/h2)
    fh <- function(x) A1/h1 * (x/h1 - 1) * exp(-x/h1) + A2/h2 * (x/h2 - 1) * exp(-x/h2)
    gh <- function(x) A1/h1 * exp(-x/h1) + A2/h2 * exp(-x/h2)
    dfh <- function(x) A1/h1^2 * (2 - x/h1) * exp(-x/h1) + A2/h2^2 * (2 - x/h2) * exp(-x/h2)
    kh <- function(x) ( log(10)^2 * x * fh(x) )/( 1 + x^2 * log(10)^2 * gh(x)^2 )^(3/2)
    hm <- optimize(kh, lower=1,upper=100000, maximum = T)$maximum # hPa
if (graph) {
    curve(kh, from = 1, to = 100000, ylab="k", xlab="",log="x", xaxt='n',...)
    mtext(expression(-psi),1,line=2.5)
    x <- c(1,10,100,1000,10000,100000)
    axis(1, at=x,labels=as.character(x), ...)
    abline(v=hm, col="red")
}
    log.hm <- log10(hm) # pF
    w <- wh(x=hm)
    out <- data.frame(h = hm, pF = log.hm, W = w)
    return(out)
}
