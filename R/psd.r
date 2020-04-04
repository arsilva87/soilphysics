psd <- 
function(thetaS, thetaR, alpha, n, h) 
{
    x <- h
    out <- abs((thetaS - thetaR) * (1 + (alpha * x)^n)^(1/n - 1) * (1/n - 1) * (alpha * x)^n * (n/(x * (1 + (alpha * x)^n))))
    return(out)
}
