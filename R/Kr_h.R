Kr_h <- 
function(Ks,alpha, n, h, f=0.5) 
{
    m <- 1 - (1/n)
    Se <- (1/(1+(alpha*h)^n))^m
    b <- (1-(1-Se^(n/(n-1)))^m)^2
    out <- Ks*(Se^f)*b
    return(out)

 }
