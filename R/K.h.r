K.h <- 
function(Ks, alpha, n, h) 
{     
        m <-  1 - (1/n)
        x <- (alpha*h)^(n-1)
        y <- (1 + (alpha*h)^n)^-m
        A <- (1 - x*y)^2
        B <- (1 + (alpha*h)^n )^(m/2)
        out <- Ks*(A/B)
        return(out)
}
