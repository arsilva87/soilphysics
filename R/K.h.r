Kr <- 
function(Ks,alpha, n, h) 
{
   out <- ((1 - ((alpha * h)^(n - 1))*
          (1 + (alpha * h)^n)^(1/n - 1))^2) /
          (1 + (alpha * h)^n)^((n-1)/(2 * n))*Ks
   return(out)

 }
