particle.sedimentation <- 
function(d, h=0.2, g=9.81, v=0.001, Pd=2650, Wd=1000) 
{
   d <- d*10^-6
   out0 <- (h*18*v)/((d^2)*(Pd-Wd)*g)
   out1 <- out0/60
   out2 <- out1/60
   out3 <- out2/24
   rest <- data.frame(d*10^6, out0, out1, out2, out3)
   colnames(rest) <- c("diameter", "seconds","minutes","hours","days")
   return(rest)
}
