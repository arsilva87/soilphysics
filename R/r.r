r <- 
function(h, surface.tension.water=0.072, water.density=1000, water.pore.contact.angle=0) 
{    
    h <- h/100
    out <- (2*surface.tension.water*cos(water.pore.contact.angle))/(water.density*9.81*h)
    out <- out*1000000
    return(out)
}
