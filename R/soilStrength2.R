
soilStrength2 <- function(bulk.density,  matric.suction, clay.content) { 

 pF <- log10(matric.suction)
 BD <- bulk.density
 clay <- clay.content

 out <- -26.51 + (25.57*BD) - (20.40*clay) + (10.26*clay*pF)

 for (j in 1: length(out)) { 
 if (out[j] < 0) {out[j] <- 0}
 }

 return(out^2)

}
