
soilStrength5 <- function(bulk.density,  water.content, clay.content) { 


 out <-  -566.8 + 443* bulk.density + 4.34*clay.content - 773*water.content

 for (j in 1: length(out)) { 
 if (out[j] < 0) {out[j] <- 0}
 }

 return(out)

}
