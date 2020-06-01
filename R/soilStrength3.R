
soilStrength3 <- function(bulk.density, water.content, texture=c("VeryFine","Fine","MediumFine","Medium","Coarse")) { 

 w <- water.content
 BD <- bulk.density
 PC <- c()
 if (texture == "VeryFine") {PC <- 7.71 + 112.21*BD - 2.81*w } # VeryFine
 if (texture == "Fine") {PC <- 4.19 + 202.54*BD - 10.92*w } # Fine
 if (texture == "MediumFine") {PC <- -223 + 347*BD - 7.93*w } # MediumFine
 if (texture == "Medium") {PC <- -136 + 155*BD } # Medium
 if (texture == "Coarse") {PC <- -220 + 191*BD + 2.77*w } # Coarse

 for (j in 1: length(PC)) { 
 if (PC[j] < 0) {PC[j] <- 0}
 }
 
 return(PC)
}
