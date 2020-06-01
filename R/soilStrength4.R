
soilStrength4 <- function(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,OM=1.5,C=30,phi=36,texture="Clay>35", pF=1.8) { 

  PC <- c()
  if (texture=="Sand" & pF ==1.8) {PC <- 438.10*BD - 0.0008*phi^3 - 3.14*PWP - 0.11*AWC^2 - 465.60} # Sand
  if (texture=="Sand" & pF ==2.5) {PC <- 410.75*BD - 0.0007*phi^3 - 3.41*PWP - 0.35*AWC^2 - 384.71} # Sand

  if (texture=="SandLoam" & pF ==1.8) {PC <- 169.30*BD - 29.03*OM^0.5 + 6.45*Ks + 32.18*log(C) -  9.44*phi + 27.25*sin(PWP) + 119.74*log(AWC) + 19.51} # SandLoam
  if (texture=="SandLoam" & pF ==2.5) {PC <- 89.50*BD - 23.99*OM^0.5 - 2.89*Ks + 125.76*log(C) -  1.14*phi + 26.90*sin(PWP) - 51.46*log(AWC) - 77.25} # SandLoam

  if (texture=="Silt" & pF ==1.8) {PC <- 374.15*BD - 4.10*OM + 3.38*AC - 1.58*Ks^(-0.5) + 1.79*C + 1.09*PWP - 6.37*(phi)^(0.67) + 0.088*AWC^2 - 472.77} # Silt
  if (texture=="Silt" & pF ==2.5) {PC <- 460.71*BD - 20.33*OM + 9.08*AC - 2.38*Ks^(-0.5) + 2.86*C + 4.5*PWP - 20.96*(phi)^(0.67) + 0.304*AWC^2 - 610.62} # Silt

  if (texture=="Clay<35" & pF ==1.8) {PC <- exp(0.843*BD - 0.544*(Ks)^0.33 - 0.022*PWP + 7.03*C^(-1) + 0.024*phi - 0.015*AWC + 0.725)} # Clay<35
  if (texture=="Clay<35" & pF ==2.5) {PC <- exp(0.844*BD - 0.456*(Ks)^0.33 - 0.026*PWP + 12.88*C^(-1) + 0.003*phi - 0.016*AWC + 1.419)} # Clay<35

  if (texture=="Clay>35" & pF ==1.8) {PC <- 4.59*BD - 1.02*OM - 16.43*(Ks)^0.33 + 0.31*PWP - 1.57*AWC + 3.55*C + 1.18*phi - 18.03} # Clay>35
  if (texture=="Clay>35" & pF ==2.5) {PC <- 70.65*BD - 0.55*OM - 7.01*(Ks)^0.33 + 1.32*PWP - 1.08*AWC + 1.72*C + 1.05*phi - 100.94} # Clay>35

 for (j in 1: length(PC)) { 
 if (PC[j] < 0) {PC[j] <- 0}
 }

  return(PC)
}
