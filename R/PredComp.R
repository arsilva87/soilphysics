
server_PredComp <- function(input, output) {
  

# StressTraffic -------------
  
  stressTraffic <- function (inflation.pressure, recommended.pressure, tyre.diameter, 
                             tyre.width, wheel.load, conc.factor, layers, plot.contact.area = FALSE, 
                             ...) 
  {
    wheel.load <- (wheel.load * 9.81)/1000
    stressmax <- 34.4 + (1.13 * inflation.pressure) + (0.72 * 
                                                         wheel.load) - 33.4 * log(inflation.pressure/recommended.pressure)
    area.length <- 0.47 + 0.11 * (tyre.diameter^2) - 0.16 * log(inflation.pressure/recommended.pressure)
    n <- 2.1 * (tyre.width * tyre.diameter)^2 + 2
    wx <- function(x) tyre.width/2 * (1 - (2 * x/area.length)^n)^(1/n)
    ly <- function(y) area.length/2 * (1 - (2 * y/tyre.width)^n)^(1/n)
    contact.area <- 4 * integrate(wx, 0, area.length/2)$value
    fstressXY <- function(x, y, alpha) {
      x <- abs(x)
      y <- abs(y)
      delta <- 9.3 - 7.3 * tyre.width
      facx <- ifelse(x <= ly(y), x/ly(y), 1)
      facy <- ifelse(y <= wx(x), wx(x), 1)
      dis.hor <- function(y) ((tyre.width/2) - y) * exp(-delta * 
                                                          ((tyre.width/2) - y))
      o <- optimize(dis.hor, interval = c(0, tyre.width/2), 
                    maximum = TRUE)
      C <- 1/o$objective
      stress <- stressmax * (1 - facx^alpha) * (C * ((tyre.width/2) - 
                                                       y) * exp(-delta * ((tyre.width/2) - y)))
      return(stress)
    }
    x <- seq(0, area.length/2, by = 0.01)
    x <- seq(-max(x), max(x), by = 0.01)
    y <- seq(-tyre.width/2, tyre.width/2, by = 0.01)
    alpha <- seq(1, 16, length = 100)
    dstress = NULL
    for (j in 1:100) {
      dstress[j] <- (sum(outer(X = x, Y = y, fstressXY, alpha = alpha[j]), 
                         na.rm = TRUE) * (0.01^2) - (wheel.load))^2
    }
    alpha.O <- alpha[which.min(dstress)]
    mstress <- round(outer(x, y, fstressXY, alpha = alpha.O), 
                     0)
    mstress[is.nan(mstress)] <- 0
    dimnames(mstress) <- list(round(x, 2), round(y, 2))
    if (plot.contact.area) {
      contour(x, y, z = mstress, method = "edge", xlab = "Tyre footprint length (m)", 
              ylab = "Tyre width (m)", ...)
      box()
    }
    Pi <- mstress * (0.01 * 0.01)
    F.max <- sum(as.vector(Pi), na.rm = T)
    kg.max <- (F.max * 1000)/9.81
    Df <- ((wheel.load - F.max) * 1000)/9.81
    Pi <- t(Pi)
    ZStress <- function(Layers, conc.factor) {
      Z.stress <- c()
      for (j in 1:length(Layers)) {
        r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
        R <- sqrt(as.vector(r)^2 + Layers[j]^2)
        Pi <- as.vector(Pi)
        coss <- (Layers[j]/R)
        stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
          (coss^(conc.factor[j] - 2))
        stress.layers <- stress.R * coss^2
        Z.stress[j] <- round(sum(as.vector(stress.layers), 
                                 na.rm = TRUE), 0)
      }
      Z.stress
    }
    XStress <- function(Layers, conc.factor) {
      X.stress <- c()
      for (j in 1:length(Layers)) {
        r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
        R <- sqrt(as.vector(r)^2 + Layers[j]^2)
        Pi <- as.vector(Pi)
        coss <- (Layers[j]/R)
        seno <- sqrt(1 - (coss^2))
        coss2 <- r
        for (k in 1:length(x)) {
          coss2[, k] <- (abs(y)/coss2[, k])
        }
        stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
          (coss^(conc.factor[j] - 2))
        stress.layers <- stress.R * (seno^2) * (as.vector(coss2)^2)
        X.stress[j] <- round(sum(as.vector(stress.layers), 
                                 na.rm = T), 0)
      }
      X.stress
    }
    YStress <- function(Layers, conc.factor) {
      Y.stress <- c()
      for (j in 1:length(Layers)) {
        r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
        R <- sqrt(as.vector(r)^2 + Layers[j]^2)
        Pi <- as.vector(Pi)
        coss <- (Layers[j]/R)
        seno <- sqrt(1 - (coss^2))
        seno2 <- r
        for (i in 1:length(y)) {
          seno2[i, ] <- (abs(x)/seno2[i, ])
        }
        stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
          (coss^(conc.factor[j] - 2))
        stress.layers <- stress.R * (seno^2) * (as.vector(seno2)^2)
        Y.stress[j] <- round(sum(as.vector(stress.layers), 
                                 na.rm = T), 0)
      }
      Y.stress
    }
    pStress <- function(Layers, conc.factor) {
      Z <- ZStress(Layers, conc.factor)
      X <- XStress(Layers, conc.factor)
      Y <- YStress(Layers, conc.factor)
      MEAN <- round((Z + X + Y)/3, 0)
      return(MEAN)
    }
    Indices <- c("Applied Wheel Load", "Modeled Wheel Load", 
                 "Diference")
    Loads <- c((wheel.load * 1000)/9.81, round(kg.max, 0), round(Df, 
                                                                 0))
    Loads <- data.frame(Indices, Loads)
    colnames(Loads) <- c("Parameters", "Loads (kg)")
    Parameters <- c("Max Stress", "Contact Area", "Area Length", 
                    "Area Width")
    Units <- c("kPa  ", "m^2  ", "m   ", "m   ")
    Value <- c(round(stressmax, 0), round(contact.area, 2), round(area.length, 
                                                                  2), tyre.width)
    Area <- data.frame(Parameters, Value, Units)
    Layers <- layers
    stress.X <- XStress(layers, conc.factor)
    stress.Y <- YStress(layers, conc.factor)
    stress.Z <- ZStress(layers, conc.factor)
    p <- round((stress.Z + stress.X + stress.Y)/3, 0)
    stress <- data.frame(Layers, stress.Z, p)
    colnames(stress) <- c("Layers (m)", "Zstress", "p")
    out <- list(Area = Area, Loads = Loads, Stress = stress, 
                stress.matrix = mstress, fZStress = ZStress, fmeanStress = pStress, 
                fXStress = XStress, fYStress = YStress, conc.factor = conc.factor)
    class(out) <- "stressTraffic"
    return(out)
  }
  
  
  
# soilDeformation --------------
  
  soilDeformation <- function (stress, p.density, iBD, N, CI, k, k2, m)
  {
    lp <- log(stress)
    Vi <- (p.density/iBD)
    NYL <- c()
    for (j in 1:length(N)) {
      NYL[j] <- N[j] + CI[j] * (0 - m[j])
    }
    XRCL2 <- c()
    for (j in 1:length(N)) {
      XRCL2[j] <- (NYL[j] - Vi[j])/(-k[j] + CI[j])
    }
    YRCL2 <- c()
    for (j in 1:length(N)) {
      YRCL2[j] <- (Vi[j] - k[j] * XRCL2[j])
    }
    NRCL2 <- c()
    for (j in 1:length(N)) {
      NRCL2[j] <- (YRCL2[j] + k2[j] * XRCL2[j])
    }
    XRCL2.VCL <- c()
    for (j in 1:length(N)) {
      XRCL2.VCL[j] <- (N[j] - NRCL2[j])/(-k2[j] + CI[j])
    }
    YRCL2.VCL <- c()
    for (j in 1:length(N)) {
      YRCL2.VCL[j] <- (NRCL2[j] - k2[j] * XRCL2.VCL[j])
    }
    fVCL <- function(x) N[1] - CI[1] * x
    fYL <- function(x) NYL[1] - CI[1] * x
    fRCL <- function(x) Vi[1] - k[1] * x
    NRCL <- fRCL(0)
    fRCL2 <- function(x) NRCL2[1] - k2[1] * x
    volume <- c()
    for (j in 1:length(N)) {
      if (lp[j] > 0 & lp[j] <= XRCL2[j]) {
        volume[j] <- (Vi[j])
      }
      else if (lp[j] > XRCL2[j] & lp[j] <= XRCL2.VCL[j]) {
        volume[j] <- (NRCL2[j] - k2[j] * lp[j])
        volume[j] <- volume[j] + k[j] * lp[j]
      }
      else if (lp[j] > XRCL2.VCL[j]) {
        volume[j] <- (N[j] - CI[j] * lp[j])
        volume[j] <- volume[j] + k[j] * lp[j]
      }
    }
    round(iBD, 3)
    fBD <- round((p.density/volume), 4)
    vf <- (p.density/fBD)
    increasing <- round(((fBD * 100)/iBD) - 100, 2)
    out <- data.frame(iBD, fBD, round(Vi, 4), round(vf, 4), increasing)
    colnames(out) <- c("iBD", "fBD", "vi", "vf", "I%")
    return(out)
  }
  

  
# Pedotransfers for precompression stress -------------------------------------
  
  
# Severiano et al. (2013)
  
  soilStrength <- function (clay.content, matric.suction = NULL, water.content = NULL) 
  {
    if (!is.null(matric.suction) || !is.null(water.content)) {
      if (is.null(clay.content)) 
        warning("To estimate soil strength, please inform water.content or matric.suction")
      if (is.numeric(matric.suction) & is.numeric(water.content)) 
        warning("To estimate soil strength, please inform only one of them: water.content or matric.suction")
    }
    vanG.matric <- function(theta, thetaR, thetaS, alpha, n) {
      S <- (theta - thetaR)/(thetaS - thetaR)
      f <- n/(1 - n)
      h <- (1/alpha) * ((S^f) - 1)^(1/n)
      out <- data.frame(theta, h)
      return(out)
    }
    thetaS <- c(0.42, 0.45, 0.46, 0.5, 0.51)
    thetaR <- c(0.049356, 0.08689, 0.10696, 0.125941, 0.139358)
    alpha <- c(0.79, 0.72, 1.66, 2.04, 2.27)
    n <- c(1.72, 1.56, 1.52, 1.47, 1.38)
    m <- c(0.42, 0.36, 0.34, 0.33, 0.28)
    matric.suction <- matric.suction/10
    pre.cons.water <- function(clay.content, water.content) {
      mh <- c()
      for (j in 1:length(clay.content)) {
        if (clay.content[j] <= 20) {
          mh[j] <- vanG.matric(theta = water.content[j], 
                               thetaR = thetaR[1], thetaS = thetaS[1], alpha = alpha[1], 
                               n = n[1])$h
        }
        else if (clay.content[j] > 20 & clay.content[j] <= 
                 31) {
          mh[j] <- vanG.matric(theta = water.content[j], 
                               thetaR = thetaR[2], thetaS = thetaS[2], alpha = alpha[2], 
                               n = n[2])$h
        }
        else if (clay.content[j] > 31 & clay.content[j] <= 
                 37) {
          mh[j] <- vanG.matric(theta = water.content[j], 
                               thetaR = thetaR[3], thetaS = thetaS[3], alpha = alpha[3], 
                               n = n[3])$h
        }
        else if (clay.content[j] > 37 & clay.content[j] <= 
                 52) {
          mh[j] <- vanG.matric(theta = water.content[j], 
                               thetaR = thetaR[4], thetaS = thetaS[4], alpha = alpha[4], 
                               n = n[4])$h
        }
        else {
          mh[j] <- vanG.matric(theta = water.content[j], 
                               thetaR = thetaR[5], thetaS = thetaS[5], alpha = alpha[5], 
                               n = n[5])$h
        }
      }
      return(round(mh, 2))
    }
    if (length(matric.suction) > 0) {
      matric.suction <- matric.suction
    }
    else {
      matric.suction <- pre.cons.water(clay.content = clay.content, 
                                       water.content = water.content)
    }
    pcs <- c()
    for (j in 1:length(clay.content)) {
      if (clay.content[j] < 20) {
        pcs[j] <- round(129 * matric.suction[j]^(0.15), 0)
      }
      else if (clay.content[j] >= 20 & clay.content[j] <= 31) {
        pcs[j] <- round(123.3 * matric.suction[j]^(0.13), 
                        0)
      }
      else if (clay.content[j] > 31 & clay.content[j] <= 37) {
        pcs[j] <- round(85 * matric.suction[j]^(0.17), 0)
      }
      else if (clay.content[j] > 37 & clay.content[j] <= 52) {
        pcs[j] <- round(70.1 * matric.suction[j]^(0.16), 
                        0)
      }
      else if (clay.content[j] > 52) {
        pcs[j] <- round(62.7 * matric.suction[j]^(0.15), 
                        0)
      }
    }
    pcs05 <- pcs * 0.5
    pcs11 <- pcs * 1.1
    soil.strength <- data.frame(pcs, pcs05, pcs11)
    colnames(soil.strength) <- c("Pc", "LL.Pc", "UL.Pc")
    return(soil.strength)
  }
  
  
  
  # Schjonning & Lamande (2018)
  
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
  
  
  # Saffih-Hdadi et al. (2009) 
  
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
  
  
  
  # Horn & Fleige (2003)
  
  soilStrength4 <- function(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,OM=1.5,C=30,phi=36,texture="Sand at pF 1.8") { 
    
    PC <- c()
    if (texture=="Sand at pF 1.8") {PC <- 438.10*BD - 0.0008*phi^3 - 3.14*PWP - 0.11*AWC^2 - 465.60} # Sand
    if (texture=="Sand at pF 2.5") {PC <- 410.75*BD - 0.0007*phi^3 - 3.41*PWP - 0.35*AWC^2 - 384.71} # Sand
    
    if (texture=="Sandy Loam at pF 1.8") {PC <- 169.30*BD - 29.03*OM^0.5 + 6.45*Ks + 32.18*log(C) -  9.44*phi + 27.25*sin(PWP) + 119.74*log(AWC) + 19.51} # SandLoam
    if (texture=="Sandy Loam at pF 2.5") {PC <- 89.50*BD - 23.99*OM^0.5 - 2.89*Ks + 125.76*log(C) -  1.14*phi + 26.90*sin(PWP) - 51.46*log(AWC) - 77.25} # SandLoam
    
    if (texture=="Silt at pF 1.8") {PC <- 374.15*BD - 4.10*OM + 3.38*AC - 1.58*Ks^(-0.5) + 1.79*C + 1.09*PWP - 6.37*(phi)^(0.67) + 0.088*AWC^2 - 472.77} # Silt
    if (texture=="Silt at pF 2.5") {PC <- 460.71*BD - 20.33*OM + 9.08*AC - 2.38*Ks^(-0.5) + 2.86*C + 4.5*PWP - 20.96*(phi)^(0.67) + 0.304*AWC^2 - 610.62} # Silt
    
    if (texture=="Clay < 35 at pF 1.8") {PC <- exp(0.843*BD - 0.544*(Ks)^0.33 - 0.022*PWP + 7.03*C^(-1) + 0.024*phi - 0.015*AWC + 0.725)} # Clay<35
    if (texture=="Clay < 35 at pF 2.5") {PC <- exp(0.844*BD - 0.456*(Ks)^0.33 - 0.026*PWP + 12.88*C^(-1) + 0.003*phi - 0.016*AWC + 1.419)} # Clay<35
    
    if (texture=="Clay > 35 at pF 1.8") {PC <- 4.59*BD - 1.02*OM - 16.43*(Ks)^0.33 + 0.31*PWP - 1.57*AWC + 3.55*C + 1.18*phi - 18.03} # Clay>35
    if (texture=="Clay > 35 at pF 2.5") {PC <- 70.65*BD - 0.55*OM - 7.01*(Ks)^0.33 + 1.32*PWP - 1.08*AWC + 1.72*C + 1.05*phi - 100.94} # Clay>35
    
    for (j in 1: length(PC)) { 
      if (PC[j] < 0) {PC[j] <- 0}
    }
    
    return(PC)
    
  }
  
  
  
  
  
# Imhoff et al. (2004)
  
  soilStrength5 <- function(bulk.density,  water.content, clay.content) { 
    
    
    out <-  -566.8 + 443* bulk.density + 4.34*clay.content - 773*water.content
    
    for (j in 1: length(out)) { 
      if (out[j] < 0) {out[j] <- 0}
    }
    
    return(out)
    
  }
  
  
  
  
  
# Pedo-tranfer for compressive properties -------------------------------------
  
  
  # Defossez et al. (2003): water content range from 5 to 30 %
  
  compressive.properties1 <- function(water.content, soil="Loess") {
    
    w <- water.content
    N <- c()
    CI <- c()
    if (soil=="Loess") { N <- 1.9997 + 0.2629*w - 0.02753*w^2 + 0.00111*w^3 - (1.6*10^-5)*w^4}
    if (soil=="Loess") { CI <- 0.1402 + 0.00192*w + 0.00021*w^2 - (1.3*10^-5)*w^3}
    
    if (soil=="Calcareous") { N <- 2.4196 + 0.13767*w - 0.02035*w^2 + 0.00121*w^3 - (2.4*10^-5)*w^4}
    if (soil=="Calcareous") { CI <- 0.08051 + 0.03131*w - 0.00502*w^2 + 0.00031*w^3 - (6*10^-6)*w^4}
    k <- rep(0.0058, length(CI))
    
    out <- data.frame(N=N,lambda=CI, kappa=k)
    return(out)
    
  }
  


# Keller & Arvidsson
  
  compressive.properties2 <- function(particle.density, bulk.density) {
    
    specific.volume <- particle.density/bulk.density
    N <- (-0.007 + 1.196*specific.volume)
    CI <- (-0.182 + 0.212*specific.volume)
    k <- rep(0.042, length(CI))
    
    out <- data.frame("Especific volume"=specific.volume, N=N,lambda=CI, kappa=k)
    return(out)
  }
  

# Lima et al. (2018)
  compressive.properties3 <- function(bulk.density, matric.suction, soil="SandyLoam") { 
    
    x <- bulk.density
    y <- log10(matric.suction)
    
    N1 <- function(x,y) 4.30 - 1.697*x + 0.307*y - 0.064*y^(2) 
    CI1 <- function(x,y) 0.2742 - 0.1749*x + 0.0679*y - 0.0142*y^(2)
    k1 <- function (x,y) 0.056 - 0.044*x + 0.019*y - 0.0033*y^(2)
    
    N2 <- function(x,y) 4.036 - 1.727*x + 0.521*y - 0.10*y^(2)
    CI2 <- function(x,y) 0.137 - 0.158*x + 0.15*y - 0.029*y^(2)
    k2 <- function(x,y) 0.051 - 0.052*x + 0.031*y - 0.006*y^(2)
    
    N <- c()
    CI <- c()
    k <- c()
    
    if (soil=="SandyLoam") {N <- N1(x=x, y=y)}
    if (soil=="SandyLoam") {CI <- CI1(x=x, y=y)}
    if (soil=="SandyLoam") {k <- k1(x=x, y=y)}
    
    if (soil=="SandyClayLoam") {N <- N2(x=x, y=y)}
    if (soil=="SandyClayLoam") {CI <- CI2(x=x, y=y)}
    if (soil=="SandyClayLoam") {k <- k2(x=x, y=y)}
    
    
    out <- data.frame(N=N,lambda=CI, kappa=k)
    return(out)
    
  }
  

# Lima et al. (2020)
  compressive.properties4 <- function(matric.suction, soil="PloughLayer") { 
    
    
    x <- log10(matric.suction)
    
    N1 <- function(x) 1.4612 + 0.5080*x - 0.0723*x^2 
    CI1 <- function(x) 0.0233 + 0.0728*x - 0.0110*x^2
    k1 <- function (x) 0.0221*exp(x^-0.4530)
    
    N2 <- function(x) 1.2726 + 0.4223*x - 0.0691*x^2 
    CI2 <- function(x) 0.0112 + 0.0578*x - 0.0105*x^2
    k2 <- function (x) 0.0252*exp(x^-0.5414)
    
    N <- c()
    CI <- c()
    k <- c()
    
    if (soil=="PloughLayer") {N <- N1(x=x)}
    if (soil=="PloughLayer") {CI <- CI1(x=x)}
    if (soil=="PloughLayer") {k <- k1(x=x)}
    
    if (soil=="PloughPan") {N <- N2(x=x)}
    if (soil=="PloughPan") {CI <- CI2(x=x)}
    if (soil=="PloughPan") {k <- k2(x=x)}
    
    
    out <- data.frame(N=N,lambda=CI, kappa=k)
    return(out)
    
  }

  

# O'Sullivan et al. (1999)
  
  compressive.properties5 <- function(water.content, soil="SandyLoam") {
    
    w <- water.content
    N <- c()
    CI <- c()
    k <- c()
    if (soil=="SandyLoam") { N <- 2.430 - 0.0055*(w-11.2)^2}
    if (soil=="SandyLoam") { CI <- (N-1.572)/17}
    if (soil=="SandyLoam") { k <- CI*(0.119-(0.082*w/17))}
    
    if (soil=="ClayLoam") { N <- 2.813 - 0.0128*(w-17.4)^2}
    if (soil=="ClayLoam") { CI <- (N-1.557)/26}
    if (soil=="ClayLoam") { k <- CI*(0.119-(0.082*w/26))}
    
    out <- data.frame(N=N,lambda=CI, kappa=k)
    return(out)
    
  }
  
  
  
  
  


# --------------------------------- NAV -------------------------------------#


# NAV 1 STRESS CALCULATION ----------------------------------------------------
  
  output$plotstresscal1 <- renderPlot({
    
    layers <- c(0.05,0.15,0.3,0.4,0.5,0.6)
    
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(5,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    par(mar=c(5,10.5,5,2))
    image.plot(x = as.numeric(rownames(stress$stress.matrix)),
               y = as.numeric(colnames(stress$stress.matrix)), 
               z = stress$stress.matrix,  legend.mar=11.5,
               xlab="Tyre footprint length (m)", ylab="Tyre width (m)",
               main="Contact stress (kPa)") 
    mtext("(<< Driving direction)",1,line=3.8, cex=0.9)
    
  })
  



  output$plotstresscal2 <- renderPlot({
    
    layers <- seq(0.03,1,by=0.01)
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(input$conc.factor,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    par(mar=c(2,8,5,8))
    plot(x=stress$Stress$Zstress,y=layers,xaxt='n',lwd=2,type="l",
         xlim=c(1,max(stress$Stress$Zstress)+100), ylim=c(1,0),
         xlab="",ylab="")
    axis(3)
    mtext("Depth (m)",2,line=2.5)
    mtext("Vertical stress (kPa)",3,line=2.5)
    
    
  })
  
  
  

  output$plotstresscal3 <- renderPlot({
    
    layers <- seq(0.03,1,by=0.01)
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(input$conc.factor,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    par(mar=c(2,8,5,8))
    plot(x=stress$Stress$p,y=layers,xaxt='n',lwd=2,type="l",
         xlim=c(1,max(stress$Stress$p)+100), ylim=c(1,0),
         xlab="",ylab="")
    axis(3)
    mtext("Depth (m)",2,line=2.5)
    mtext("Mean normal stress (kPa)",3,line=2.5)
    
    
  })
  
  
  
  output$outAREA <- renderTable({
    
    layers <- seq(0.03,1,by=0.01)
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(input$conc.factor,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    

    out <- data.frame(stress$Area[1,2],stress$Area[2,2],stress$Area[3,2],stress$Area[4,2])
    colnames(out) <- c("Maximum stress (kPa)", "Contact area (m^2)", "Contact length (m)", 
                       "Contact Width (m)")
    out
    
    
  })
  
  
  
  
  
  
  downloadData1 <- reactive({
    
    
    layers <- seq(0.03,1,by=0.01)
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(input$conc.factor,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    out <- data.frame(Depth=layers,"Vertical stress"=stress$Stress$Zstress, 
                      "Mean normal stress"=stress$Stress$p)
    out
    
    
  })
  

  
  output$downloadData1 <- downloadHandler(
    filename = function(){"StressPropagation.csv"}, 
    content = function(fname){
      write.csv(downloadData1(), fname,row.names = FALSE)
    }
  )
  
  
  
  
  
  downloadData2 <- reactive({
    
    
    layers <- seq(0.03,1,by=0.01)
    stress <-stressTraffic(inflation.pressure=input$inflation.pressure, 
                           recommended.pressure=input$recommended.pressure, 
                           tyre.diameter=input$tyre.diameter, 
                           tyre.width=input$tyre.width, 
                           wheel.load=input$wheel.load, 
                           conc.factor=rep(input$conc.factor,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    

    out <- stress$stress.matrix
    # out <- write.csv(x=stress$stress.matrix, file="contactstress.csv")
    out
    
    
  })
  
  
  
  output$downloadData2 <- downloadHandler(
    filename = function(){"ContactStress.csv"}, 
    content = function(fname){
      write.csv(downloadData2(), fname,row.names = TRUE)
    }
  )

  
# ------------------------------------------------------------------------------
 

  
  
  
  
  
  
  
# NAV 2 PRECOMPRESSION STRESS ---------------------------------------------------
  

  output$Horn2003 <- renderTable({
    
  out <- soilStrength4(BD=input$BD.Horn,AC=input$AC.Horn,
                       AWC=input$AWC.Horn,
                       PWP=input$PWP.Horn,Ks=input$Ks.Horn,
                       OM=input$OM.Horn,
                       C=input$C.Horn,phi=input$angle.Horn,
                       texture=input$Horn2003type)
  
  classification <- c()
  if (out < 30) {classification <- "Very low"}
  if (out > 30 & out < 60) {classification <- "Low"}
  if (out > 60 & out < 90) {classification <- "Mean"}
  if (out > 90 & out < 120) {classification <- "High"}
  if (out > 120 & out < 150) {classification <- "Very High"}
  if (out > 150) {classification <- "Extremely High"}
  
  soiltype <- input$Horn2003type
  m <- matrix(nrow=1, ncol=3)
  m[1,1] <- soiltype
  m[1,2] <- round(out,0)
  m[1,3] <- classification
  colnames(m) <- c("Soil", "PC (kPa)","Classification")
  m
  
  
    
  })
  
  
  
  output$Imhoff2004<- renderTable({
    
    out <-   soilStrength5(bulk.density=input$BDImhoff, 
                           water.content=input$wImhoff,
                           clay.content=input$clayImhoff)
    
    classification <- c()
    if (out < 30) {classification <- "Very low"}
    if (out > 30 & out < 60) {classification <- "Low"}
    if (out > 60 & out < 90) {classification <- "Mean"}
    if (out > 90 & out < 120) {classification <- "High"}
    if (out > 120 & out < 150) {classification <- "Very High"}
    if (out > 150) {classification <- "Extremely High"}
    
    
    m <- matrix(nrow=1, ncol=3)
    m[1,1] <- "Various soils"
    m[1,2] <- round(out,0)
    m[1,3] <- classification
    colnames(m) <- c("Soil","PC (kPa)","Classification")
    m
    
    
    
  })
  

  
  output$Saffih2009 <- renderTable({
    
    out <-   soilStrength3(bulk.density=input$BDSaffih, 
                           water.content=input$wSaffih,
                           texture=input$soilSaffih)
    
    classification <- c()
    if (out < 30) {classification <- "Very low"}
    if (out > 30 & out < 60) {classification <- "Low"}
    if (out > 60 & out < 90) {classification <- "Mean"}
    if (out > 90 & out < 120) {classification <- "High"}
    if (out > 120 & out < 150) {classification <- "Very High"}
    if (out > 150) {classification <- "Extremely High"}
    
    
    soiltype <- input$soilSaffih
    m <- matrix(nrow=1, ncol=3)
    m[1,1] <- soiltype
    m[1,2] <- round(out,0)
    m[1,3] <- classification
    colnames(m) <- c("Texture", "PC (kPa)","Classification")
    m
    
    
    
  })
  
  
  
  
  output$severiano2013 <- renderTable({
    
    out <-   soilStrength(clay.content=input$claySeveriano, 
                          matric.suction = input$matricSeveriano)$Pc
    
    classification <- c()
    if (out < 30) {classification <- "Very low"}
    if (out > 30 & out < 60) {classification <- "Low"}
    if (out > 60 & out < 90) {classification <- "Mean"}
    if (out > 90 & out < 120) {classification <- "High"}
    if (out > 120 & out < 150) {classification <- "Very High"}
    if (out > 150) {classification <- "Extremely High"}
    
    
    m <- matrix(nrow=1, ncol=3)
    m[1,1] <- "Oxisols"
    m[1,2] <- round(out,0)
    m[1,3] <- classification
    colnames(m) <- c("Soil","PC (kPa)","Classification")
    m
    
    
    
  })
  
  

  
  output$lamande2018 <- renderTable({
    
    out <-   soilStrength2(bulk.density=input$BDLamande,  
                           matric.suction=input$matricLamande, 
                           clay.content=(input$clayLamande/100))
    
    classification <- c()
    if (out < 30) {classification <- "Very low"}
    if (out > 30 & out < 60) {classification <- "Low"}
    if (out > 60 & out < 90) {classification <- "Mean"}
    if (out > 90 & out < 120) {classification <- "High"}
    if (out > 120 & out < 150) {classification <- "Very High"}
    if (out > 150) {classification <- "Extremely High"}
    
    
    m <- matrix(nrow=1, ncol=3)
    m[1,1] <- "Luvisol"
    m[1,2] <- round(out,0)
    m[1,3] <- classification
    colnames(m) <- c("Soil","PC (kPa)","Classification")
    m
    
    
    
  })
  
  
  
# --------------------------------------------------------------------------------

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# NAV 3 COMPRESSIVE PROPERTIES --------------------------------------------------
  

  output$Sullivan1999 <- renderTable({
    
    out <- compressive.properties5(water.content=input$w.Sullivan, 
                                   soil=input$Sullivantype)
    
    
    soiltype <- input$Sullivantype
    m <- matrix(nrow=1, ncol=4)
    m[1,1] <- soiltype
    m[1,2] <- round(out$N,4)
    m[1,3] <- round(out$lambda,4)
    m[1,4] <- round(out$k,4)
    colnames(m) <- c("Soil", "N","Lambda", "Kappa")
    m
    
    
  })
  
  
  output$Defossez2003 <- renderTable({
    
    out <- compressive.properties1(water.content=input$w.Defossez, 
                                   soil=input$Defossez2003type)
    

    soiltype <- input$Defossez2003type
    m <- matrix(nrow=1, ncol=4)
    m[1,1] <- soiltype
    m[1,2] <- round(out$N,4)
    m[1,3] <- round(out$lambda,4)
    m[1,4] <- round(out$k,4)
    colnames(m) <- c("Soil", "N","Lambda", "Kappa")
    m
    
  })
  
  
  
  output$Keller2007 <- renderTable({
    
    out <-  compressive.properties2(particle.density=input$PDKeller, 
                                    bulk.density=input$BDKeller)
    
    

    soiltype <- "All soils"
    m <- matrix(nrow=1, ncol=4)
    m[1,1] <- soiltype
    m[1,2] <- round(out$N,4)
    m[1,3] <- round(out$lambda,4)
    m[1,4] <- round(out$k,4)
    colnames(m) <- c("Soil", "N","Lambda", "Kappa")
    m
    
  })
  
  
  output$Lima2018 <- renderTable({
    
    out <- compressive.properties3(bulk.density=input$BDLima2018, 
                                   matric.suction=input$matricLima2018, 
                                   soil=input$Lima2018type)
    
    
    soiltype <- input$Lima2018type
    m <- matrix(nrow=1, ncol=4)
    m[1,1] <- soiltype
    m[1,2] <- round(out$N,4)
    m[1,3] <- round(out$lambda,4)
    m[1,4] <- round(out$k,4)
    colnames(m) <- c("Soil", "N","Lambda", "Kappa")
    m
    
    
  })
  
  
  output$Lima2020 <- renderTable({
    
    out <- compressive.properties4(matric.suction=input$matricLima2020, 
                                   soil=input$Lima2020type)
    
    
    soiltype <- input$Lima2020type
    m <- matrix(nrow=1, ncol=5)
    m[1,1] <- "SandLoam"
    m[1,2] <- soiltype
    m[1,3] <- round(out$N,4)
    m[1,4] <- round(out$lambda,4)
    m[1,5] <- round(out$k,4)
    colnames(m) <- c("Soil","Condition", "N","Lambda", "Kappa")
    m
    
  })
  

# ------------------------------------------------------------------------------

  
  
  
  
  
  
  
  
# NAV 4, RISK OF COMPACTION ----------------------------------------------------

  
  
  output$plotrisk11 <- renderPlot({
    
    layers <- c(0.05,0.1,0.20,0.30,0.40,0.50,0.60)
    stress <-stressTraffic(inflation.pressure=input$risk1.IP, 
                           recommended.pressure=input$risk1.RIP, 
                           tyre.diameter=input$risk1.TD, 
                           tyre.width=input$risk1.TW, 
                           wheel.load=input$risk1.WL, 
                           conc.factor=rep(input$risk1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    
    f.risk <- function(x) {
      
      factor <- c()
      if (x==1) {factor <- c(1,1,NA)}
      if (x==2) {factor <- c(0.5,1.1,NA)}
      if (x==3) {factor <- c(0.66,1.25,0.83)}
      if (x==4) {factor <- c(input$risk1.userdefine/100,NA)}
      return(factor)
    }
    
    
    factor.riskLOWER <- f.risk(input$risk1terra)[1]
    factor.riskUUPPER <- f.risk(input$risk1terra)[2]
    factor.riskmedium <- f.risk(input$risk1terra)[3]

    
    low <- c(input$aP1,input$aP2,input$aP3,input$aP4,
             input$aP5,input$aP6)*factor.riskLOWER
    
    upper <- c(input$aP1,input$aP2,input$aP3,input$aP4,
               input$aP5,input$aP6)*factor.riskUUPPER
    
    medium <- c(input$aP1,input$aP2,input$aP3,input$aP4,
                input$aP5,input$aP6)*factor.riskmedium


    
    stress_max <- stress$Area[1,2]
    strenth_max <- max(upper)
    lim <- c()
    
    {
      if (stress_max > strenth_max) {lim <- stress_max}
      else {lim <- strenth_max}
    }
    
    par(mar=c(3.8,3.4,4,1))
    plot(x = 1, y = 1, 
         xlim=c(0,lim),ylim=c(0.7,0),xaxt = "n",
         ylab = "",xlab ="", type="l", main="", lwd=2)
    axis(3)
    mtext("Depth (m)",side=2,line=2.2)
    
    
    if (input$meanRISK==FALSE){

    stressz <- stress$Stress$Z
    layers <- stress$Stress$Layers
    points(x=stressz[-1], y=layers[-1], pch=15)
    points(x=stressz, y=layers, pch=15, type="l")
    mtext("Vertical stress (kPa)",side=3,line=2.2)
    }
    
    
    if (input$meanRISK==TRUE){
      
      stressp <- stress$Stress$p
      layers <- stress$Stress$Layers
      points(x=stressp[-1], y=layers[-1], pch=15)
      points(x=stressp, y=layers, pch=15, type="l")
      mtext("Mean normal stress (kPa)",side=3,line=2.2)
    }
    
    
    
    
    
    
    if (input$risk1terra==1) {
      L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
             seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
             seq(0.55,0.70,len=10))
      
      lower <- c(rep(low[1],10),rep(low[2],10),rep(low[3],10),
               rep(low[4],10),rep(low[5],10),rep(low[6],10))
      
      upperr <- c(rep(upper [1],10),rep(upper[2],10),rep(upper[3],10),
                 rep(upper[4],10),rep(upper[5],10),rep(upper[6],10))
      
      mediumm <- c(rep(medium[1],10),rep(medium[2],10),rep(medium[3],10),
                  rep(medium[4],10),rep(medium[5],10),rep(medium[6],10))
      
      # Green zone
      x0 <- lower
      x1 <- rep(0,length(L)) 
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=0, green=1, blue=0, alpha=0.3))
      

      # Red zone
      x0 <- rep(lim,length(L)) 
      x1 <- rev(upperr)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=1, green=0, blue=0, alpha=0.3))
      
     }
    
    
    
    
    if (input$risk1terra==2)  {
      
      
      L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
             seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
             seq(0.55,0.70,len=10))
      
      lower <- c(rep(low[1],10),rep(low[2],10),rep(low[3],10),
                 rep(low[4],10),rep(low[5],10),rep(low[6],10))
      
      upperr <- c(rep(upper [1],10),rep(upper[2],10),rep(upper[3],10),
                  rep(upper[4],10),rep(upper[5],10),rep(upper[6],10))
      
      mediumm <- c(rep(medium[1],10),rep(medium[2],10),rep(medium[3],10),
                   rep(medium[4],10),rep(medium[5],10),rep(medium[6],10))
      
      
      
    # Green zone
    x0 <- lower
    x1 <- rep(0,length(L)) 
    y0 <- L
    y1 <- rev(L)
    polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
            col=rgb(red=0, green=1, blue=0, alpha=0.3))
    
    # Yellow zone
    x0 <- upperr
    x1 <- rev(lower)
    y0 <- L
    y1 <- rev(L)
    polygon(x=c(x0,x1), y = c(y0,y1),density = NA, border = "lightgoldenrod1",
            col=rgb(red=1, green=1, blue=0, alpha=0.3))
    
    # Red zone
    x0 <- rep(lim,length(L)) 
    x1 <- rev(upperr)
    y0 <- L
    y1 <- rev(L)
    polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
            col=rgb(red=1, green=0, blue=0, alpha=0.3))
    
  }
    
  

    
    if (input$risk1terra==3) {
      
      
      L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
             seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
             seq(0.55,0.70,len=10))
      
      lower <- c(rep(low[1],10),rep(low[2],10),rep(low[3],10),
                 rep(low[4],10),rep(low[5],10),rep(low[6],10))
      
      upperr <- c(rep(upper [1],10),rep(upper[2],10),rep(upper[3],10),
                  rep(upper[4],10),rep(upper[5],10),rep(upper[6],10))
      
      mediumm <- c(rep(medium[1],10),rep(medium[2],10),rep(medium[3],10),
                   rep(medium[4],10),rep(medium[5],10),rep(medium[6],10))
      
      
      
      # Green zone
      x0 <- lower
      x1 <- rep(0,length(L)) 
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=0, green=1, blue=0, alpha=0.3))
      
      # Yellow zone
      x0 <- upperr
      x1 <- rev(lower)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, border = "lightgoldenrod1",
              col=rgb(red=1, green=1, blue=0, alpha=0.3))
      
      # Orange zone
      x0 <- mediumm
      x1 <- rev(upperr)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, border = "gold",
              col=rgb(red=1, green=1, blue=0, alpha=0.3))
      
      
      # Red zone
      x0 <- rep(lim,length(L)) 
      x1 <- rev(upperr)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=1, green=0, blue=0, alpha=0.3))
      

      
    }
    
    
    
    
    
    
    if (input$risk1terra==4)  {
      
      L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
             seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
             seq(0.55,0.70,len=10))
      
      lower <- c(rep(low[1],10),rep(low[2],10),rep(low[3],10),
                 rep(low[4],10),rep(low[5],10),rep(low[6],10))
      
      upperr <- c(rep(upper [1],10),rep(upper[2],10),rep(upper[3],10),
                  rep(upper[4],10),rep(upper[5],10),rep(upper[6],10))
      
      mediumm <- c(rep(medium[1],10),rep(medium[2],10),rep(medium[3],10),
                   rep(medium[4],10),rep(medium[5],10),rep(medium[6],10))
      
      
      
      # Green zone
      x0 <- lower
      x1 <- rep(0,length(L)) 
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=0, green=1, blue=0, alpha=0.3))
      
      # Yellow zone
      x0 <- upperr
      x1 <- rev(lower)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, border = "lightgoldenrod1",
              col=rgb(red=1, green=1, blue=0, alpha=0.3))
      
      # Red zone
      x0 <- rep(lim,length(L)) 
      x1 <- rev(upperr)
      y0 <- L
      y1 <- rev(L)
      polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
              col=rgb(red=1, green=0, blue=0, alpha=0.3))
      
    }
    
    
    
    
    if (input$showPC==TRUE){
      
    L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
             seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
             seq(0.55,0.70,len=10))
      
    PC <- c(input$aP1,input$aP2,input$aP3,input$aP4,
            input$aP5,input$aP6)

    y <- c(rep(PC[1],10),rep(PC[2],10),rep(PC[3],10),
           rep(PC[4],10),rep(PC[5],10),rep(PC[6],10))
    
    points(x=y, y=L, type="l")
    axis(1)
    mtext("Precompression stress (PC) (kPa)",side=1,line=2.5)
      
    }
    

  })
  
  
  
  
  
  
  output$plotrisk12 <- renderPlot({
    
    layers <- c(0.05,0.15,0.3,0.4,0.5,0.6)
    
    stress <-stressTraffic(inflation.pressure=input$risk1.IP, 
                           recommended.pressure=input$risk1.RIP, 
                           tyre.diameter=input$risk1.TD, 
                           tyre.width=input$risk1.TW, 
                           wheel.load=input$risk1.WL, 
                           conc.factor=rep(input$risk1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    par(mar=c(7,4,5,1))
    image.plot(x = as.numeric(rownames(stress$stress.matrix)),
               y = as.numeric(colnames(stress$stress.matrix)), 
               z = stress$stress.matrix,  legend.mar=4.5,
               xlab="Tyre footprint length (m)", ylab="Tyre width (m)",
               main="Contact stress (kPa)") 
    mtext("(<< Driving direction)",1,line=3.8, cex=0.9)
    
  })
  
  
  
  
  output$outrisk <- renderTable({
    
    layers <- c(0.10,0.20,0.30,0.40,0.50,0.60)
    stress <-stressTraffic(inflation.pressure=input$risk1.IP, 
                           recommended.pressure=input$risk1.RIP, 
                           tyre.diameter=input$risk1.TD, 
                           tyre.width=input$risk1.TW, 
                           wheel.load=input$risk1.WL, 
                           conc.factor=rep(input$risk1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    
    stressz <- stress$Stress$Z
    layers <- stress$Stress$Layers
    PC <- c(input$aP1,input$aP2,input$aP3,input$aP4,input$aP5,input$aP6)
    
    m <- matrix(nrow=6, ncol=4)
    m[,1] <- layers
    m[,2] <- PC
    m[,3] <- stressz
    m[,4] <- stress$Stress$p
    colnames(m) <- c("Layers (m)","PC (kPa)", "Vertical stress (kPa)", "p (kPa)")
    m
    
  })
  
  
  
  
  
  
  output$plotPRED1 <- renderPlot({
  
  
    layers <- c(0.05,0.10,0.20,0.3,0.4,0.5,0.6)
    
    stress <-stressTraffic(inflation.pressure=input$PRED1.IP, 
                           recommended.pressure=input$PRED1.RIP, 
                           tyre.diameter=input$PRED1.TD, 
                           tyre.width=input$PRED1.TW, 
                           wheel.load=input$PRED1.WL, 
                           conc.factor=rep(input$PRED1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    
  N <- c(input$N1,input$N2,input$N3,input$N4,input$N5,input$N6)
  lambda <- c(input$Lambda1,input$Lambda2,input$Lambda3,
              input$Lambda4,input$Lambda5,input$Lambda6)
  k <- c(input$kappa1,input$kappa2,input$kappa3,
         input$kappa4,input$kappa5,input$kappa6)
  PD <- c(input$PD1,input$PD2,input$PD3,input$PD4,input$PD5,input$PD6)
  BD <- c(input$BD1,input$BD2,input$BD3,input$BD4,input$BD5,input$BD6)
  
  def <- soilDeformation(stress = stress$Stress$p[-1],
                         p.density = PD,
                         iBD = BD, 
                         N = N, 
                         CI = lambda, 
                         k = k, 
                         k2 = (lambda*k)^0.5,
                         m = rep(1.3, length(layers[-1])))
  
  

  L <- c(seq(0,0.15,len=10),seq(0.15,0.25,len=10),seq(0.25,0.35,len=10),
         seq(0.35,0.45,len=10),seq(0.45,0.55,len=10),
         seq(0.55,0.70,len=10))
  
  iBD <- c(rep(def$iBD[1],10),rep(def$iBD[2],10),rep(def$iBD[3],10),
           rep(def$iBD[4],10),rep(def$iBD[5],10),rep(def$iBD[6],10))
  
  fBD <- c(rep(def$fBD[1],10),rep(def$fBD[2],10),rep(def$fBD[3],10),
           rep(def$fBD[4],10),rep(def$fBD[5],10),rep(def$fBD[6],10))

           
  
  par(mar=c(4.5,4,4,1.5))
  plot(x = 1, y = 1, 
       xlim=c(1,2),ylim=c(0.7,0),xaxt = "n",
       ylab = "",xlab ="", type="l", main="")
  axis(3)
  mtext(expression("Bulk density"~(Mg~m^-3)),3,line=2)
  mtext("Depth (m)",2,line=2.4)
  
  if (input$checkbox.PRED==TRUE) {
  x0 <- iBD
  y0 <- L
  x1 <- rev(fBD)
  y1 <- rev(L)
  polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
          col=rgb(red=1, green=0, blue=0, alpha=0.2))
  }
  
  points(x=iBD,y=L, type="l")
  points(x=def$iBD,y=layers[-1], pch=15)
  points(x=fBD,y=L, type="l", col=2)
  points(x=def$fBD,y=layers[-1], pch=15, col=2)
  
  legend(input$label.PRED, legend=c("Initial","Final"), col=c(1,2), pch=15, lty=1, cex=0.9)
  
  
  })
  
  
  
  
  
  
  
  output$plotPRED2 <- renderPlot({
    
    layers <- c(0.05,0.15,0.3,0.4,0.5,0.6)
    
    stress <-stressTraffic(inflation.pressure=input$PRED1.IP, 
                           recommended.pressure=input$PRED1.RIP, 
                           tyre.diameter=input$PRED1.TD, 
                           tyre.width=input$PRED1.TW, 
                           wheel.load=input$PRED1.WL, 
                           conc.factor=rep(input$PRED1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    par(mar=c(7,4,5,1))
    image.plot(x = as.numeric(rownames(stress$stress.matrix)),
               y = as.numeric(colnames(stress$stress.matrix)), 
               z = stress$stress.matrix,  legend.mar=4.5,
               xlab="Tyre footprint length (m)", ylab="Tyre width (m)",
               main="Contact stress (kPa)") 
    mtext("(<< Driving direction)",1,line=3.8, cex=0.9)
    
  })
  
  
  
  output$outPRED1 <- renderTable({
    
    layers <- c(0.10,0.20,0.3,0.4,0.5,0.6)
    
    stress <-stressTraffic(inflation.pressure=input$PRED1.IP, 
                           recommended.pressure=input$PRED1.RIP, 
                           tyre.diameter=input$PRED1.TD, 
                           tyre.width=input$PRED1.TW, 
                           wheel.load=input$PRED1.WL, 
                           conc.factor=rep(input$PRED1.CF,length(layers)),
                           layers=layers, 
                           plot.contact.area = FALSE)
    
    
    N <- c(input$N1,input$N2,input$N3,input$N4,input$N5,input$N6)
    lambda <- c(input$Lambda1,input$Lambda2,input$Lambda3,
                input$Lambda4,input$Lambda5,input$Lambda6)
    k <- c(input$kappa1,input$kappa2,input$kappa3,
           input$kappa4,input$kappa5,input$kappa6)
    PD <- c(input$PD1,input$PD2,input$PD3,input$PD4,input$PD5,input$PD6)
    BD <- c(input$BD1,input$BD2,input$BD3,input$BD4,input$BD5,input$BD6)
    
    
    def <- soilDeformation(stress = stress$Stress$p,
                           p.density = PD,
                           iBD = BD, 
                           N = N, 
                           CI = lambda, 
                           k = k, 
                           k2 = (lambda*k)^0.5,
                           m = rep(1.3, length(layers)))
    
    

    out <- data.frame(layers,"iBD"=def[,1],"fBD"=def[,2],
                      "iEV"=def[,3],"fEV"=def[,4],"Variation"=def[,5])
    colnames(out) <- c("Layer (m)","iBD", "fBD", "iEV", "fEV", "I%")
    out
    
  
  })
  
}










# UI ----------------

ui_PredComp = fluidPage(
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: LightSkyBlue;}',
             '.navbar-default .navbar-brand{color: black;}',
             '.tab-panel{ background-color: black; color: black}',
             '.nav navbar-nav li.active:hover a, .nav navbar-nav li.active a {
                        background-color: black ;
                        border-color: black;
                        }'
             
  ),
  
  navbarPage( "PredComp 1.0", 
              
  


    
  
# NAV 1
    
    tabPanel("Stress calculation",
             
             
             verticalLayout(
               column(12,wellPanel(
                 
                 helpText(tags$p("STRESS CALCULATION SECTION: in this section, the user is able to calculate the contact stress and 
                                        contact area, as well as the vertical and mean normal stress propagation. 
                                        For that, move the 'Machinery parameters' slider input. In this section, only contact stress 
                                        and stress propagation are simulated. For compaction effects, go to the 'Risk of compacton' 
                                        or 'Prediction of compaction' sections",
                                 
                                 style = "font-size: 100%;text-align:justify"))
                 
               ))),
             
             
             
             titlePanel(tags$p("Contact area, vertical and mean normal stress propagation", style = "font-size: 80%;")),
             
             column(3,wellPanel(
               h4("Machinery parameters"),
               
               
               sliderInput("inflation.pressure", "Tyre inflation pressure (TIP) (kPa)",
                           min = 50, max = 600,
                           step = 5, value=200, tick=FALSE),
               
               
               sliderInput("recommended.pressure", "Recommended TIP (kPa)",
                           min = 50, max = 600,
                           value = 200, step = 5,tick=FALSE),
               
               
               sliderInput("tyre.diameter", 'Tyre diameter (m)',
                           min = 0.5, max = 2.5,
                           value = 1, step = 0.05,tick=FALSE),
               
               sliderInput("tyre.width", 'Tyre width (m)',
                           min = 0.1, max = 1,
                           value = 0.60, step = 0.02,tick=FALSE),
               
               sliderInput("wheel.load", 'Wheel load (kg)',
                           min = 1000, max = 10000,
                           value = 4000, step = 10,tick=FALSE),
               
               
               sliderInput("conc.factor", 'Concentration factor',
                           min = 3, max = 6,
                           value = 3, step = 0.1,tick=FALSE),
               
               helpText(tags$p("Move the slider input to calculate the soil contact stress and stress propagation",
                               style = "font-size: 90%;text-align:center"))
               
             )),
             
             
             
             downloadButton("downloadData1", "Stress propagation",class = "btn-primary"),
             downloadButton("downloadData2", "Contact stress",class = "btn-primary"),
             
             column(6, wellPanel(
               h4("Stress distribution and propagation"),
               tabsetPanel(type = "tabs",
                           tabPanel("Contact stress", plotOutput("plotstresscal1")),
                           tabPanel("Contact area", tableOutput("outAREA")),
                           tabPanel("Vertical stress", plotOutput("plotstresscal2")),
                           tabPanel("Mean normal stress", plotOutput("plotstresscal3")))
               
             )         
             ),
             
             
             
             
             
             verticalLayout(
               column(12,wellPanel(
                 h4("Related links"),
                 
                 
                 actionButton(inputId='ab1', label="Richards et al. (1997)", 
                              icon = icon("th"), 
                              onclick ="window.open('http://www.international-agrophysics.org/Modelling-the-effects-of-repeated-wheel-loads-on-soil-profiles,107057,0,2.html')"),
                 
                 
                 actionButton(inputId='ab1', label="Defossez & Richard (2002)", 
                              icon = icon("th"), 
                              onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198702000302')"),
                 
                 
                 actionButton(inputId='ab1', label="Keller (2005)", 
                              icon = icon("th"), 
                              onclick ="window.open('https://www.sciencedirect.com/science/article/abs/pii/S1537511005001030')"),
                 
                 
                 actionButton(inputId='ab1', label="SoilFlex", 
                              icon = icon("th"), 
                              onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198706001413')"),
                 
                 
                 actionButton(inputId='ab1', label="Lima (2017)", 
                              icon = icon("th"), 
                              onclick ="window.open('https://teses.usp.br/teses/disponiveis/11/11140/tde-09082017-151718/pt-br.php')")
                 
                 
               )))
             
       
 ),


# NAV 2

navbarMenu("Precompression stress",
           
           tabPanel("Horn & Fleige (2003)",
                    
                    titlePanel(tags$p("Precompression stress estimation using pedo-transfer function (Horn & Fleige, 2003)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      selectInput("Horn2003type", "Choose soil texture/condition:", 
                                  choices=c("Sand at pF 1.8","Sandy Loam at pF 1.8",
                                            "Silt at pF 1.8","Clay < 35 at pF 1.8",
                                            "Clay > 35 at pF 1.8","Sand at pF 2.5",
                                            "Sandy Loam at pF 2.5","Silt at pF 2.5",
                                            "Clay < 35 at pF 2.5","Clay > 35 at pF 2.5")),
                      
                      
                      sliderInput("BD.Horn", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1.3, max = 2,
                                  step = 0.01, value=1.55, tick=FALSE),
                      
                     
                      sliderInput("AC.Horn",  tags$p("Volumetric air capacity (%) at the specified pF", style = "font-size: 90%;text-align:justify"),
                                  min = 2, max = 20,
                                  value = 10, step = 1,tick=FALSE),
                      

                      sliderInput("AWC.Horn",tags$p("Volumetric available water (%) at the specified pF", style = "font-size: 90%;text-align:justify"),
                                  min = 6, max = 30,
                                  value = 15, step = 0.1,tick=FALSE),
                      
                      sliderInput("PWP.Horn", tags$p("Volumetric non available water capacity (%) (pF > 4.2)", style = "font-size: 90%;text-align:justify"),
                                  min = 4, max = 26,
                                  step = 0.5, value=26, tick=FALSE)
                      
                      
                    )),
                    
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                    
                      
                      
                      sliderInput("Ks.Horn", HTML(paste0("Ks (10",tags$sup("3")," cm s",tags$sup("-1"),")")),
                                  min = 0.005, max = 5,
                                  value = 0.29, step = 0.001,tick=FALSE),
                      
                      
                      sliderInput("OM.Horn", "Organic matter (%)",
                                  min = 1, max = 5,
                                  value = 1.5, step = 0.1,tick=FALSE),
                      
                      sliderInput("C.Horn", tags$p("Cohesion (kPa) at the specified pF", style = "font-size: 90%;text-align:justify"),
                                  min = 1, max = 70,
                                  value = 30, step = 1,tick=FALSE),
                      
                      
                      sliderInput("angle.Horn", tags$p("Angle of internal friction at the specified pF", style = "font-size: 90%;text-align:justify"),
                                  min = 20, max = 56,
                                  value = 36, step = 1,tick=FALSE)
                      
                      
                    )),
                    
                    
            
                    column(5, wellPanel(
                      h4(tags$p("Precompression stress (PC)", style = "font-size: 80%;")),
                      tableOutput("Horn2003"),
                      helpText(tags$p("Classification: classification of PC level from Horn & Fleige (2003)", style = "font-size: 90%;"))
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="Lebert & Horn (1991)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/016719879190095F')"),
                        
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')"),
                        
                        
                        actionButton(inputId='ab1', label="Alcor model", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://evenor-tech.com/microleis/microlei/manual2/alcor/alcor.htm')")
                        
                        
                      )))
                    
                    
                    
           ),
           
           
           
           
  tabPanel("Imhoff et al. (2004)",
                    
                    titlePanel(tags$p("Precompression stress estimation using pedo-transfer function (Imhoff et al. 2004)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
     
                      sliderInput("BDImhoff", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1, max = 2,
                                  step = 0.01, value=1.5, tick=FALSE),
                      
                      sliderInput("clayImhoff", "Clay content (%)",
                                  min = 10, max = 70,
                                  step = 1, value=35, tick=FALSE),
                      
                      
                      sliderInput("wImhoff", 'Gravimetric water content (g/g)',
                                  min = 0.1, max = 0.4,
                                  value = 0.15, step = 0.01,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(5, wellPanel(
                      h4(tags$p("Precompression stress (PC)", style = "font-size: 80%;")),
                      tableOutput("Imhoff2004"),
                      helpText(tags$p("Classification: classification of PC level from Horn & Fleige (2003)", style = "font-size: 90%;"))
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="Imhoff et al. (2004)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://acsess.onlinelibrary.wiley.com/doi/abs/10.2136/sssaj2004.1700')"),
                        
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')")
                        
                        
                      )))
                    
                    
                    
           ),
           
           
           
           
           
           tabPanel("Saffih-Hdadi et al. (2009)",
                    
                    titlePanel(tags$p("Precompression stress estimation using pedo-transfer function (Saffih-Hdadi et al. 2009)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      
                      selectInput("soilSaffih", "Choose the soil texture:", 
                                  choices=c("VeryFine",
                                            "Fine",
                                            "MediumFine",
                                            "Medium",
                                            "Coarse")),
                      
                      
            
                      
                      sliderInput("BDSaffih", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1, max = 2,
                                  step = 0.01, value=1.4, tick=FALSE),
                      
                      
                      sliderInput("wSaffih", 'Gravimentric water content (%)',
                                  min = 5, max = 40,
                                  value = 25, step = 0.1,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(5, wellPanel(
                      h4(tags$p("Precompression stress (PC)", style = "font-size: 80%;")),
                      tableOutput("Saffih2009"),
                      helpText(tags$p("Classification: classification of PC level from Horn & Fleige (2003)", style = "font-size: 90%;"))
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="Saffih-Hdadi et al. (2009)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198709001287')"),
                        
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')")
                        
                      )))
                    
                    
                    
           ),
           
           
           
           
           
           tabPanel("Severiano et al. (2013)",
                    
                    titlePanel(tags$p("Precompression stress estimation using pedo-transfer function (Severiano et al. 2013)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      
                      sliderInput("claySeveriano", "Clay content (%)",
                                  min = 10, max = 70,
                                  step = 1, value=30, tick=FALSE),
                      
                      
                      sliderInput("matricSeveriano", 'Matric suction (hPa)',
                                  min = 10, max = 800,
                                  value = 100, step = 1,tick=FALSE),
                      helpText(tags$p("The matric suction input in Severiano et al. (2013) is in kPa. Here, the input is given in hPa, 
                                       but converted to kPa in the function", style = "font-size: 70%;text-align:justify"))
                      
                      
                    )),
                    
                    
                    
                    column(5, wellPanel(
                      h4(tags$p("Precompression stress (PC)", style = "font-size: 80%;")),
                      tableOutput("severiano2013"),
                      helpText(tags$p("Classification: classification of PC level from Horn & Fleige (2003)", style = "font-size: 90%;"))
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="Severiano et al. (2013)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.publish.csiro.au/SR/SR12366')"),
                        
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')")
                        
                        
                      )))
                    
                    
           ),
           
           
           
           tabPanel("Schjonning & Lamande (2018)",
                    
                    titlePanel(tags$p("Precompression stress estimation using pedo-transfer function (Schjonning & Lamande, 2018)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      
                      sliderInput("clayLamande", "Clay content (%)",
                                  min = 10, max = 70,
                                  step = 1, value=30, tick=FALSE),
                      
                      sliderInput("BDLamande", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1.2, max = 2,
                                  step = 0.01, value=1.4, tick=FALSE),
    
                      
                      sliderInput("matricLamande", 'Matric suction (hPa)',
                                  min = 1, max = 800,
                                  value = 100, step = 1,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(5, wellPanel(
                      h4(tags$p("Precompression stress (PC)", style = "font-size: 80%;")),
                      tableOutput("lamande2018"),
                      helpText(tags$p("Classification: classification of PC level from Horn & Fleige (2003)", style = "font-size: 90%;"))
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="Schjonning & Lamande (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.publish.csiro.au/SR/SR12366')"),
                        
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')")
                        
                        
                      )))
                    
                    
                    
           )
),
           







# NAV 3


navbarMenu("Compressive properties",
           
           
           tabPanel("O'Sullivan et al. (1999)",
                    
                    titlePanel(tags$p("Compressive properties estimation using pedo-transfer function (O'Sullivan et al. 1999)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      selectInput("Sullivantype", "Choose the soil texture:", 
                                  choices=c("SandyLoam","ClayLoam")),
                      
                      
                      sliderInput("w.Sullivan", "Gravimentric water content (%)",
                                  min = 8, max = 23,
                                  step = 1, value=17, tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(6, wellPanel(
                      h4(tags$p("Compressive properties from the soil compression curve", style = "font-size: 80%;")),
                      tableOutput("Sullivan1999"),
                      tags$p("See de Lima et al. (2018) for model architecture", style = "font-size: 90%;")
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),
                        
                        
                        actionButton(inputId='ab1', label="Defossez et al. (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S001670610300096X')"),
                        
                        
                        actionButton(inputId='ab1', label="Keller & Arvidsson (2007)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2007.00944.x')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198717301708')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2020)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/sum.12601')")
                        
                        
                      ))),
                    
                    
           ),
           
           
           
           
           tabPanel("Defossez et al. (2003)",
                    
                    titlePanel(tags$p("Compressive properties estimation using pedo-transfer function (Defossez et al. 2003)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      selectInput("Defossez2003type", "Choose soil type:", 
                                  choices=c("Loess","Calcareous")),
                      
                      
                      sliderInput("w.Defossez", "Gravimentric water content (%)",
                                  min = 5, max = 30,
                                  step = 1, value=20, tick=FALSE)
                      
            
                    )),
                    
                    
                    
                    column(6, wellPanel(
                      h4(tags$p("Compressive properties from the soil compression curve", style = "font-size: 80%;")),
                      tableOutput("Defossez2003"),
                      tags$p("See de Lima et al. (2018) for model architecture", style = "font-size: 90%;")
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),
                        
                        
                        actionButton(inputId='ab1', label="Defossez et al. (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S001670610300096X')"),
                        
                        
                        actionButton(inputId='ab1', label="Keller & Arvidsson (2007)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2007.00944.x')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198717301708')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2020)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/sum.12601')")
                        
                        
                      )))
                    
    
           ),
           
           
           
           
           tabPanel("Keller & Arvidsson (2007)",
                    
                    titlePanel(tags$p("Compressive properties estimation using pedo-transfer function (Keller & Arvidsson, 2007)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),

                      sliderInput("BDKeller", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1, max = 2,
                                  value = 1.5, step = 0.01,tick=FALSE),                    
    
                      
                      sliderInput("PDKeller", HTML(paste0("Particle density (Mg m",tags$sup("-3"),")")),
                                  min = 2.4, max = 2.8,
                                  value = 2.65, step = 0.01,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(6, wellPanel(
                      h4(tags$p("Compressive properties from the soil compression curve", style = "font-size: 80%;")),
                      tableOutput("Keller2007"),
                      tags$p("See de Lima et al. (2018) for model architecture", style = "font-size: 90%;")
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),
                        
                        
                        actionButton(inputId='ab1', label="Defossez et al. (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S001670610300096X')"),
                        
                        
                        actionButton(inputId='ab1', label="Keller & Arvidsson (2007)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2007.00944.x')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198717301708')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2020)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/sum.12601')")
                        
                        
                      )))
                    
                    
                    
           ),
           
           
           
           
           
           tabPanel("de Lima et al. (2018)",
                    
                    titlePanel(tags$p("Compressive properties estimation using pedo-transfer function (de Lima et al. 2018)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      selectInput("Lima2018type", "Choose soil texture:", 
                                  choices=c("SandyLoam","SandyClayLoam")),
                      
                      
                      sliderInput("BDLima2018", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                                  min = 1.3, max = 1.7,
                                  step = 0.01, value=1.4, tick=FALSE),
                      
                      
                      sliderInput("matricLima2018", 'Matric suction (hPa)',
                                  min = 30, max = 800,
                                  value = 100, step = 1,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(6, wellPanel(
                      h4(tags$p("Compressive properties from the soil compression curve", style = "font-size: 80%;")),
                      tableOutput("Lima2018"),
                      tags$p("See de Lima et al. (2018) for model architecture", style = "font-size: 90%;")
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),
                        
                        
                        actionButton(inputId='ab1', label="Defossez et al. (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S001670610300096X')"),
                        
                        
                        actionButton(inputId='ab1', label="Keller & Arvidsson (2007)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2007.00944.x')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198717301708')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2020)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/sum.12601')")
                        
                        
                      ))),
                    
                    
           ),
           
           
           
           tabPanel("de Lima et al. (2020)",
                    
                    titlePanel(tags$p("Compressive properties estimation using pedo-transfer function (de Lima et al. 2020)", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Soil inputs"),
                      
                      selectInput("Lima2020type", "Choose soil the condition:", 
                                  choices=c("PloughLayer","PloughPan")),
                      

                      sliderInput("matricLima2020", 'Matric suction (hPa)',
                                  min = 30, max = 800,
                                  value = 100, step = 1,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(6, wellPanel(
                      h4(tags$p("Compressive properties from the soil compression curve", style = "font-size: 80%;")),
                      tableOutput("Lima2020"),
                      tags$p("See de Lima et al. (2018) for model architecture", style = "font-size: 90%;")
                      
                    )         
                    ),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),
                        
                        
                        actionButton(inputId='ab1', label="Defossez et al. (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S001670610300096X')"),
                        
                        
                        actionButton(inputId='ab1', label="Keller & Arvidsson (2007)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2007.00944.x')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2018)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198717301708')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="de Lima et al. (2020)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/sum.12601')")
                        
                        
                      )))
                    
                    
                    
           )
),














# NAV 4 - RISK OF COMPACTION 


           tabPanel("Risk of compaction",
                    
                    verticalLayout(
                      column(12,wellPanel(
                        
                        helpText(tags$p("RISK OF COMPACTION SECTION: in this section, the user is able to perform a compaction risk assessment. 
                                        For that, 1) move the 'Machinery parameters' slider input to calculate the contact stress and stress 
                                        propagation, 2) move the 'Precompression stress' slider input to calculate the soil strength, and 3) choose the compaction risk criterion. Examine the 
                                        outputs through the range of colors risk. In the 'Precompression stress' 
                                        section above, it is possible to estimate the precompression stress (in terms of vertical stress) from readily available soil properties using pedo-transfer functions",
       
                                style = "font-size: 100%;text-align:justify"))
                        
                      ))),
                    
                    
                    
                    titlePanel(tags$p("Assessment of the risk of soil compaction", style = "font-size: 80%;")),
                    
                    column(3,wellPanel(
                      h4("Machinery parameters"),
                      
                      
                      sliderInput("risk1.IP", "Tyre inflation pressure (TIP) (kPa)",
                                  min = 50, max = 600,
                                  step = 5, value=200, tick=FALSE),
                      
                      
                      sliderInput("risk1.RIP", "Recommended TIP (kPa)",
                                  min = 50, max = 600,
                                  value = 200, step = 5,tick=FALSE),
                      
                      
                      sliderInput("risk1.TD", 'Tyre diameter (m)',
                                  min = 0.5, max = 2.5,
                                  value = 1, step = 0.05,tick=FALSE),
                      
                      sliderInput("risk1.TW", 'Tyre width (m)',
                                  min = 0.1, max = 1,
                                  value = 0.60, step = 0.02,tick=FALSE),
                      
                      sliderInput("risk1.WL", 'Wheel load (kg)',
                                  min = 1000, max = 10000,
                                  value = 4000, step = 10,tick=FALSE),
                      
                      
                      sliderInput("risk1.CF", 'Concentration factor',
                                  min = 3, max = 6,
                                  value = 3, step = 0.1,tick=FALSE),
                      
                      helpText(tags$p("Move the slider input to calculate the soil contact stress and stress propagation",
                                      style = "font-size: 90%;text-align:center"))
                      
                    )),
                    
                    

                    column(3,wellPanel(
                      h4("Precompression stress (kPa)"),
                      
                      
                      sliderInput("aP1", tags$p("Depth 0.10 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1, value=100, tick=FALSE),
                      
                      
                      sliderInput("aP2", tags$p("Depth 0.20 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1, value=100, tick=FALSE),
                      
                      
                      sliderInput("aP3", tags$p("Depth 0.30 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1, value=100, tick=FALSE),
                      
                      sliderInput("aP4", tags$p("Depth 0.40 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1, value=100, tick=FALSE),
                      
                      sliderInput("aP5", tags$p("Depth 0.50 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1, value=100, tick=FALSE),
                      
                      
                      sliderInput("aP6", tags$p("Depth 0.60 m", style = "font-size: 85%;"),
                                  min = 30, max = 400,
                                  step = 1,value = 100,tick=FALSE),
                      
                      helpText(tags$p("Set the precompression stress for each depth. Optionally, 
                                            access the 'PRECOMPRESSION STRESS' 
                                            section and estimate it through readily available
                                            soil properties", style = "font-size: 80%;text-align:justify"))
                      
                    )),
                    
                    

                    
                    
                    column(2,wellPanel(
                      h4(""),
                      
                      
                      radioButtons("risk1terra", tags$p("Criteria of risk", style = "font-size: 90%;"),
                                   choiceNames=c("PC only","Terranimo","Horn&Fleige","User define"),
                                   choiceValues=c(1,2,3,4),
                                   width='70%'),
                      
                      
                      sliderInput("risk1.userdefine",  tags$p("To define", style = "font-size: 90%;"),
                                  min = 50, max = 150,
                                  value = c(80,110), step = 1,tick=FALSE),
                      tags$p("If the option 'User define' is chosen, the slider input above should be 
                              used to define the lower and upper ranges of risk based on the percentage 
                              of the precompression stress (PC)", style = "font-size: 75%;text-align:justify"),
                      tags$p("___________________________", style = "font-size: 70%;"),
                      tags$p("Risk of compaction:", style = "font-size: 90%;"),
                      tags$p("Green: Low risk", style = "font-size: 75%;"),
                      tags$p("Yellow: Moderate risk ", style = "font-size: 75%;"),
                      tags$p("Dark yellow: Considerable risk ", style = "font-size: 75%;"),
                      tags$p("Red: High risk", style = "font-size: 75%;")
                      
                    )),
                    
                    

                    
                    column(4, wellPanel(
                      h4("Soil stress and strength"),
                      tabsetPanel(type = "tabs",
                                  tabPanel("Vertical stress", plotOutput("plotrisk11")),
                                  tabPanel("Contact stress", plotOutput("plotrisk12")),
                                  tabPanel("Data", tableOutput("outrisk"))),
                      checkboxInput("showPC", "PC profile", value = FALSE),
                      checkboxInput("meanRISK", "Mean normal stress", value = FALSE)
                      
                    )         
                    ),
                    

                    
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Related links"),
                        
                        
                        actionButton(inputId='ab1', label="Keller (2005)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/abs/pii/S1537511005001030')"),
                        
                        
                        actionButton(inputId='ab1', label="SoilFlex", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198706001413')"),
                        

                        actionButton(inputId='ab1', label="Terranimo/paper", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.landtechnik-online.eu/landtechnik/article/view/2014-69-3-132-138')"),
                        
                        actionButton(inputId='ab1', label="Horn & Fleige (2003)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198703001028')"),
                        
                        
                        
                        actionButton(inputId='ab1', label="Lima (2017)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://teses.usp.br/teses/disponiveis/11/11140/tde-09082017-151718/pt-br.php')")
                        
                        
                      )))
                    
                    
           ),
           









# NAV 5 - PREDICION OF COMPACTION 



tabPanel("Prediction of compaction",
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("PREDICTION OF COMPACTION SECTION: in this section, the user is able to perform a simulation to predict the 
                                        occurrence of compaction and its corresponding effect on soil bulk density. 
                                        For that, 1) move the 'Machinery parameters' slider input to calculate the contact stress and stress 
                                        propagation and 2) set the 
                                        compressive parameters and initial soil bulk density. Examine the 
                                        outputs through the changes on the final bulk density after the applied stress. In the 'Compressive properties' 
                                        section above, it is possible to estimate these compressive parameters from readily available soil properties using pedo-transfer function",
                             
                             style = "font-size: 97%;text-align:justify"))
             
           ))),
         
         
         
         
         titlePanel(tags$p("Prediction of soil compaction", style = "font-size: 80%;")),
         
         column(3,wellPanel(
           h4("Machinery parameters"),
           
           
           sliderInput("PRED1.IP", "Tyre inflation pressure (TIP) (kPa)",
                       min = 50, max = 600,
                       step = 5, value=200, tick=FALSE),
           
           
           sliderInput("PRED1.RIP", "Recommended TIP (kPa)",
                       min = 50, max = 600,
                       value = 200, step = 5,tick=FALSE),
           
           
           sliderInput("PRED1.TD", 'Tyre diameter (m)',
                       min = 0.5, max = 2.5,
                       value = 1, step = 0.05,tick=FALSE),
           
           sliderInput("PRED1.TW", 'Tyre width (m)',
                       min = 0.1, max = 1,
                       value = 0.60, step = 0.02,tick=FALSE),
           
           sliderInput("PRED1.WL", 'Wheel load (kg)',
                       min = 1000, max = 10000,
                       value = 4000, step = 10,tick=FALSE),
           
           
           sliderInput("PRED1.CF", 'Concentration factor',
                       min = 3, max = 6,
                       value = 3, step = 0.1,tick=FALSE),
           
           helpText(tags$p("Move the slider input to calculate the soil contact stress and stress propagation",
                           style = "font-size: 90%;text-align:center"))
           
           
           
         )),
         
         
         
         column(width = 5, style='padding:0px;',
                         wellPanel( 
                           h4("Compressive properties"),
                             fluidRow(
                             
                             # Labels
                               column(2,style='padding:0px;',
                               helpText(tags$p("Depth (m)", style = "font-size: 90%;"))),
                               column(2,style='padding:0px;',
                                      helpText(tags$p("N", style = "font-size: 90%;"))),
                               column(2,style='padding:0px;',
                                      helpText(tags$p("Lambda", style = "font-size: 90%;"))),
                               column(2,style='padding:0px;',
                                      helpText(tags$p("Kappa", style = "font-size: 90%;"))),
                               column(2,style='padding:0px;',
                                      helpText(tags$p(HTML(paste0("PD (Mg m",tags$sup("-3"),")")), style = "font-size: 90%;"))),
                               column(2,style='padding:0px;',
                                      helpText(tags$p(HTML(paste0("iBD (Mg m",tags$sup("-3"),")")), style = "font-size: 90%;"))),
                               
                               
                            
                            
                             # Depth 0.05
                               
                             column(2,"'",
                                    actionButton("a1","0.10",width='100%',class = "btn-primary")),
                             
                                    
                                    
                               
                             column(2,style='padding:0px;',
                                    numericInput("N1", tags$p("", style = "font-size: 85%;"),
                                                 min = 1.6, max = 2.6,
                                                 step = 0.01, value=2.2,width='150%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("Lambda1", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.05, max = 0.3,
                                                 step = 0.01, value=0.12,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("kappa1", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.001, max = 0.05,
                                                 step = 0.001, value=0.001,width='200%')),
                             column(2,style='padding:0px;',
                                    numericInput("PD1", tags$p("", style = "font-size: 85%;"),
                                                 min = 2.6, max = 2.8,
                                                 step = 0.01, value=2.65,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD1", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52,width='200%')),
                             
                             
                             
                             # Depth 0.15 
                            
                             column(2,"'",
                                    actionButton("a2","0.20",width='100%',class = "btn-primary")),
                             
                             column(2,style='padding:0px;',
                                    numericInput("N2", tags$p("", style = "font-size: 85%;"),
                                                 min = 1.6, max = 2.6,
                                                 step = 0.01, value=2.2,width='150%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("Lambda2", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.05, max = 0.3,
                                                 step = 0.01, value=0.12,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("kappa2", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.001, max = 0.05,
                                                 step = 0.001, value=0.001,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("PD2", tags$p("", style = "font-size: 85%;"),
                                                 min = 2.6, max = 2.8,
                                                 step = 0.01, value=2.65,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD2", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52,width='200%')),
                             
                             
                             
                             # Depth 0.30
                            
                            
                             column(2,"'",
                                    actionButton("a3","0.30",width='100%',class = "btn-primary")),
                             
                             column(2,style='padding:0px;',
                                    numericInput("N3", tags$p("", style = "font-size: 85%;"),
                                                 min = 1.6, max = 2.6,
                                                 step = 0.01, value=2.2,width='150%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("Lambda3", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.05, max = 0.3,
                                                 step = 0.01, value=0.12,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("kappa3", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.001, max = 0.05,
                                                 step = 0.001, value=0.001,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("PD3", tags$p("", style = "font-size: 85%;"),
                                                 min = 2.6, max = 2.8,
                                                 step = 0.01, value=2.65,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD3", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52,width='200%')),
                             
                             
                             # Depth 0.40 
                            
                            
                             column(2,"'",
                                    actionButton("a4","0.40",width='100%',class = "btn-primary")),
                             
                             column(2,style='padding:0px;',
                                    numericInput("N4", tags$p("", style = "font-size: 85%;"),
                                                 min = 1.6, max = 2.6,
                                                 step = 0.01, value=2.2,width='150%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("Lambda4", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.05, max = 0.3,
                                                 step = 0.01, value=0.12,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("kappa4", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.001, max = 0.05,
                                                 step = 0.001, value=0.001,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("PD4", tags$p("", style = "font-size: 85%;"),
                                                 min = 2.6, max = 2.8,
                                                 step = 0.01, value=2.65,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD4", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52,width='200%')),
                             
                             
                             # Depth 0.50
                            
                            
                             column(2,"'",
                                    actionButton("a5","0.50",width='100%',class = "btn-primary")),
                             
                             column(2,style='padding:0px;',
                                    numericInput("N5", tags$p("", style = "font-size: 85%;"),
                                                 min = 1.6, max = 2.6,
                                                 step = 0.01, value=2.2,width='150%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("Lambda5", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.05, max = 0.3,
                                                 step = 0.01, value=0.12,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("kappa5", tags$p("", style = "font-size: 85%;"),
                                                 min = 0.001, max = 0.05,
                                                 step = 0.001, value=0.001,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("PD5", tags$p("", style = "font-size: 85%;"),
                                                 min = 2.6, max = 2.8,
                                                 step = 0.01, value=2.65,width='200%')),
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD5", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52,width='200%')),
                             
                             
                             # Depth 0.60
                            
                            
                             column(2,"'",
                                    actionButton("a6","0.60",width='100%',class = "btn-primary")),
                            
                            
                            column(2,style='padding:0px;',
                                   numericInput("N6", tags$p("", style = "font-size: 85%;"),
                                                min = 1.6, max = 2.6,
                                                step = 0.01, value=2.2,width='150%')),
                            
                            column(2,style='padding:0px;',
                                   numericInput("Lambda6", tags$p("", style = "font-size: 85%;"),
                                                min = 0.05, max = 0.3,
                                                step = 0.01, value=0.12,width='200%')),
                            
                            column(2,style='padding:0px;',
                                   numericInput("kappa6", tags$p("", style = "font-size: 85%;"),
                                                min = 0.001, max = 0.05,
                                                step = 0.001, value=0.001,width='200%')),
                            
                            column(2,style='padding:0px;',
                                   numericInput("PD6", tags$p("", style = "font-size: 85%;"),
                                                min = 2.6, max = 2.8,
                                                step = 0.01, value=2.65,width='200%')),
                             
                             
                             column(2,style='padding:0px;',
                                    numericInput("BD6", tags$p("", style = "font-size: 85%;"),
                                                 min = 1, max = 1.8,
                                                 step = 0.01, value=1.52)),
                            
                            helpText(tags$p("Set the soil compressive parameters and bulk densities for each depth. Optionally, 
                                            access the 'COMPRESSIVE PROPERTIES' 
                                            section and estimate them through readily available
                                            soil properties", style = "font-size: 86%;text-align:justify")),
                            helpText(tags$p("DATA INPUT LEGEND: N: the specific volume at p = 1 kPa, where p is the mean normal stress; 
                                             Lambda: the compression index; Kappa: the recompression index; PD: particle density; 
                                             iBD: initial bulk density. DATA OUTPUT LEGEND: iBD: initial bulk density; fBD: final bulk density; 
                                             iEV: initial specific volume; fEV: final specific volume; I%: soil final volume variation.",
                                             style = "font-size: 70%;text-align:justify"))
                             
                             
                           ))),
         
         
   
         
         column(4, wellPanel(
           h4("Stress and changes in soil volume"),
           tabsetPanel(type = "tabs",
                       tabPanel("Vertical stress", plotOutput("plotPRED1")),
                       tabPanel("Contact stress", plotOutput("plotPRED2")),
                       tabPanel("Data", tableOutput("outPRED1"))),
           
           helpText(tags$p("", style = "font-size: 90%;text-align:justify")),
           
           selectInput("label.PRED", tags$p("Label position", style = "font-size: 100%;"), 
                       choices=c("topright",
                                 "topleft",
                                 "bottomleft",
                                 "bottomright")),
           
           checkboxInput("checkbox.PRED", "Shaded increase", value = FALSE)
           
                 
         )),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Related links"),
             
             
             actionButton(inputId='ab1', label="O'Sullivan et al. (1999)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198798001871')"),

             
             actionButton(inputId='ab1', label="Keller (2005)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/abs/pii/S1537511005001030')"),
             
             
             actionButton(inputId='ab1', label="SoilFlex", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0167198706001413')"),
             
             
             actionButton(inputId='ab1', label="Alcor model", 
                          icon = icon("th"), 
                          onclick ="window.open('https://evenor-tech.com/microleis/microlei/manual2/alcor/alcor.htm')"),
             
             
             actionButton(inputId='ab1', label="Lima (2017)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://teses.usp.br/teses/disponiveis/11/11140/tde-09082017-151718/pt-br.php')")
             
             
           )))
         
         
),




# NAV ABOUT
    
tabPanel("About", "",
         
         
         verticalLayout(
           column(12,wellPanel(
             tags$p("This R App is an interactive web interface for simulation
                     of soil compaction induced by agricultural field traffic and 
                     integrate the set of functions 
                     for soil physical data analysis of the R package 'soilphysics' ",
                     style = "font-size: 100%;text-align:justify"),
             
             
             
             actionButton(inputId='ab1', label="soilphysics", 
                          icon = icon("th"), 
                          onclick ="window.open('https://arsilva87.github.io/soilphysics/')")
             
           ))),
         
         
         
         
         
         verticalLayout(
           column(12,wellPanel(
             tags$p("Developed by Renato P. de Lima & Anderson R. da Silva", style = "font-size: 90%;")
             
           ))),
             
             
             verticalLayout(
               column(12,wellPanel(
                 tags$p("Suggestions and bug reports: renato_agro_@hotmail.com", style = "font-size: 90%;")
             
             
             
             
           )))
         
)
   
  )
)


PredComp <- function() {
  shinyApp(ui_PredComp, server_PredComp)
  }



