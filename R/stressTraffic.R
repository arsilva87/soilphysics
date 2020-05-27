stressTraffic <- 
function (inflation.pressure, recommended.pressure, tyre.diameter, 
	tyre.width, wheel.load, conc.factor, layers, plot.contact.area = FALSE, ...)
{ 
   # CONTACT AREA ---------------------------------------

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
     mstress <- round(outer(x, y, fstressXY, alpha = alpha.O),0)
     mstress[is.nan(mstress)] <- 0
     dimnames(mstress) <- list(round(x, 2), round(y, 2))
     if (plot.contact.area) {
         contour(x, y, z = mstress, method = "edge", xlab = "Tyre footprint length (m)", 
                 ylab = "Tyre width (m)",...)
         box()
     }

  # STRESS PROPAGATION ---------------------------------
    Pi <- mstress * (0.01 * 0.01)
    F.max <- sum(as.vector(Pi), na.rm = T)
    kg.max <- (F.max * 1000)/9.81
    Df <- ((wheel.load - F.max) * 1000)/9.81
    Pi <- t(Pi)

  # PRINCIPAL STRESS, Z...
    ZStress <- function(Layers,conc.factor) {
        Z.stress <- c()
        for (j in 1:length(Layers)) {
            r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
            R <- sqrt(as.vector(r)^2 + Layers[j]^2)
            Pi <- as.vector(Pi)
            coss <- (Layers[j]/R)
            stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
                        (coss^(conc.factor[j] - 2))
            stress.layers <- stress.R * coss^2
            Z.stress[j] <- round(sum(as.vector(stress.layers), na.rm = T), 0)
                                    }
        Z.stress
     }
 
  # X STRESS, X...

    XStress <- function(Layers,conc.factor) {
        X.stress <- c()
        for (j in 1:length(Layers)) {
            r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
            R <- sqrt(as.vector(r)^2 + Layers[j]^2)
            Pi <- as.vector(Pi)

            coss <- (Layers[j]/R)
            seno <- sqrt(1 - (coss ^ 2))
            coss2 <-r
            for (k in 1:length(x)){
                 coss2[,k] <- (abs(y)/coss2[,k])
                 }  

            stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
                        (coss^(conc.factor[j] - 2))
            stress.layers <- stress.R*(seno^2)*(as.vector(coss2)^2)
            X.stress[j] <- round(sum(as.vector(stress.layers), na.rm = T), 0)
                                    }
        X.stress
     }

  # Y STRESS, Y...

    YStress <- function(Layers,conc.factor) {
        Y.stress <- c()
        for (j in 1:length(Layers)) {
            r <- outer(y, x, function(y, x) sqrt(y^2 + x^2))
            R <- sqrt(as.vector(r)^2 + Layers[j]^2)
            Pi <- as.vector(Pi)

            coss <- (Layers[j]/R)
            seno <- sqrt(1 - (coss ^ 2))
            seno2 <-r
            for (i in 1:length(y)){
                 seno2[i,] <- (abs(x)/seno2[i,])
                 }

            stress.R <- ((Pi * conc.factor[j])/(2 * (pi) * R^2)) * 
                        (coss^(conc.factor[j] - 2))
            stress.layers <- stress.R*(seno^2)*(as.vector(seno2)^2)
            Y.stress[j] <- round(sum(as.vector(stress.layers), na.rm = T), 0)
                                    }
        Y.stress
    }


  # MEAN NORMAL STRESS, p
   
    pStress <- function(Layers,conc.factor) {
           
            Z<-ZStress(Layers,conc.factor)
            X<-XStress(Layers,conc.factor)
            Y<-YStress(Layers,conc.factor)
            MEAN <- round((Z+X+Y)/3,0)
            return(MEAN)
     }


  # OUTS -------------------------------------------------
    # Load 

      Indices <- c("Applied Wheel Load", "Modeled Wheel Load","Diference")
      Loads <- c((wheel.load * 1000)/9.81, round(kg.max, 0), round(Df,0))
      Loads <- data.frame(Indices, Loads)
      colnames(Loads) <- c("Parameters","Loads (kg)")

    # Area

      Parameters <- c("Max Stress", "Contact Area", "Area Length", "Area Width")
      Units <- c("kPa  ", "m^2  ", "m   ", "m   ")
      Value <- c(round(stressmax, 0), round(contact.area, 2), 
                 round(area.length, 2), tyre.width)
      Area <- data.frame(Parameters, Value, Units)
      Layers <- layers
 
    # Stress

      stress.X <- XStress(layers,conc.factor)
      stress.Y <- YStress(layers,conc.factor)
      stress.Z <- ZStress(layers,conc.factor)
      p <- round((stress.Z+stress.X+stress.Y)/3, 0)

      stress <- data.frame(Layers, stress.Z,p)
      colnames(stress) <- c("Layers (m)","sigma_vertical", "sigma_mean")

      out <- list(Area = Area, Loads = Loads, 
                  Stress = stress, stress.matrix = mstress,
                  fZStress = ZStress, fmeanStress=pStress, 
		fXStress=XStress,fYStress=YStress, conc.factor = conc.factor)
      class(out) <- "stressTraffic"
      return(out)
}

# ---------------------------------
# print method
print.stressTraffic <- function(x, ...)
{
   cat("\n ---------- Boundaries of Contact Area\n\n")
   print(x$Area, ...)
   cat("\n ---------- Wheel Loads\n\n")
   print(x$Loads, ...)
   cat("\n ---------- Stress Propagation\n\n")
   print(x$Stress)
   invisible(x)
}

    
