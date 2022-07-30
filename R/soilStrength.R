soilStrength <-  
function(clay.content, matric.suction=NULL, water.content=NULL)
{
   if (!is.null(matric.suction) || !is.null(water.content)) {
      if (is.null(clay.content)) 
            warning("To estimate soil strength, please inform water.content or matric.suction")
      if (is.numeric(matric.suction) & is.numeric(water.content))
            warning("To estimate soil strength, please inform only one of them: water.content or matric.suction")
   }

  # inverted water retention curve 
  vanG.matric <- function (theta, thetaR, thetaS, alpha, n) {
      S <- (theta-thetaR)/(thetaS-thetaR)
      f <- n/(1-n)
      h <- (1/alpha)*((S^f)-1)^(1/n)
      out <- data.frame(theta, h)
      return(out)
  }

  # parameters of the soil retention curve 
  thetaS <- c(0.47,0.54,0.57,0.64,0.66)
  thetaR <- c(0.049356,0.08689,0.10696,0.125941,0.139358)
  alpha <- c(0.79,0.72,1.66,2.04,2.27)
  n <- c(1.72,1.56,1.52,1.47,1.38)
  m <- c(0.42,0.36,0.34,0.33,0.28)

  # precompression stress estimation
     pre.cons.water <- function(clay.content, water.content) {
         mh <- c()
         for (j in 1:length(clay.content)) {
            if (clay.content[j] <= 20) {
               mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[1], thetaS=thetaS[1], alpha=alpha[1], n=n[1])$h
            }
            else if (clay.content[j] > 20 & clay.content[j] <= 31) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[2], thetaS=thetaS[2], alpha=alpha[2], n=n[2])$h
            }
            else if (clay.content[j] > 31 & clay.content[j] <= 37) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[3], thetaS=thetaS[3], alpha=alpha[3], n=n[3])$h
            }
            else if (clay.content[j] > 37 & clay.content[j] <= 52) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[4], thetaS=thetaS[4], alpha=alpha[4], n=n[4])$h
            }
            else {
               mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[5], thetaS=thetaS[5], alpha=alpha[5], n=n[5])$h
            }
         }
         return(round(mh, 2))
      }
       
     if (length(matric.suction) > 0) {
         matric.suction <- matric.suction
     }
     else {
            matric.suction <- pre.cons.water(clay.content = clay.content,water.content = water.content)
     }

     pcs <- c()
     for (j in 1:length(clay.content)) {
        if (clay.content[j] < 20) {
            pcs[j] <- round(129 * matric.suction[j]^(0.15),0)
        }
        else if (clay.content[j] >= 20 & clay.content[j] <= 31) {
            pcs[j] <- round(123.3 * matric.suction[j]^(0.13),0)
        }
        else if (clay.content[j] > 31 & clay.content[j] <= 37) {
            pcs[j] <- round(85 * matric.suction[j]^(0.17),0)
        }
        else if (clay.content[j] > 37 & clay.content[j] <= 52) {
            pcs[j] <- round(70.1 * matric.suction[j]^(0.16),0)
        }
        else if (clay.content[j] > 52) {
            pcs[j] <- round(62.7 * matric.suction[j]^(0.15),0)
        }
     }

     pcs05 <- pcs*0.5
     pcs11 <- pcs*1.1
     soil.strength <- data.frame(pcs,pcs05,pcs11)
        colnames(soil.strength) <- c("Pc","LL.Pc","UL.Pc")
    return(soil.strength)
}
