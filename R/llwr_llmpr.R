
llwr_llmpr <- function(thetaR, thetaS, alpha, n, d, e, f = NULL, critical.PR, PD, Bd = NULL, 
         h.FC, h.PWP, air.porosity,
         labels = c("AIR", "FC", "PWP", "PR"), ylab = "",
         graph1 = TRUE, graph2 = FALSE, ...)
{   

  # ---
   m = 1 - 1/n
   vanG.matric <- function (theta, thetaR, thetaS, alpha, n) {

         S <- (theta-thetaR)/(thetaS-thetaR)
         f <- n/(1-n)
         h <- (1/alpha)*((S^f)-1)^(1/n)
         out <- data.frame(theta,h)
         return(out)
       }



   BD <- round((1-thetaS)*PD,2)
   spr <- function (d,e,f,BD,critical.PR) {
          
          if (is.null(f)) {
             f <- 0 
             BD <- 1
           }
          m <- (critical.PR/(d*BD^f))^(1/e)
          return(m)
       }


   thetaAIR <- (thetaS - air.porosity)
   thetaFC <- (thetaR + ((thetaS-thetaR)/(1+(alpha*(h.FC))^n)^m))
   thetaPWP <- (thetaR + ((thetaS-thetaR)/(1+(alpha*(h.PWP))^n)^m))
   thetaPR <- spr(d=d,e=e,f=f,BD=BD,critical.PR=critical.PR) 

   hAIR <- vanG.matric(theta=thetaAIR, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h
   hPR <-  vanG.matric(theta=thetaPR, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h
         
   if (hPR ==  "NaN" || hPR == "NA" || hPR == Inf) (hPR <- h.PWP)
  # ---



        SL <- c()
        SL.out <- c()
        IL <- c()
        IL.out <- c()
        for (j in 1:length(thetaS)) {
            if (thetaAIR[j] < thetaFC[j]) {
                SL[j] <- thetaAIR[j]
                SL.out[j] <- thetaFC[j]
            }
            else if (thetaAIR[j] > thetaFC[j]) {
                SL[j] <- thetaFC[j]
                SL.out[j] <- thetaAIR[j]
            }
            if (thetaPWP[j] > thetaPR[j]) {
                IL[j] <- thetaPWP[j]
                IL.out[j] <- thetaPR[j]
            }
            else if (thetaPWP[j] < thetaPR[j]) {
                IL[j] <- thetaPR[j]
                IL.out[j] <- thetaPWP[j]
            }
        }

        SL_LLWR <- SL
        SL_LLWR.out <- SL.out
        IL_LLWR <- IL
        IL_LLWR.out <- IL.out
        SL_LLMPR <- vanG.matric(theta=SL_LLWR, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h
        SL_LLMPR.out <- vanG.matric(theta=SL_LLWR.out, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h
        IL_LLMPR <- vanG.matric(theta=IL_LLWR, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h
        IL_LLMPR.out <- vanG.matric(theta=IL_LLWR.out, thetaR=thetaR, thetaS=thetaS, 
                       alpha=alpha, n=n)$h


        LLWR <- (SL_LLWR - IL_LLWR)
        LLMPR <- (IL_LLMPR - SL_LLMPR)
        if (LLWR < 0) {LLWR <- 0}
        if (LLMPR < 0) {LLMPR <- 0}

   
   matric <- round(c(hAIR, h.FC, h.PWP, hPR),2)
   theta <- round(c(thetaAIR,thetaFC,thetaPWP,thetaPR),4)
   out1 <- data.frame(theta=theta, potential=matric)
   rownames(out1) <- c("AIR", "FC", "PWP", "PR")

   Limits <- round(data.frame(Upper=SL_LLWR, Lower=IL_LLWR, Range=LLWR),4)
   Limits[2,] <- round(c(SL_LLMPR, IL_LLMPR, LLMPR),2) 
   rownames(Limits) <- c("LLWR","LLMPR") 


 if (graph1) {

  par(cex=0.9, mar=c(4,4,2,4))
  plot(y=1,x=1, xlim=c(0,1), log="y", 
      pch=15, ylab="", xlab="", xaxt='n',type="l",cex=0.9,...)
  mtext(ylab,side=2,line=2.2, las=3,cex=0.9)
  axis(2,at=c(1,10,100,1000,10000))
  la <- Bd
  if (is.null(Bd)) {
    la <- BD
     }
  axis(1,at=0.5,labels=la)

  x <- c(0,1,1,0)
  yU <- SL_LLMPR
  yL <- IL_LLMPR
  y <- c(yL,yL,yU,yU)
  polygon(x,y,col="gray")

  abline(h=c(SL_LLMPR,IL_LLMPR), col=2)
  points(x=rep(0.5,4),y=c(hAIR, h.FC, h.PWP, hPR), pch=15)
  points(x=rep(0.5,2),y=c(SL_LLMPR,IL_LLMPR),col=2, pch=15)
  f <- 0.15
  text(labels, x=c(0.5+f,0.5-f,0.5-f,0.5+f), y=c(hAIR, h.FC, h.PWP, hPR), cex=0.9)

  }


 if (graph2) {

  par(cex=0.9, mar=c(4,4,2,4))
  plot(y=1,x=1, xlim=c(0,1), 
      pch=15, ylab="", xlab="", xaxt='n',type="l",cex=0.9, ...)
  mtext(ylab,side=2,line=2.2, las=3,cex=0.9)
  la <- Bd
  if (is.null(Bd)) {
    la <- BD
     }
  axis(1,at=0.5,labels=la)


  x <- c(0,1,1,0)
  yU <- SL_LLWR
  yL <- IL_LLWR
  y <- c(yL,yL,yU,yU)
  polygon(x,y,col="gray")

  abline(h=c(SL_LLWR,IL_LLWR), col=2)
  points(x=rep(0.5,4),y=c(thetaAIR, thetaFC, thetaPWP, thetaPR), pch=15)
  points(x=rep(0.5,2),y=c(SL_LLWR,IL_LLWR),col=2, pch=15)
  f <- 0.15
  text(labels, x=c(0.5+f,0.5-f,0.5-f,0.5+f), 
       y=c(thetaAIR, thetaFC, thetaPWP, thetaPR), cex=0.9)

  }




   out <- list(CRITICAL_LIMITS = out1, LLRW_LLMPR = Limits)
   return(out)
}

