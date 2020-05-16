
ui = navbarPage(

    "LLWR calculation",
    
    
    
    
    tabPanel("Start", "This panel is intentionally left blank"),
    
    
    
    
    tabPanel("FC and PWP",
             
             titlePanel("FC and PWP restriction thresholds"),
             
             column(3,wellPanel(
               h4("h at the FC"),
               
               
               sliderInput("alpha1", "alpha",
                           min = 0.01, max = 0.5,
                           step = 0.001, value=0.056, tick=FALSE),
               
               
               sliderInput("n1", "n",
                           min = 1, max = 4,
                           value = 2.5, step = 0.0001,tick=FALSE)
               
               
             )),
             
             
             
             column(3,wellPanel(
               h4("Hydraulic cut-off"),
               
               
               sliderInput("C", "C [DE]",
                           min = 0.01, max = 0.15,
                           step = 0.0001, value=0.08,tick=FALSE),
               
               sliderInput("A1", "A1 [DE]",
                           min = 0.01, max = 0.10,
                           step = 0.0001, value=0.02,tick=FALSE),
               
               sliderInput("h1", "h1 [DE]",
                           min = 1000, max = 10000,
                           step = 1, value=4470,tick=FALSE),
               
               sliderInput("A2", "A2 [DE]",
                           min = 0.01, max = 0.50,
                           step = 0.0001, value=0.18,tick=FALSE),
               
               sliderInput("h2", "h2 [DE]",
                           min = 10, max = 2000,
                           step = 1, value=1400,tick=FALSE),
               
             )),
             
             
             column(5,wellPanel(
               h4("FC and PWP restriction thresholds"),
               tabsetPanel(type = "tabs",
                           tabPanel("h at the FC", plotOutput("plot1")),
                           tabPanel("Hydraulic cut-off", plotOutput("plot2")))              
                        
             )
          ),
             
             
  ),
    
  
  
  tabPanel("AFP and SPR",
           
           titlePanel("AFP and SPR restriction thresholds"),
           
           column(3,wellPanel(
             h4("Critical AFP"),
             
             
             sliderInput("TP", "Soil total porosity",
                         min = 0.2, max = 0.8,
                         step = 0.01, value=0.50, tick=FALSE),
             
             
             sliderInput("mim.gas.difusion", "Min gas difusion",
                         min = 0.005, max = 0.02,
                         value = 0.01, step = 0.0001,tick=FALSE)
             
             
           )),
           
           
           
           column(3,wellPanel(
             h4("Critical SPR"),
             
             
             sliderInput("root.rate", "Root elogation rate",
                         min = 10, max = 90,
                         step = 1, value=30,tick=FALSE),
             
             sliderInput("rate.factor", "Factor of decrease",
                         min = -0.7, max = -0.2,
                         step = 0.001, value=-0.4,tick=FALSE),
             
           )),
           
           
           column(5,wellPanel(
             h4("AFP and SPR restriction thresholds"),
             tabsetPanel(type = "tabs",
                         tabPanel("Air-filled porosity", plotOutput("plot3")),
                         tabPanel("Root elongation rate", plotOutput("plot4")))        
             
           )
           ),
           
           
  ),
  
  
    
    
    tabPanel("LLWR and LLMPR",
             
             titlePanel("Least Limiting Water and Matric Potential Ranges"),
             
             column(3,wellPanel(
               h4("Genuchten's parameters"),
               
               
               sliderInput("thetaS2", "thetaS (Genuchten's parameter)",
                           min = 0.2, max = 0.7,
                           step = 0.01, value=0.55, tick=FALSE),
               
               sliderInput("thetaR2", "thetaR (Genuchten's parameter)",
                           min = 0.01, max = 0.3,
                           step = 0.01, value=0.11, tick=FALSE),
               
               
               sliderInput("alpha2", "alpha (Genuchten's parameter)",
                           min = 0.01, max = 0.5,
                           value = 0.13, step = 0.0001,tick=FALSE),
               
               
               sliderInput("n2", "n (Genuchten's parameters)",
                           min = 1, max = 4,
                           step = 0.01, value=1.3, tick=FALSE),
               
               sliderInput("d", "d (Busscher's parameter)",
                           min = 0.001, max = 0.5,
                           step = 0.001, value=0.03, tick=FALSE),
               
               sliderInput("e", "e (Busscher's parameter)",
                           min = -3, max = -1,
                           step = 0.001, value=-2.9, tick=FALSE),
               
               
               sliderInput("f", "f (Busscher's parameter)",
                           min = 1, max = 8,
                           value = 3.54, step = 0.0001,tick=FALSE)

             )),
             
 
             
             
             column(3,wellPanel(
               h4("Restriction thresholds"),
               
               
               sliderInput("air", "AFP (m^3/m^3)",
                           min = 0.05, max = 0.25,
                           step = 0.001, value=0.10, tick=FALSE),
               
               
               sliderInput("fc", "FC (hPa)",
                           min = 10, max = 330,
                           value = 100, step = 1,tick=FALSE),
               
               
               sliderInput("pr", 'PR (MPa)',
                           min = 1, max = 7,
                           value = 4, step = 0.1,tick=FALSE),
               
               sliderInput("pwp", 'PWP (hPa)',
                           min = 10000, max = 20000,
                           value = 15000, step = 100,tick=FALSE),
              
               
               sliderInput("PD", 'Particle density (Mg/m^3)',
                           min = 2.4, max = 2.8,
                           value = 2.65, step = 0.01,tick=FALSE)
               
             )),
             
             
             
             
             column(5,wellPanel(
               h4("LLWR and LLMPR"),
               tabsetPanel(type = "tabs",
                           tabPanel("LLWR", plotOutput("plot5")),
                           tabPanel("LLMPR", plotOutput("plot6")),
                           tabPanel("Limits", tableOutput("values3")),
                           tabPanel("Range", tableOutput("values4")))  
                      
             )
             ),
        ),
    
    
    
  tabPanel("About", 
           "Renato P. de Lima")
    
    
    
    
)





# ---------------------------------------------------------------------------------
server <- function(input, output) {
  
  
hFC <-   function (alpha, n) (1/alpha)*((n-1)/n)^((1-2*n)/n)



AIR.critical <- function (mim.gas.difusion, thetaS)  (mim.gas.difusion*(thetaS)^2)^(1/(10/3))



PR.critical <- function (root.elongation.rate,x) log(root.elongation.rate)/x


hydraulicCutOff.2 <- function (theta_R,A1,A2,h1,h2) {
    
    wh <- function(x) theta_R + A1 * exp(-x/h1) + A2 * exp(-x/h2)
    fh <- function(x) A1/h1 * (x/h1 - 1) * exp(-x/h1) + A2/h2 * (x/h2 - 1) * exp(-x/h2)
    gh <- function(x) A1/h1 * exp(-x/h1) + A2/h2 * exp(-x/h2)
    dfh <- function(x) A1/h1^2 * (2 - x/h1) * exp(-x/h1) + A2/h2^2 * (2 - x/h2) * exp(-x/h2)
    kh <- function(x) ( log(10)^2 * x * fh(x) )/( 1 + x^2 * log(10)^2 * gh(x)^2 )^(3/2)
    hm <- optimize(kh, lower=1,upper=100000, maximum = T)$`maximum` # hPa
    
    return(hm)
}
  
  




llwr_llmpr <- function (thetaR, thetaS, alpha, n, d, e, f, critical.PR, 
                        PD,h.FC, h.PWP, air.porosity)
  
{
  m = 1 - 1/n
  vanG.matric <- function(theta, thetaR, thetaS, alpha, n) {
    S <- (theta - thetaR)/(thetaS - thetaR)
    f <- n/(1 - n)
    h <- (1/alpha) * ((S^f) - 1)^(1/n)
    out <- data.frame(theta, h)
    return(out)
  }
  
  BD <- round((1 - thetaS) * PD, 2)
  
  spr <- function(d, e, f, BD, critical.PR) {
    m <- (critical.PR/(d * BD^f))^(1/e)
    return(m)
  }
  
  thetaAIR <- (thetaS - air.porosity)
  thetaFC <- (thetaR + ((thetaS - thetaR)/(1 + (alpha * (h.FC))^n)^m))
  thetaPWP <- (thetaR + ((thetaS - thetaR)/(1 + (alpha * (h.PWP))^n)^m))
  thetaPR <- spr(d = d, e = e, f = f, BD = BD, critical.PR = critical.PR)
  hAIR <- vanG.matric(theta = thetaAIR, thetaR = thetaR, thetaS = thetaS, 
                      alpha = alpha, n = n)$h
  hPR <- vanG.matric(theta = thetaPR, thetaR = thetaR, thetaS = thetaS, 
                     alpha = alpha, n = n)$h
  if (hPR ==  "NaN" || hPR > 15000 || hPR == "NA" || hPR == Inf) (hPR <- 15000)
  
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
  
  SL_LLMPR <- vanG.matric(theta = SL_LLWR, thetaR = thetaR, 
                          thetaS = thetaS, alpha = alpha, n = n)$h
  SL_LLMPR.out <- vanG.matric(theta = SL_LLWR.out, thetaR = thetaR, 
                              thetaS = thetaS, alpha = alpha, n = n)$h
  IL_LLMPR <- vanG.matric(theta = IL_LLWR, thetaR = thetaR, 
                          thetaS = thetaS, alpha = alpha, n = n)$h
  IL_LLMPR.out <- vanG.matric(theta = IL_LLWR.out, thetaR = thetaR, 
                              thetaS = thetaS, alpha = alpha, n = n)$h
  LLWR <- (SL_LLWR - IL_LLWR)
  LLMPR <- (IL_LLMPR - SL_LLMPR)
  
  if (LLWR < 0) {LLWR <- 0}
  if (LLMPR ==  "NaN" || LLMPR < 0 || LLMPR == "NA") {LLMPR <- 0}
  matric <- round(c(hAIR, h.FC, h.PWP, hPR), 2)
  theta <- round(c(thetaAIR, thetaFC, thetaPWP, thetaPR), 4)
  out1 <- data.frame(theta = theta, potential = matric)
  rownames(out1) <- c("AIR", "FC", "PWP", 
                      "PR")
  Limits <- round(data.frame(Upper = SL_LLWR, Lower = IL_LLWR, 
                             Range = LLWR), 4)
  Limits[2, ] <- round(c(SL_LLMPR, IL_LLMPR, LLMPR), 2)
  rownames(Limits) <- c("LLWR", "LLMPR")
  out <- list(CRITICAL_LIMITS = out1, LLRW_LLMPR = Limits)
  return(out)
}




# NAVEGACAO 1 ------------------------------------------------------------------


output$plot1 <- renderPlot({
  
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(1, 1000), log="y", xlim=c(1,4),
       ylab = "", xlab = "", yaxt='n',
       type = "l")
  mtext(expression("h at the field capacity "~ (hPa)),2,line=2)
  mtext("n (Genuchten's parameters)",1,line=2.3)
  axis(2,at=c(1,10,100,1000))
  mtext("Genuchten's water retention curve", 3,line=1.5)
  
  y <- hFC(alpha=input$alpha1, n=seq(1,4,len=100))
  
  points(x=seq(1,4,len=100),y=y, type="l", lwd=2)
  h.FC <- hFC(alpha=input$alpha1, n=input$n1)
  
  segments(x0=input$n1,x1=input$n1,y0=0.1,y1=h.FC)
  segments(x0=-1,x1=input$n1,y0=h.FC,y1=h.FC)
  points(x=input$n1,y=h.FC, col=2, pch=15)
  
  
  
  legend("topright",legend=c("hFC",round(h.FC,2)))
  
  
})



output$plot2 <- renderPlot({
  
  
  DE <- function(theta_R,x,A1,A2,h1,h2) theta_R + A1 * exp(-x/h1) + A2 * exp(-x/h2)
  h <- seq(log10(1), log10(100000),len=100)
  y <- DE(x=10^h,theta_R = input$C,A1=input$A1,A2=input$A2,h1=input$h1,h2=input$h2)
  par(cex=0.9)
  plot(x=1,y=1,type="l", ylab="", xlab="", 
       xaxt="n", ylim=c(0,0.6),xlim=c(1,100000), log="x")
  x <- c(1,10,100,1000,10000,100000)
  axis(1, at=x, labels=as.character(x))
  points(x=10^h,y=y, type="l", lwd=2)
  mtext("Water content (w)", 2,line=2.4)
  mtext("Water suction (h)", 1,line=2.8)
  mtext("Dexter's water retention curve", 3,line=1.5)
  
  
  h.cut.off <- hydraulicCutOff.2(theta_R = input$C, 
                                 A1=input$A1,A2=input$A2,
                                 h1=input$h1,h2=input$h2)
  w <- DE(x=h.cut.off,theta_R = input$C,A1=input$A1,A2=input$A2,
          h1=input$h1,h2=input$h2)
  
  segments(x0=h.cut.off,x1=h.cut.off,y0=-1,y1=w)
  segments(x0=0.1,x1=h.cut.off,y0=w,y1=w)
  points(x=h.cut.off,y=w, col=2, pch=15)
  
  
  legend("topright",legend=c("hco",round(h.cut.off,0)))
  
  
  
})



output$values1 <- renderTable({
  
  h.FC <- hFC(alpha=input$alpha1, n=input$n1)
  
  h.cut.off <- hydraulicCutOff.2(theta_R = input$C, 
                                 A1=input$A1,A2=input$A2,
                                 h1=input$h1,h2=input$h2)
  
  
  out <- data.frame("hFC"=h.FC, "hco" = h.cut.off)
  out
  
})






# -----------------------------------------------------------------------------



# NAVEGACAO 2  -----------------------------------------------------------------



output$plot3 <- renderPlot({
  
  
  AIR.cri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                          thetaS=input$TP)
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(0, 0.5), xlim=c(0.005,0.02),
       ylab = "", xlab = "", 
       type = "l")
  mtext(expression("Minimal air-filled porosity"~(m^3~m^-3)),2,line=2)
  mtext("Relative gas diffusivity (-)",1,line=2.3)
  
  y <- AIR.critical(mim.gas.difusion=seq(0.005,0.02,len=100), 
                    thetaS=input$TP)
  points(x=seq(0.005,0.02,len=100),y=y, type="l", lwd=2)
  AFPcri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                         thetaS=input$TP)
  
  segments(x0=input$mim.gas.difusion,x1=input$mim.gas.difusion,y0=-1,y1=AFPcri)
  segments(x0=-1,x1=input$mim.gas.difusion,y0=AFPcri,y1=AFPcri)
  points(x=input$mim.gas.difusion,y=AFPcri, col=2, pch=15)
  
  
  legend("topright",legend=c(expression(AFP[minimal]),round(AFPcri,4)))
  
  
})




output$plot4 <- renderPlot({
  

  c.PR <- PR.critical(root.elongation.rate=input$root.rate/100,
                      x=input$rate.factor) 
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(0, 100), xlim=c(0,15),
       ylab = "", xlab = "", 
       type = "l")
  mtext("SPR (MPa)",1,line=2.5)
  mtext("Root elongation rate (%)",2,line=2.5)
  
  f <- function (x,Q) exp(x*Q)
  y <- f(x=input$rate.factor,Q=seq(0.1,15,len=100))
  points(x=seq(0.1,15,len=100),y=y*100, type="l", lwd=2)
  Qcri <- (log(input$root.rate/100))/input$rate.factor
  
  segments(x0=Qcri,x1=Qcri,y0=-10,y1=input$root.rate)
  segments(x0=-10,x1=Qcri,y0=input$root.rate,y1=input$root.rate)
  points(x=Qcri,y=input$root.rate, col=2, pch=15)
  
  legend("topright",legend=c(expression(SPR[critical]),round(Qcri,2)))
  
  
})



output$values2 <- renderTable({
  
  c.PR <- PR.critical(root.elongation.rate=input$root.rate/100,
                      x=input$rate.factor) 
  
  AIR.cri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                          thetaS=input$TP)
  
  
  out <- data.frame("Critical SPR"=c.PR, "Critical AFP" = AIR.cri)
  out
  
})



# ------------------------------------------------------------------------------




# NAVEGACAO 3  -----------------------------------------------------------------



output$plot5 <- renderPlot({
  
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2, 
                     d=input$d, e=input$e, f=input$f, 
                     critical.PR=input$pr, 
                     PD=input$PD,
                     h.FC=input$fc, h.PWP=input$pwp, 
                     air.porosity=input$air)
  
  par(cex = 0.9, mar = c(4, 4, 2, 4))
  plot(y = 1, x = 1, xlim = c(0, 1), pch = 15, ylim=c(0,0.8),
       ylab = "", xlab = "",xaxt = "n", 
       type = "l", cex = 0.9)
  mtext(expression(theta~(m^3~m^-3)), side = 2, 
        line = 2.2, las = 3, cex = 0.9)
  
  la <- round((1 - input$thetaS2) * input$PD, 2)
  mtext(expression("Bulk density" ~ (Mg~m^3)), side = 1, line = 2.2, cex = 0.9)
  
  axis(1, at = 0.5, labels = la)
  x <- c(0, 1, 1, 0)
  yU <- LLWR$LLRW_LLMPR[1,1]
  yL <- LLWR$LLRW_LLMPR[1,2]
  y <- c(yL, yL, yU, yU)
  
  RANGE <- LLWR$LLRW_LLMPR[1,3]
  
  if (RANGE > 0) {  polygon(x, y, col = "gray")}
  
  points(x = rep(0.5, 4), 
         y = c(LLWR$CRITICAL_LIMITS[1,1], LLWR$CRITICAL_LIMITS[2,1], 
               LLWR$CRITICAL_LIMITS[3,1], LLWR$CRITICAL_LIMITS[4,1]), 
         pch = 15)
  points(x = rep(0.5, 2), 
         y = c(LLWR$LLRW_LLMPR[1,1], LLWR$LLRW_LLMPR[1,2]), col = 2, 
         pch = 15)
  f <- 0.15
  labels = c(expression(theta[AFP]), expression(theta[FC]), 
             expression(theta[PWP]), expression(theta[SPR]))
  text(labels, x = c(0.5 + f, 0.5 - f, 0.5 - f, 0.5 + f), 
       y = c(LLWR$CRITICAL_LIMITS[1,1], LLWR$CRITICAL_LIMITS[2,1], 
             LLWR$CRITICAL_LIMITS[3,1], LLWR$CRITICAL_LIMITS[4,1]), cex = 1.2)
  
  legend("topright",legend=c("LLWR",round(RANGE,4)))
  
  
})



output$plot6 <- renderPlot({
  
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  
  
  par(cex = 0.9, mar = c(4, 4, 2, 4))
  plot(y = 1, x = 1, xlim = c(0, 1), log = "y", pch = 15, ylim=c(100000,1),
       ylab = "", xlab = "", xaxt = "n", 
       type = "l", cex = 0.9)
  mtext(expression(h~(hPa)), side = 2, line = 2.2, las = 3, cex = 0.9)
  axis(2, at = c(1, 10, 100, 1000, 10000,100000), 
       labels=c(1, 10, 100, 1000, 10000,expression(10^5)))
  
  
  la <- round((1 - input$thetaS2) * input$PD, 2)
  mtext(expression("Bulk density" ~ (Mg~m^3)), side = 1, line = 2.2, cex = 0.9)
  
  axis(1, at = 0.5, labels = la)
  x <- c(0, 1, 1, 0)
  yU <- LLWR$LLRW_LLMPR[2,1]
  yL <- LLWR$LLRW_LLMPR[2,2]
  y <- c(yL, yL, yU, yU)
  
  RANGE <- LLWR$LLRW_LLMPR[2,3]
  
  if (RANGE > 0) {  polygon(x, y, col = "gray")}
  
  points(x = rep(0.5, 4), 
         y = c(LLWR$CRITICAL_LIMITS[1,2], LLWR$CRITICAL_LIMITS[2,2], 
               LLWR$CRITICAL_LIMITS[3,2], LLWR$CRITICAL_LIMITS[4,2]), 
         pch = 15)
  points(x = rep(0.5, 2), 
         y = c(LLWR$LLRW_LLMPR[2,1], LLWR$LLRW_LLMPR[2,2]), col = 2, 
         pch = 15)
  f <- 0.15
  labels = c(expression(h[AFP]), expression(h[FC]), 
             expression(h[PWP]), expression(h[SPR]))
  text(labels, x = c(0.5 + f, 0.5 - f, 0.5 - f, 0.5 + f), 
       y = c(LLWR$CRITICAL_LIMITS[1,2], LLWR$CRITICAL_LIMITS[2,2], 
             LLWR$CRITICAL_LIMITS[3,2], LLWR$CRITICAL_LIMITS[4,2]), cex = 1.2)
  
  legend("topright",legend=c("LLMPR",round(RANGE,4)))
  
  
})




output$values3 <- renderTable({
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  data <- matrix(nrow=2,ncol=5)
  
  waterlimits <- format(c(LLWR$CRITICAL_LIMITS[1,1],
                        LLWR$CRITICAL_LIMITS[2,1],
                        LLWR$CRITICAL_LIMITS[3,1], 
                        LLWR$CRITICAL_LIMITS[4,1]),digits = 4)

  data[1,] <- c("LLWR", waterlimits)
  
  
  hlimits <- format(c(LLWR$CRITICAL_LIMITS[1,2],
               LLWR$CRITICAL_LIMITS[2,2],
               LLWR$CRITICAL_LIMITS[3,2], 
               LLWR$CRITICAL_LIMITS[4,2]),digits = 1)
  
  data[2,] <- c("LLMPR",hlimits)
  colnames(data) <- c("-","AFP","FC","PWP","SPR")
  data
  
  })



output$values4 <- renderTable({
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  data <- matrix(nrow=2,ncol=4)
  
  waterlimits <- format(c(LLWR$LLRW_LLMPR[1,1],
                          LLWR$LLRW_LLMPR[1,2], 
                          LLWR$LLRW_LLMPR[1,3]),digits = 4)
  
  data[1,] <- c("LLWR", waterlimits)
  
  
  hlimits <- format(c(LLWR$LLRW_LLMPR[2,1],
                      LLWR$LLRW_LLMPR[2,2], 
                      LLWR$LLRW_LLMPR[2,3]),digits = 1)
  
  data[2,] <- c("LLMPR",hlimits)
  colnames(data) <- c("-","Upper","Lower","Range")
  data
  
})


  
}


