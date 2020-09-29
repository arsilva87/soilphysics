


server_IndSoilWater <- function(input, output) {
  
  # my function 
  
  SI <- function(theta_R, theta_S, alpha, n, m = 1 - 1/n) {
    
    S <- -n * (theta_S - theta_R) * (1 + 1/m)^(-(1 + m))
    return(abs(S))
  }
  
  
  
  Kr_theta <- function (theta, thetaS, thetaR, alpha, n, Ks, f = 0.5) 
  {
    m <- 1 - (1/n)
    Se <- (theta - thetaR)/(thetaS - thetaR)
    a <- (1 - (1 - Se^(1/m))^m)^2
    out <- Ks * (Se^f) * a
    return(out)
  }
  
  
  Kr_h <- function (Ks, alpha, n, h, f = 0.5) 
  {
    m <- 1 - (1/n)
    Se <- (1/(1 + (alpha * h)^n))^m
    b <- (1 - (1 - Se^(n/(n - 1)))^m)^2
    out <- Ks * (Se^f) * b
    return(out)
  }
  
  
  
  
  psd <-  function(thetaS, thetaR, alpha, n, h) {
    x <- 10^h
    out <- abs((thetaS - thetaR) * (1 + (alpha * x)^n)^(1/n - 
                                                            1) * (1/n - 1) * (alpha * x)^n * (n/(x * (1 + (alpha * 
                                                                                                             x)^n))))
    return(out)
  }

  van <-  function(thetaS, thetaR, alpha, n, h) {
    h <- 10^h
    m <- 1-1/n
    out <- thetaR + ((thetaS-thetaR)/(1+(alpha*h)^n)^m)  
    return(out)
  }
  
  
  
  TimeD <- function (thetaS,thetaR,Ks,z,Kr) {
    
    Q <- z*(thetaS-thetaR)
    t <- -(Q/Ks)*log(Kr/Ks)
    time <- data.frame(depth.cm=z,days=t,hours=t*24)
    return(time)
  }
  
  
  
  output$plot1 <- renderPlot({
    
    h <- seq(0,log10(15000), len=500)
    w <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
             alpha=input$alpha, n=input$n, h=h)
    
    plot(x=10^h,y=w,xaxt='n',lwd=2,type="l", log="x",
         xlim=c(1,15000), 
         ylim=c(0,round(input$thetaS,2)+0.05),xlab="",ylab="")
    
    axis(1, at=c(1,10,100,1000,10000),labels=c(1,10,100,1000,10000))
    mtext("Water matric tension (hPa)",1,line=2.5)
    mtext(expression(theta~(m^3~m^-3)),2,line=2.5)
    
    
    MaP <- c()
    if (input$macro==1) {MaP <- 10}
    if (input$macro==2) {MaP <- 30}
    if (input$macro==3) {MaP <- 60}
    if (input$macro==4)  {MaP <- input$macro2}
    
    
    FC <- c()
    if (input$micro==1) {FC <- 100}
    if (input$micro==2) {FC <- 330}
    if (input$micro==3)  {FC <- input$micro2}

  
    if (input$thresholds==TRUE) {
    abline(v=as.numeric(MaP), col=1)
    abline(v=as.numeric(FC), col=2)
    legend("topright",legend=c("Thresholds","Macroporosity","Field capacity"), lwd=2, 
           cex=0.9, col=c(NA,1,2))
    }
    
    
  })
  
  
 output$plot2 <- renderPlot({
   
    h <- seq(0,log10(15000), len=500)
    w <- psd(thetaS=input$thetaS, thetaR=input$thetaR, 
             alpha=input$alpha, n=input$n, h=h)
    

    plot(x=10^h,y=w,xaxt='n',lwd=2,type="l", log="x",
        xlim=c(1,15000), 
        ylim=c(0,round(max(w),3)+0.005),xlab="",ylab="")
    
    axis(1, at=c(1,10,100,1000,10000), 
         labels=c(1,10,100,1000,10000))
    mtext(expression(delta*theta/delta*h),2,line=2.5)
    mtext("Water matric tension (hPa)",1,line=2.5)
    
    
    axis(3, at=c(1,10,100,1000,10000),labels=c(1467,146,15,1.50,0.15))
    mtext(expression("Equivalent pore radius"~(mu*m)),3,line=2.5)

    
    
    MaP <- c()
    if (input$macro==1) {MaP <- 10}
    if (input$macro==2) {MaP <- 30}
    if (input$macro==3) {MaP <- 60}
    if (input$macro==4)  {MaP <- input$macro2}
    
    
    FC <- c()
    if (input$micro==1) {FC <- 100}
    if (input$micro==2) {FC <- 330}
    if (input$micro==3)  {FC <- input$micro2}
    
    
    if (input$thresholds==TRUE) {
    abline(v=as.numeric(MaP), col=1)
    abline(v=as.numeric(FC), col=2)
    legend("topright",legend=c("Thresholds","Macroporosity","Field Capacity"), lwd=2, 
           cex=0.9, col=c(NA,1,2))
    }
    
    
    
    x0 <- 10^h
    x1 <- rev(10^h) 
    y0 <- rep(0, length(w))
    y1 <- rev(w)
    polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
            col=rgb(red=1, green=0, blue=0, alpha=0.3))
    points(x=10^h,y=w, lwd=2, type="l")

    
    
  })
 
 
 
 
 output$plot3 <- renderPlot({
   
   KS <- (4.65*10^4)*(input$thetaS)*(input$alpha^2)
   
   Ks <- c()
   if (input$Ks.action==TRUE) {Ks <- input$Ks.in}
   if (input$Ks.action==FALSE) {Ks <- KS}
   
   
   theta <- seq(input$thetaS, input$thetaR, len=50)
   y <- Kr_theta(theta=theta,thetaS=input$thetaS,thetaR=input$thetaR, 
                 alpha = input$alpha, n = input$n, Ks = Ks, f=0.5)
   Kr <- y[-50]
   w <- theta[-50]
   par(cex=0.9)
   plot(x=w,y=Kr,xlab="",
        ylim=c(0.00001,1000), log="y",yaxt='n',xaxt='n',
        ylab="", xlim=c(0,0.80), type="l", lwd=2)
   mtext(expression(K[theta] ~ (cm~d^-1)), 2, line=2)
   mtext(expression(theta~(m^3~m^-3)), 3, line=2)
   axis(3, at=seq(0,0.8,0.1))
   axis(3, at=seq(0,0.8,0.05), label=FALSE)
   ax <- c(0.00001,0.0001,0.001, 0.01, 0.1, 1, 10, 100,1000)
   label <- c(expression(10^{-5}),expression(10^{-4}),
              expression(10^{-3}), 
              0.01, 0.1, 1, 10, 100,1000)
   axis(2,at=ax, labels=label)
   legend("topright",legend=c(expression(K[s]),round(y[1],2)))
   points(x=input$thetaS,y=Ks, pch=15, cex=1)
   
 })
 
 
 
 
 
 output$plot4 <- renderPlot({
   
   hFC <- c()
   if (input$micro==1) {hFC <- 100}
   if (input$micro==2) {hFC <- 330}
   if (input$micro==3)  {hFC <- input$micro2}
   
   

   
   z <- seq(1,50,len=30)

   thetaFC <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, h=log10(hFC))
   KS <- (4.65*10^4)*(input$thetaS)*(input$alpha^2)
   
   
   Ks <- c()
   if (input$Ks.action==TRUE) {Ks <- input$Ks.in}
   if (input$Ks.action==FALSE) {Ks <- KS}
   

   Kr <- Kr_theta(theta=thetaFC, thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, Ks=Ks)
   t <- TimeD(thetaS=input$thetaS,thetaR=input$thetaR,Ks=Ks,z=z,Kr=Kr)
   
   par(cex=0.9)
   plot(y=z,x=t$days, yaxt='n', xaxt='n', xlim=c(0.001,1000), ylim=c(50,0),
        log="x", ylab="", xlab="", type="l", lwd=2)
   x <- c(0.001,0.01,0.1,1,10,100,100,1000)
   axis(3,at=x, labels=as.factor(x))
   axis(2)
   mtext("Time to reach the FC (Days)",3,line=2.3)
   mtext("Depth (cm)",2,line=2.3)
   
   
 })
 
 
 
 

 output$values <- renderTable({
   
   
   MaP <- c()
   if (input$macro==1) {MaP <- 10}
   if (input$macro==2) {MaP <- 30}
   if (input$macro==3) {MaP <- 60}
   if (input$macro==4)  {MaP <- input$macro2}
   
   FC <- c()
   if (input$micro==1) {FC <- 100}
   if (input$micro==2) {FC <- 330}
   if (input$micro==3)  {FC <- input$micro2}
   
  # Time of drainage
   z <- 10
   thetaFC <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, h=log10(FC))
   KS <- (4.65*10^4)*(input$thetaS)*(input$alpha^2)
   
   
   Ks <- c()
   if (input$Ks.action==TRUE) {Ks <- input$Ks.in}
   if (input$Ks.action==FALSE) {Ks <- KS}
   
   
   Kr <- Kr_theta(theta=thetaFC, thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, Ks=Ks)
   t <- TimeD(thetaS=input$thetaS,thetaR=input$thetaR,Ks=Ks,z=z,Kr=Kr)
   
   
  # Indircators 
   TP <- input$thetaS
   hFC <- log10(FC)
   FC <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
             alpha=input$alpha, n=input$n, h=hFC)
   hMA <- log10(MaP)
   macro <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
                alpha=input$alpha, n=input$n, h=hMA)
   
   thetaS <- input$thetaS
   alpha <- input$alpha

  
   MA <- TP-macro
   ME <- macro-FC
   
   WP <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
             alpha=input$alpha, n=input$n, h=15000)
   PAW <- round(FC-WP,4)
   
   BD <- (1-input$thetaS)*input$PD
   S <- SI(theta_R=input$thetaR/BD, theta_S=input$thetaS/BD,
           alpha=input$alpha, n=input$n)
   
   WC <- round((FC/TP)*100,0)
   AC <- round(100-WC,0)
   
   
   A <- c("TP","MA","ME","FC","PWP","PAW")
  
   out <- data.frame(TP,MA,ME,FC,WP,PAW)
   colnames(out) <- A
   format(out, digits=3,justify="centre")

  
 })
 
 

 
 
 
 output$values2 <- renderTable({
   
   
   FC <- c()
   if (input$micro==1) {FC <- 100}
   if (input$micro==2) {FC <- 330}
   if (input$micro==3)  {FC <- input$micro2}
   
   
   # Indircators 
   TP <- input$thetaS
   hFC <- log10(FC)
   FC <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
             alpha=input$alpha, n=input$n, h=hFC)


   BD <- (1-input$thetaS)*input$PD
   S <- SI(theta_R=input$thetaR/BD, theta_S=input$thetaS/BD,
           alpha=input$alpha, n=input$n)
   
   WC <- (FC/TP)*100
   AC <- 100-WC
   
  
   A <- c("WC","AC","BD","S") 
   m <- matrix(nrow=1,ncol=4)
   m[1,] <- c(round(WC,0),round(AC,0),BD,round(S,4))
   colnames(m) <- A
   format(m, digits=3,justify="centre")
   
 })
 
 
 
 
 
 data.down <- reactive({
   
# Water retention  
   h <- seq(0,log10(15000), len=500)
   w <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
            alpha=input$alpha, n=input$n, h=h)
   
   WRC <- data.frame(h=10^h,theta=w)
   

   
# PSD
   
   h <- seq(0,log10(15000), len=500)
   soilpores <- psd(thetaS=input$thetaS, thetaR=input$thetaR, 
            alpha=input$alpha, n=input$n, h=h)
   
   
   PSD <- data.frame(h=10^h,PSD=soilpores)
   

   
# CHS

   KS <- (4.65*10^4)*(input$thetaS)*(input$alpha^2)
   
   Ks <- c()
   if (input$Ks.action==TRUE) {Ks <- input$Ks.in}
   if (input$Ks.action==FALSE) {Ks <- KS}
   
   
   theta <- seq(input$thetaS, input$thetaR, len=100)
   y <- Kr_theta(theta=theta,thetaS=input$thetaS,thetaR=input$thetaR, 
                 alpha = input$alpha, n = input$n, Ks = Ks, f=0.5)
   
   SHC <- data.frame(theta=theta,Kr=y)
   
   
# Drainage
   MaP <- c()
   if (input$macro==1) {MaP <- 10}
   if (input$macro==2) {MaP <- 30}
   if (input$macro==3) {MaP <- 60}
   if (input$macro==4)  {MaP <- input$macro2}
   
   FC <- c()
   if (input$micro==1) {FC <- 100}
   if (input$micro==2) {FC <- 330}
   if (input$micro==3)  {FC <- input$micro2}
   
   # Time of drainage
   z <- seq(1,100,by=1)
   thetaFC <- van(thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, h=log10(FC))
   
   KS <- (4.65*10^4)*(input$thetaS)*(input$alpha^2)
   
   Ks <- c()
   if (input$Ks.action==TRUE) {Ks <- input$Ks.in}
   if (input$Ks.action==FALSE) {Ks <- KS}
   

   Kr <- Kr_theta(theta=thetaFC, thetaS=input$thetaS, thetaR=input$thetaR, 
                  alpha=input$alpha, n=input$n, Ks=Ks)
   t <- TimeD(thetaS=input$thetaS,thetaR=input$thetaR,Ks=Ks,z=z,Kr=Kr)
   
   
   download <- c()
   if (input$down=="WRC") {download <- WRC}
   if (input$down=="PSD") {download <- PSD}
   if (input$down=="Kr") {download <- SHC}
   if (input$down=="DrainageTime") {download <- t}
   out <- download
   out

   
 })
 
 
 
 output$downloadData <- downloadHandler(
   filename = function(){"output_data.csv"}, 
   content = function(fname){
     write.csv(data.down(), fname, row.names = FALSE)
   }
 )
 
 

# ABA 1 ------------------------------
 

 output$plot0 <- renderPlot({
    
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(1,100000), log="x",yaxt='n',xaxt='n',
         ylab="", ylim=c(0,0.80), type="l", lwd=2)
    mtext(expression(theta~(m^3~m^-3)), 2, line=2.3)
    mtext("h (hPa)", 1, line=2.3)
    x <- c(1,10,100,1000,10000,100000)
    axis(1,at=x, labels=c(1,10,100,1000,10000,expression(10^5)))
    axis(2)
    
    x <- c(input$p1,input$p2,input$p3,input$p4,input$p5,input$p6,
           input$p7,input$p8,input$p9,input$p10)
    y <- c(input$theta1,input$theta2,input$theta3,input$theta4,input$theta5,
           input$theta6,input$theta7,input$theta8,input$theta9,input$theta10)
    points(x=x,y=y,pch=15)
    
    h <- seq(0,log10(15000), len=100)
    w <- van(thetaS=input$thetaS0, thetaR=input$thetaR0, 
             alpha=input$alpha0, n=input$n0, h=h)
    points(x=10^h, y=w, type="l", col="red")
    
    

    OUT <- mySUMMARY$fitting 
    if (class(OUT)=="summary.nls") {
      
      data <- OUT$parameters[,1]
      names <- rownames(OUT$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
       
    thetaS <- c()
    thetaR <- c()
    alpha <- c()
    n <- c()
    if (length(table[1,])==4){
    thetaS <- table$thetaS[1]
    thetaR <- table$thetaR[1]
    alpha <- table$alpha[1]
    n <- table$n[1]
    }
    
    if (length(table[1,])==2){
       thetaS <- input$thetaS0
       thetaR <- input$thetaR0
       alpha <- table$alpha[1]
       n <- table$n[1]
    }
    
       
    w2 <- van(thetaS=thetaS, thetaR=thetaR, 
             alpha=alpha, n=n, h=h)
    points(x=10^h, y=w2, type="l", col="blue")
    legend("topright",legend="Fitted model", lwd=1, col="blue")
    
    }

    
 })
 
 
 
 
 
 mySUMMARY <- reactiveValues(Data=NULL)
 
 observeEvent(input$start,{
    
    OUT <- NULL
    h <- c(input$p1,input$p2,input$p3,input$p4,input$p5,input$p6,
           input$p7,input$p8,input$p9,input$p10)
    w <- c(input$theta1,input$theta2,input$theta3,input$theta4,input$theta5,
           input$theta6,input$theta7,input$theta8,input$theta9,input$theta10)
    
    thetaR <- input$thetaR0
    thetaS <- input$thetaS0
    theta_R <- input$thetaR0
    theta_S <- input$thetaS0
    alphaa <- input$alpha0
    nn <- input$n0
    
    lista <- list()
    if (input$thetaSR==FALSE) {lista <- c(thetaR=theta_R,thetaS=theta_S,alpha=alphaa, n=nn)}
    if (input$thetaSR==TRUE) {lista <- c(alpha=alphaa, n=nn)}
    
    m <- try(nls(w ~ thetaR + ((thetaS-thetaR)/(1+(alpha*h)^n)^(1 - 1/n)), start=lista))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- summary(m)
    mySUMMARY$fitting <- OUT


 })
 
 
 output$fitting <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY$fitting) || class(mySUMMARY$fitting)!="summary.nls") {OUT <- "Try again!"}
    if (class(mySUMMARY$fitting)=="summary.nls") {OUT <- mySUMMARY$fitting}
    if (is.null(mySUMMARY$fitting)) {OUT <- "Moves the slider input for a numerical starting"}
    OUT
    
 })
 

 
}






ui_IndSoilWater <- fluidPage(
  
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: LightSkyBlue;}',
             '.navbar-default .navbar-brand{color: black;}',
             '.tab-panel{ background-color: black; color: black}',
             '.nav navbar-nav li.active:hover a, .nav navbar-nav li.active a {
                        background-color: black ;
                        border-color: black;
                        }'
             
  ),
  
  navbarPage(
    
 "IndSoilWater",
 
 

  
 tabPanel("Exploring the WRC",
 
 
 verticalLayout(
   column(12,wellPanel(
     
     helpText(tags$p("IndSoilWater: using IndSoilWater, the user is able  explore soil 
                      retention curve and associated soil physical indicators by assigning the macroporosity and 
                      field capacity water tension thresholds, and the Genuchten parameters. Saturated hydraulic conductivity 
                      is estimated from the Genuchten's parameters, or optionally assinged by the user. Outputs include graphs, the soil indicators, 
                      and downloadable data sets.",
                     
                     style = "font-size: 100%;text-align:justify"))
     
   ))),
 
    
  column(4,wellPanel(h4(""),

                 
      # -------
      
      radioButtons("macro", "Macroporosity water tension threshold?",
                   c("10 hPa"= 1,"30 hPa"= 2,"60 hPa"= 3, "User define"=4)),
      
      sliderInput("macro2", "Define macroporosity threshold",
                  min = 10, max = 60,
                  step = 1, value=30,tick=FALSE),
      
      
      radioButtons("micro", "Field capacity threshold?",
                   c("100 hPa (Field capacity - usually for sandy soils)"= 1,
                     "330 hPa (Field capacity - usually for clayey soils)"= 2,
                     "User define"=3)),
  
      sliderInput("micro2", "Define field capacity threshold",
                  min = 60, max = 330,
                  step = 1, value=100,tick=FALSE),
      
      
      sliderInput("thetaS", HTML(paste0("&theta;",tags$sub("s") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                  min = 0, max = 0.8000,
                  step = 0.001, value=0.560,tick=FALSE),
      
      sliderInput("thetaR", HTML(paste0("&theta;",tags$sub("r") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                  min = 0, max = 0.30,
                  step = 0.001, value=0.150,tick=FALSE),
      
      sliderInput("alpha", HTML(paste0("&alpha; (hPa",tags$sup("-1"),")")),
                  min = 0, max = 0.20,
                  step = 0.001, value=0.026,tick=FALSE),
      
      sliderInput("n", "n",
                  min = 1, max = 4,
                  value = 2.145, step = 0.001,tick=FALSE),
      
      sliderInput("PD", HTML(paste0("Particle density (Mg m",tags$sup("-3"),")")),
                  min = 2.4, max = 2.9,
                  step = 0.01, value=2.65,tick=FALSE),
      

      numericInput("Ks.in", HTML(paste0("Ks (cm d",tags$sup("-1"),")")),
                          min = 0.01, max = 1000,
                          step = 1, value=100,width='30%'),
      checkboxInput("Ks.action", "Optionally, provide Ks", value = FALSE)
      
      
    )),

    
  
  column(4,wellPanel(
             h4("Exploring water retention curve"),
             tabsetPanel(type = "tabs",
                         tabPanel("WRC", plotOutput("plot1")),
                         tabPanel("PSD", plotOutput("plot2")),
                         tabPanel("K", plotOutput("plot3")),
                         tabPanel("DT", plotOutput("plot4")),
                         tabPanel("SWI", tableOutput("values")),
                         tabPanel("SPQI", tableOutput("values2"))),
             
             helpText(
               HTML("WRC: water retention curve;"),
               HTML("PSD: pore size distribution;"),
               HTML("K: hydraulic conductivity;"),
               HTML("DT: drainage time;"),
               HTML("SWI: soil water indicators;"),
               HTML("SPQI: soil physical quality indicators;"),
                      HTML(paste0("TP: total porosity (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("ME: mesoporosity (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("MA: macroporosity (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("FC: field capacity (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("PWP: permanent wilting point (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("PAW: plant available water (m",tags$sup("3") ," m",tags$sup("-3"),");")),
                      HTML(paste0("Ks: saturated hydraulic conductivity (cm d",tags$sup("-1"),");")),
                      HTML(paste0("Kr: unsaturated hydraulic conductivity (cm d",tags$sup("-1"),");")),
                      HTML("WC: water capacity (%);"),
                      HTML("AC: air capacity (%);"),
                      HTML(HTML(paste0("BD: bulk density (Mg m",tags$sup("-3"),");"))),
                      HTML("S: S-index (-)"),
                      style = "font-size: 84%;text-align:justify"),
             br(),
             checkboxInput("thresholds", "Thresholds", value = FALSE)
             
             
         )         
),


column(3,wellPanel(
  h4(""),
  
  selectInput("down", "Download data", 
              choices=c("WRC","PSD","K","DrainageTime")),
  
  downloadButton("downloadData", "Download data")
  
  
)),








verticalLayout(
  column(12,wellPanel(
    h4("Useful links"),
    
    
    actionButton(inputId='ab1', label="van Genuchten", 
                 icon = icon("th"), 
                 onclick ="window.open('http://people.ucalgary.ca/~hayashi/glgy607/reading/van_Genuchten1980.pdf', '_blank')"),
    
    actionButton(inputId='ab1', label="Indicators", 
                 icon = icon("th"), 
                 onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0016706102002288', '_blank')"),
    
    actionButton(inputId='ab1', label="Pore frequency", 
                 icon = icon("th"), 
                 onclick ="window.open('http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-90162015000200167', '_blank')"),
    
    actionButton(inputId='ab1', label="Dexter (2004)", 
                 icon = icon("th"), 
                 onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0016706103002891', '_blank')"),
    
    
    actionButton(inputId='ab1', label="Guarracino (2007)", 
                 icon = icon("th"), 
                 onclick ="window.open('https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006WR005766', '_blank')")
    
    
    
  )))),




tabPanel("About", "",
         
         
         verticalLayout(
           column(12,wellPanel(
             
             tags$p("This R app is an interactive web interface for exploring soil water retention curve using van Genuchten's model and 
             associated soil physical indicators
             and integrate the set of functions for soil physical data analysis of the R soilphysics package.", 
                    style = "font-size: 90%;text-align:justify"),
             
             
             actionButton(inputId='ab1', label="soilphysics", 
                          icon = icon("th"), 
                          onclick ="window.open('https://arsilva87.github.io/soilphysics/')"),
             
           ))),
         
         
         verticalLayout(
           column(12,wellPanel(
             tags$p("Suggestions and bug reports: renato_agro_@hotmail.com", style = "font-size: 90%;")
             
             
             
             
           ))))
    )
)
  
  
IndSoilWater_App <- function() {
  shinyApp(ui_IndSoilWater, server_IndSoilWater)
  }



