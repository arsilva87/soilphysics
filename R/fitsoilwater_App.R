server_fitsoilwater <- function(input, output, session) {
  
  # my functions
  

  
  Rsq <- function (model) 
  {
    if (!inherits(model, c("lm", "aov", "nls"))) 
      stop("'Rsq' is only applied to the classes: 'lm', 'aov' or 'nls'.")
    if (inherits(model, c("glm", "manova", "maov", 
                          "mlm"))) 
      stop("'Rsq' is not applied to an object of this class!")
    pred <- predict(model)
    n <- length(pred)
    res <- resid(model)
    w <- weights(model)
    if (is.null(w)) 
      w <- rep(1, n)
    rss <- sum(w * res^2)
    resp <- pred + res
    center <- weighted.mean(resp, w)
    if (inherits(model, c("lm", "aov"))) {
      r.df <- model$df.residual
      int.df <- attr(model$terms, "intercept")
      if (int.df) {
        mss <- sum(w * scale(pred, scale = FALSE)^2)
      }
      else {
        mss <- sum(w * scale(pred, center = FALSE, scale = FALSE)^2)
      }
      r.sq <- mss/(mss + rss)
      adj.r.sq <- 1 - (1 - r.sq) * (n - int.df)/r.df
      out <- list(R.squared = r.sq, adj.R.squared = adj.r.sq)
    }
    else {
      r.df <- summary(model)$df[2]
      int.df <- 1
      tss <- sum(w * (resp - center)^2)
      r.sq <- 1 - rss/tss
      adj.r.sq <- 1 - (1 - r.sq) * (n - int.df)/r.df
      out <- list(pseudo.R.squared = r.sq, adj.R.squared = adj.r.sq)
    }
    class(out) <- "Rsq"
    return(out)
  }
  
  
  
  
  # wrc
  
  VG <-  function(thetaS, thetaR, alpha, n, h) {
    h <- 10^h
    m <- 1-1/n
    out <- thetaR + ((thetaS-thetaR)/(1+(alpha*h)^n)^m)  
    return(out)
  }
  

  
  DE <- function (theta_R,a1,a2,h1,h2,x) theta_R + a1 * exp(-x/h1) + a2 * exp(-x/h2)
  
  BC <- function (thetaR, thetaS, lambda, hb, h) ifelse(h < hb, thetaS, thetaR + (thetaS-thetaR)*(hb/h)^lambda)
  

  DN <- function (x,thetaS,thetaR,w1,alpha1,n1,w2,alpha2,n2) {
    
    out <- thetaR + (thetaS-thetaR)*(w1*(1 + (alpha1*x)^n1)^( (1/n1) - 1) + w2*(1 + (alpha2*x)^n2)^( (1/n2) - 1))
    return(out)
    
  }
  
  GG <- function (x, k0, k1, n, x0) k1 * (exp(-k0/x0^n) - exp(-k0/x^n))
  


  
# NAVEGA 2 -------------------------------------------
  
  outdf <- NULL
  outdf <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    return(df)
  })
  
  
  output$contents <- renderPrint({
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdf()}
    RETOR
  })
  
  
  
# ABA 1 (BC, Brooks-Corey) ------------------------------
  
  
  outdfBC2 <- NULL
  outdfBC2 <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    
    updateSelectInput(session, inputId = 'xcolBC2',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolBC2',
                      choices = names(df), selected = names(df))
    return(df)
  })
  

  output$contentsBC2 <- renderPrint({
    
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdfBC2()}
    RETOR
    
  })
  
  output$plotBC2 <- renderPlot({
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(1,100000), log="x",yaxt='n',xaxt='n',
         ylab="", ylim=c(0,input$ylimBC2), type="l", lwd=2)
    mtext(expression(theta~(m^3~m^-3)), 2, line=2.3)
    mtext("h (hPa)", 1, line=2.3)
    x <- c(1,10,100,1000,10000,100000)
    axis(1,at=x, labels=c(1,10,100,1000,10000,expression(10^5)))
    axis(2)
    
    
    if (input$mBC2==FALSE) {
      mtext(expression(  theta(h)==theta[r]~"+"~(theta[s]-theta[r])(h[b]/h)^{lambda}  ),
            3,line=2)
    }
  
    
    if (input$expBC2==FALSE) {
      if (input$xcolBC2!= "" & input$ycolBC2!= "") {
        points(x=outdfBC2()[,input$xcolBC2],y=outdfBC2()[,input$ycolBC2],pch=15)
      }
    }
    
    if (input$redBC2==FALSE) {
    h <- seq(log10(1),log10(15000), len=100)
    wBC <- BC(h=10^h, thetaR=input$thetaRBC2,thetaS=input$thetaSBC2,lambda=input$lambdaBC2, hb=input$hbBC2)
    points(x=10^h, y=wBC, type="l", col="red")
    }
    
    OUT <- mySUMMARY_BC2$fittingBC2 
    if (class(OUT[[1]])=="summary.nls") {
      
      data <- OUT[[1]]$parameters[,1]
      names <- rownames(OUT[[1]]$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
      
      thetaSBC <- table$thetaS[1]
      thetaRBC <- table$thetaR[1]
      lambdaBC <- table$lambda[1]
      hbBC <- table$hb[1]

      if (input$blueBC2==FALSE) {
      hexp <- seq(from=log10(min(sort(outdfBC2()[,input$xcolBC2]))),to=log10(max(sort(outdfBC2()[,input$xcolBC2]))), len=100)
      wBC2 <- BC(h=10^hexp, thetaR=thetaRBC,thetaS=thetaSBC,lambda=lambdaBC, hb=hbBC)
      points(x=10^hexp, y=wBC2, type="l", col="blue")
      legend("topright",legend="Fitted model", lwd=1, col="blue")
      }
    

      
      data.outBC2 <- reactive({
        
        dataDOWNBC <- data.frame(h=10^hexp,w=wBC2)
        dataDOWNBC
        
      })
      

      output$downloadBC2 <- downloadHandler(
        filename = function(){"BC_curve.csv"}, 
        content = function(fname){
          write.csv(data.outBC2(), fname,row.names = FALSE)
        }
      )
      
      
      
    }
  
})
  

    
  mySUMMARY_BC2 <- reactiveValues(Data=NULL)
  mySTAT_BC2 <- reactiveValues(Data=NULL)

  
  observeEvent(input$startBC2,{

    OUT <- NULL
    h <- outdfBC2()[,input$xcolBC2]
    w <- outdfBC2()[,input$ycolBC2]
    
    lista <- c(thetaR=input$thetaRBC2,thetaS=input$thetaSBC2,lambda=input$lambdaBC2,hb=input$hbBC2)
    
    m <- try(nls(w ~ ifelse(h < hb, thetaS, thetaR + (thetaS-thetaR)*(hb/h)^lambda), start=lista,control=list(maxiter = 1000)))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- list(summary(m), m)
    mySUMMARY_BC2$fittingBC2 <- OUT


    
    if (class(m)=="nls") {
    STAT <- NULL
    res = residuals(m)
    MAPE = 100 * mean(abs(res)/(res + predict(m)))
    STAT <- format(data.frame("R2"=Rsq(m)$pseudo, AIC=AIC(m),MAPE), digits = 4)
    rownames(STAT) <- ""
    mySTAT_BC2$statBC2 <- STAT
    }
 
  })
  
  
  output$fittingBC2 <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY_BC2$fittingBC2[[1]]) || class(mySUMMARY_BC2$fittingBC2[[1]])!="summary.nls") {OUT <- matrix(nrow=1,ncol=1,data=c("Try again!"));rownames(OUT) <- "";colnames(OUT) <- ""}
    if (class(mySUMMARY_BC2$fittingBC2[[1]])=="summary.nls") {OUT <- mySUMMARY_BC2$fittingBC2[[1]]$parameters[,-3]}
    if (is.null(mySUMMARY_BC2$fittingBC2[[1]])) {OUT <- matrix(nrow=1,ncol=1,data=c("Moves the slider input for a numerical starting"));rownames(OUT) <- "";colnames(OUT) <- ""}
    OUT
    
  })
  
  
  output$statBC2 <- renderPrint({
    
    STAT <- NULL
    if (!is.null(mySUMMARY_BC2$fittingBC2[[1]]) || class(mySUMMARY_BC2$fittingBC2[[1]])!="summary.nls") {STAT <- NULL}
    if (class(mySUMMARY_BC2$fittingBC2[[1]])=="summary.nls") {STAT <- mySTAT_BC2$statBC2}
    if (is.null(mySUMMARY_BC2$fittingBC2[[1]])) {STAT <- NULL}
    STAT
    
  })
  
  
  
  
  
  
  
# ABA 2 (VG) ------------------------------
  
  
  outdfVG2 <- NULL
  outdfVG2 <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    
    updateSelectInput(session, inputId = 'xcolVG2', 
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolVG2', 
                      choices = names(df), selected = names(df))
    return(df)
  })
  
  
  output$contentsVG2 <- renderPrint({
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdfVG2()}
    RETOR
  })
  
  
  
  output$plotVG2 <- renderPlot({
    
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(1,100000), log="x",yaxt='n',xaxt='n',
         ylab="", ylim=c(0,input$ylimVG2), type="l", lwd=2)
    mtext(expression(theta~(m^3~m^-3)), 2, line=2.3)
    mtext("h (hPa)", 1, line=2.3)
    x <- c(1,10,100,1000,10000,100000)
    axis(1,at=x, labels=c(1,10,100,1000,10000,expression(10^5)))
    axis(2)
    
    
    if (input$mVG2==FALSE) {
      mtext(expression(  theta(h)==theta[r]~"+"~"["~(theta[s]-theta[r])/(1+~(alpha*h)^n)^{(1-1/n)}~"]"  ),3,line=2)
    }
    

    
    if (input$expVG2==FALSE) {
      if (input$xcolVG2!= "" & input$ycolVG2!= "") {
        points(x=outdfVG2()[,input$xcolVG2],y=outdfVG2()[,input$ycolVG2],pch=15)
      }
    }
    
    if (input$redVG2==FALSE) {
      h <- seq(0,log10(15000), len=100)
      wVG <- VG(thetaS=input$thetaSVG2, thetaR=input$thetaRVG2, 
                alpha=input$alphaVG2, n=input$nVG2, h=h)
      points(x=10^h, y=wVG, type="l", col="red")
    }
    
    

    OUT <- mySUMMARY_VG2$fittingVG2
    if (class(OUT[[1]])=="summary.nls") {
      
      data <- OUT[[1]]$parameters[,1]
      names <- rownames(OUT[[1]]$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
      

      if (length(table[1,])==4){
        thetaS <- table$thetaS[1]
        thetaR <- table$thetaR[1]
        alpha <- table$alpha[1]
        n <- table$n[1]
      }
      
      if (length(table[1,])==2){
        thetaS <- max(sort(outdfVG2()[,input$ycolVG2])) # medido
        thetaR <- min(sort(outdfVG2()[,input$ycolVG2])) # medido
        alpha <- table$alpha[1]
        n <- table$n[1]
      }
      
      if (input$blueVG2==FALSE) {
      hexp <- seq(from=log10(min(sort(outdfVG2()[,input$xcolVG2]))),to=log10(max(sort(outdfVG2()[,input$xcolVG2]))), len=100)
      wVG2 <- VG(thetaS=thetaS, thetaR=thetaR, 
                 alpha=alpha, n=n, h=hexp)
      points(x=10^hexp, y=wVG2, type="l", col="blue")
      legend("topright",legend="Fitted model", lwd=1, col="blue")
      }
      
      
      data.outVG2 <- reactive({
        
        dataDOWNVG <- data.frame(h=10^hexp,w=wVG2)
        dataDOWNVG
        
      })
      
      output$downloadVG2 <- downloadHandler(
        filename = function(){"VG_curve.csv"}, 
        content = function(fname){
          write.csv(data.outVG2(), fname,row.names = FALSE)
        }
      )
      
      
    }
    
    
  })
  
  
  
  
  
  mySUMMARY_VG2 <- reactiveValues(Data=NULL)
  mySTAT_VG2 <- reactiveValues(Data=NULL)
  
  observeEvent(input$startVG2,{
    
    OUT <- NULL
    h <- outdfVG2()[,input$xcolVG2]
    w <- outdfVG2()[,input$ycolVG2]
    
    thetaS <- max(sort(w)) # medido
    thetaR <- min(sort(w)) # medido
    theta_R <- input$thetaRVG2
    theta_S <- input$thetaSVG2
    alpha <- input$alphaVG2
    n <- input$nVG2
    
    lista <- list()
    if (input$thetaSR2==FALSE) {lista <- c(thetaR=theta_R,thetaS=theta_S,alpha=alpha, n=n)}
    if (input$thetaSR2==TRUE) {lista <- c(alpha=alpha, n=n)}
    
    m <- try(nls(w ~ thetaR + ((thetaS-thetaR)/(1+(alpha*h)^n)^(1 - 1/n)), start=lista))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- list(summary(m), m)
    mySUMMARY_VG2$fittingVG2 <- OUT
    
    
    if (class(m)=="nls") {
      STAT <- NULL
      res = residuals(m)
      MAPE = 100 * mean(abs(res)/(res + predict(m)))
      STAT <- format(data.frame("R2"=Rsq(m)$pseudo, AIC=AIC(m),MAPE), digits = 4)
      rownames(STAT) <- ""
      mySTAT_VG2$statVG2 <- STAT
    }
    
    
  })
  
  
  output$fittingVG2 <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY_VG2$fittingVG2[[1]] ) || class(mySUMMARY_VG2$fittingVG2[[1]] )!="summary.nls") {OUT <- matrix(nrow=1,ncol=1,data=c("Try again!"));rownames(OUT) <- "";colnames(OUT) <- ""}
    if (class(mySUMMARY_VG2$fittingVG2[[1]] )=="summary.nls") {OUT <- mySUMMARY_VG2$fittingVG2[[1]]$parameters[,-3]}
    if (is.null(mySUMMARY_VG2$fittingVG2[[1]] )) {OUT <- matrix(nrow=1,ncol=1,data=c("Moves the slider input for a numerical starting"));rownames(OUT) <- "";colnames(OUT) <- ""}
    OUT
    
  })
  
  
  output$statVG2 <- renderPrint({
    
    STAT <- NULL
    if (!is.null(mySUMMARY_VG2$fittingVG2[[1]] ) || class(mySUMMARY_VG2$fittingVG2[[1]] )!="summary.nls") {STAT <- NULL}
    if (class(mySUMMARY_VG2$fittingVG2[[1]] )=="summary.nls") {STAT <- mySTAT_VG2$statVG2}
    if (is.null(mySUMMARY_VG2$fittingVG2[[1]] )) {STAT <- NULL}
    STAT
    
  })
  
  
  
  
  
# ABA 3 (DN) ------------------------------
  
  outdfDN2 <- NULL
  outdfDN2 <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    
    updateSelectInput(session, inputId = 'xcolDN2', 
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolDN2', 
                      choices = names(df), selected = names(df))
    return(df)
  })
  
  
  output$contentsDN2 <- renderPrint({
    
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdfDN2()}
    RETOR

  })
  
  
  
  output$plotDN2 <- renderPlot({
    
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(1,100000), log="x",yaxt='n',xaxt='n',
         ylab="", ylim=c(0,input$ylimDN2), type="l", lwd=2)
    mtext(expression(theta~(m^3~m^-3)), 2, line=2.3)
    mtext("h (hPa)", 1, line=2.3)
    X <- c(1,10,100,1000,10000,100000)
    axis(1,at=X, labels=c(1,10,100,1000,10000,expression(10^5)))
    axis(2)
    
    
    if (input$mDN2==FALSE) {
      mtext(expression(  theta(h)==theta[r]+(theta[s]-theta[r])~"["~w[1]((1+~(alpha[1]*h)^n[1])^(1/n[1]-1))~+~w[2]((1+~(alpha[2]*h)^n[2])^(1/n[2]-1))~"]"),
            3,line=2, cex=0.8)
    }
    
    if (input$expDN2==FALSE) {
      if (input$xcolDN2!= "" & input$ycolDN2!= "") {
        points(x=outdfDN2()[,input$xcolDN2],y=outdfDN2()[,input$ycolDN2],pch=15)
      }
    }
    
    if (input$redDN2==FALSE) {
      h <- seq(log10(1),log10(15000), len=100)
      wDN <- DN(x=10^h,
                thetaS=input$thetaSDN2,
                thetaR=input$thetaRDN2,
                w1=input$w1DN2,alpha1=input$alpha1DN2,n1=input$n1DN2,
                w2=input$w2DN2,alpha2=input$alpha2DN2,n2=input$n2DN2)
      points(x=10^h, y=wDN, type="l", col="red")
    }
    
    

    
    
    OUT <- mySUMMARY_DN2$fittingDN2
    if (class(OUT[[1]])=="summary.nls") {
      
      data <- OUT[[1]]$parameters[,1]
      names <- rownames(OUT[[1]]$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
      

      if (length(table[1,])==7){
        thetaR <- table$thetaR[1]
        alpha1 <- table$alpha1[1]
        alpha2 <- table$alpha2[1]
        w1 <- table$w1[1]
        w2 <- table$w2[1]
        n1 <- table$n1[1]
        n2 <- table$n2[1]

      }
      
      if (length(table[1,])!=7){
        
        thetaR <- min(sort(outdfDN2()[,input$ycolDN2])) # medido
        alpha1 <- table$alpha1[1]
        alpha2 <- table$alpha2[1]
        w1 <- table$w1[1]
        w2 <- table$w2[1]
        n1 <- table$n1[1]
        n2 <- table$n2[1]
        
      }
      
      if (input$blueDN2==FALSE) {
      hexp <- seq(from=log10(min(sort(outdfDN2()[,input$xcolDN2]))),to=log10(max(sort(outdfDN2()[,input$xcolDN2]))), len=100)
      wDN2 <- DN(x=10^hexp,
                 thetaS=max(sort(outdfDN2()[,input$ycolDN2])), # medido
                 thetaR=thetaR,
                 w1=w1,alpha1=alpha1,n1=n1,
                 w2=w2,alpha2=alpha2,n2=n2)
      points(x=10^hexp, y=wDN2, type="l", col="blue")
      legend("topright",legend="Fitted model", lwd=1, col="blue")
      }
      
      data.outDN2 <- reactive({
        
        dataDOWNDN <- data.frame(h=10^hexp,w=wDN2)
        dataDOWNDN
        
      })
      
      output$downloadDN2 <- downloadHandler(
        filename = function(){"DN_curve.csv"}, 
        content = function(fname){
          write.csv(data.outDN2(), fname,row.names = FALSE)
        }
      )
      
      
    }
    
    
  })
  
  
  
  
  
  mySUMMARY_DN2 <- reactiveValues(Data=NULL)
  mySTAT_DN2 <- reactiveValues(Data=NULL)
  
  observeEvent(input$startDN2,{
    
    OUT <- NULL
    x <- outdfDN2()[,input$xcolDN2]
    w <- outdfDN2()[,input$ycolDN2]
    
    thetaS <- max(sort(w))  # medido
    thetaR <- min(sort(w))  # medido
    alpha1 <- input$alpha1DN2
    alpha2 <- input$alpha2DN2
    w1 <- input$w1DN2
    w2 <- input$w2DN2
    n1 <- input$n1DN2
    n2 <- input$n2DN2
    
    lista <- list()
    if (input$thetaSR_DN2==FALSE) {lista <- c(thetaR=input$thetaRDN2,w1=w1,alpha1=alpha1,n1=n1,w2=w2,alpha2=alpha2,n2=n2)}
    
    if (input$thetaSR_DN2==TRUE) {lista <- c(w1=w1,alpha1=alpha1,n1=n1,w2=w2,alpha2=alpha2,n2=n2)}
    
    
    m <- try(nls(w ~ thetaR + (thetaS-thetaR)*(w1*(1 + (alpha1*x)^n1)^( (1/n1) - 1) + w2*(1 + (alpha2*x)^n2)^( (1/n2) - 1)), start=lista))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- list(summary(m), m)
    mySUMMARY_DN2$fittingDN2 <- OUT
    
    
    if (class(m)=="nls") {
      STAT <- NULL
      res = residuals(m)
      MAPE = 100 * mean(abs(res)/(res + predict(m)))
      STAT <- format(data.frame("R2"=Rsq(m)$pseudo, AIC=AIC(m),MAPE), digits = 4)
      rownames(STAT) <- ""
      mySTAT_DN2$statDN2 <- STAT
    }
    
    
  })
  
  
  output$fittingDN2 <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY_DN2$fittingDN2[[1]]) || class(mySUMMARY_DN2$fittingDN2[[1]])!="summary.nls") {OUT <- matrix(nrow=1,ncol=1,data=c("Try again!"));rownames(OUT) <- "";colnames(OUT) <- ""}
    if (class(mySUMMARY_DN2$fittingDN2[[1]])=="summary.nls") {OUT <- mySUMMARY_DN2$fittingDN2[[1]]$parameters[,-3]}
    if (is.null(mySUMMARY_DN2$fittingDN2[[1]])) {OUT <- matrix(nrow=1,ncol=1,data=c("Moves the slider input for a numerical starting"));rownames(OUT) <- "";colnames(OUT) <- ""}
    OUT
    
  })
  
  
  output$statDN2 <- renderPrint({
    
    STAT <- NULL
    if (!is.null(mySUMMARY_DN2$fittingDN2[[1]]) || class(mySUMMARY_DN2$fittingDN2[[1]])!="summary.nls") {STAT <- NULL}
    if (class(mySUMMARY_DN2$fittingDN2[[1]])=="summary.nls") {STAT <- mySTAT_DN2$statDN2}
    if (is.null(mySUMMARY_DN2$fittingDN2[[1]])) {STAT <- NULL}
    STAT
    
  })
  
  
  

  
  
# ABA 4 (GG, Groenevelt & Grant ) ------------------------------
  
  outdfGG2 <- NULL
  outdfGG2 <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    
    updateSelectInput(session, inputId = 'xcolGG2', 
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolGG2', 
                      choices = names(df), selected = names(df))
    return(df)
  })
  
  
  output$contentsGG2 <- renderPrint({
    
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdfGG2()}
    RETOR
    

  })
  
  
  output$plotGG2 <- renderPlot({
    
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(0,7),yaxt='n',xaxt='n',
         ylab="", ylim=c(0,input$ylimGG2), type="l", lwd=2)
    mtext(expression(theta~(m^3~m^-3)), 2, line=2.3)
    mtext("pF", 1, line=2.3)
    axis(1)
    axis(2)
    
    
    if (input$mGG2==FALSE) {
      mtext(expression(  theta(pF)==k[1]~"["~exp(-k[0]/pF[0]^n)-exp(-k[0]/pF^n)~"]" ),
            3,line=2)
    }
    
    if (input$expGG2==FALSE) {
      if (input$xcolGG2!= "" & input$ycolGG2!= "") {
        points(x=outdfGG2()[,input$xcolGG2],y=outdfGG2()[,input$ycolGG2],pch=15)
      }
    }
    
    
    if (input$redGG2==FALSE) {
      h <- seq(log10(1),log10(1000000), len=100)
      wGG <- GG(x=h, k0=input$k0GG2,k1=input$k1GG2,n=input$nGG2,x0=input$x0GG2)
      points(x=h, y=wGG, type="l", col="red")
    }
    
    OUT <- mySUMMARY_GG2$fittingGG2 
    if (class(OUT[[1]])=="summary.nls") {
      
      data <- OUT[[1]]$parameters[,1]
      names <- rownames(OUT[[1]]$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
      
      k0GG <- table$k0[1]
      k1GG <- table$k1[1]
      nGG <- table$n[1]
        
      if (input$blueGG2==FALSE) {
      hexp <- seq(from=min(sort(outdfGG2()[,input$xcolGG2])),to=max(sort(outdfGG2()[,input$xcolGG2])), len=100)
      wGG2 <- GG(x=hexp, k0=k0GG,k1=k1GG,n=nGG, x0=input$x0GG2)
      points(x=hexp, y=wGG2, type="l", col="blue")
      legend("topright",legend="Fitted model", lwd=1, col="blue")
      }
      
      data.outGG2 <- reactive({
        
        dataDOWNGG <- data.frame(h=hexp,w=wGG2)
        dataDOWNGG
        
      })
      
      output$downloadGG2 <- downloadHandler(
        filename = function(){"GG_curve.csv"}, 
        content = function(fname){
          write.csv(data.outGG2(), fname,row.names = FALSE)
        }
      )
      
      
      
    }
    
    
    
    
  })
  
  
  
  mySUMMARY_GG2 <- reactiveValues(Data=NULL)
  mySTAT_GG2 <- reactiveValues(Data=NULL)
  
  observeEvent(input$startGG2,{
    
    OUT <- NULL
    x <- outdfGG2()[,input$xcolGG2]
    w <- outdfGG2()[,input$ycolGG2]
    
    lista <- c(k0=input$k0GG2,k1=input$k1GG2,n=input$nGG2)
    x0 <- input$x0GG2
    
    m <- try(nls(w ~ k1 * (exp(-k0/x0^n) - exp(-k0/x^n)), start=lista))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- list(summary(m), m)
    mySUMMARY_GG2$fittingGG2 <- OUT
    
    
    if (class(m)=="nls") {
      STAT <- NULL
      res = residuals(m)
      MAPE = 100 * mean(abs(res)/(res + predict(m)))
      STAT <- format(data.frame("R2"=Rsq(m)$pseudo, AIC=AIC(m),MAPE), digits = 4)
      rownames(STAT) <- ""
      mySTAT_GG2$statGG2 <- STAT
    }
    
    
    
  })
  
  
  output$fittingGG2 <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY_GG2$fittingGG2[[1]]) || class(mySUMMARY_GG2$fittingGG2[[1]])!="summary.nls") {OUT <- matrix(nrow=1,ncol=1,data=c("Try again!"));rownames(OUT) <- "";colnames(OUT) <- ""}
    if (class(mySUMMARY_GG2$fittingGG2[[1]])=="summary.nls") {OUT <- mySUMMARY_GG2$fittingGG2[[1]]$parameters[,-3]}
    if (is.null(mySUMMARY_GG2$fittingGG2[[1]])) {OUT <- matrix(nrow=1,ncol=1,data=c("Moves the slider input for a numerical starting"));rownames(OUT) <- "";colnames(OUT) <- ""}
    OUT
    
  })
  
  
  output$statGG2 <- renderPrint({
    
    STAT <- NULL
    if (!is.null(mySUMMARY_GG2$fittingGG2[[1]]) || class(mySUMMARY_GG2$fittingGG2[[1]])!="summary.nls") {STAT <- NULL}
    if (class(mySUMMARY_GG2$fittingGG2[[1]])=="summary.nls") {STAT <- mySTAT_GG2$statGG2}
    if (is.null(mySUMMARY_GG2$fittingGG2[[1]])) {STAT <- NULL}
    STAT
    
  })
  
  
  
  
# ABA 5 (DE) ------------------------------
  
  outdfDE2 <- NULL
  outdfDE2 <- reactive({ 
    req(input$infile)
    inFile <- input$infile
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep2)
    
    updateSelectInput(session, inputId = 'xcolDE2',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolDE2', 
                      choices = names(df), selected = names(df))
    return(df)
  })
  
  
  output$contentsDE2 <- renderPrint({
    inFile <- input$infile
    RETOR <- matrix(nrow=1,ncol=1,data=c("Awaiting input data!"));rownames(RETOR) <- "";colnames(RETOR) <- ""
    if (!is.null(inFile)) {RETOR <- outdfDE2()}
    RETOR
  

  })
  
  
  output$plotDE2 <- renderPlot({
    
    
    par(cex=0.9)
    plot(x=1,y=1,xlab="",
         xlim=c(1,100000), log="x",yaxt='n',xaxt='n',
         ylab="", ylim=c(0,input$ylimDE2), type="l", lwd=2)
    mtext(expression(italic(w)~(g~g^-1)), 2, line=2.3)
    mtext("h (hPa)", 1, line=2.3)
    X <- c(1,10,100,1000,10000,100000)
    axis(1,at=X, labels=c(1,10,100,1000,10000,expression(10^5)))
    axis(2)
    
    
    if (input$mDE2==FALSE) {
      mtext(expression(  theta(h)==C+A[1](exp(-h/h[1]))+ A[2](exp(-h/h[2])) ),
            3,line=2)
    }
    
    if (input$expDE2==FALSE) {
      if (input$xcolDE2!= "" & input$ycolDE2!= "") {
        points(x=outdfDE2()[,input$xcolDE2],y=outdfDE2()[,input$ycolDE2],pch=15)
      }
    }
    
    
    if (input$redDE2==FALSE) {
    h <- seq(0,log10(15000), len=100)
    wDE <- DE(theta_R=input$thetaRDE2,a1=input$a1DE2,a2=input$a2DE2,
            h1=input$h1_DE2,h2=input$h2_DE2,x=10^h)
    points(x=10^h, y=wDE, type="l", col="red")
    }
    
    
    OUT <- mySUMMARY_DE2$fittingDE2 
    if (class(OUT[[1]])=="summary.nls") {
      
      data <- OUT[[1]]$parameters[,1]
      names <- rownames(OUT[[1]]$parameters)
      table <- matrix(nrow=2,ncol=length(data))
      table <- as.data.frame(table)
      colnames(table) <- names
      table[1,] <- data
      

      thetaRDE <- table$C[1]
      a1 <- table$A1[1]
      h1 <- table$h1[1]
      a2 <- table$A2[1]
      h2 <- table$h2[1]
     
      if (input$blueDE2==FALSE) {
      hexp <- seq(from=log10(min(sort(outdfDE2()[,input$xcolDE2]))),to=log10(max(sort(outdfDE2()[,input$xcolDE2]))), len=100)
      w2DE <- DE(theta_R=thetaRDE,a1=a1,a2=a2,
               h1=h1,h2=h2,x=10^hexp)
      points(x=10^hexp, y=w2DE, type="l", col="blue")
      legend("topright",legend="Fitted model", lwd=1, col="blue")
      }
      
      data.outDE2 <- reactive({
        
        dataDOWNDE <- data.frame(h=10^hexp,w=w2DE)
        dataDOWNDE
        
      })
      
      output$downloadDE2 <- downloadHandler(
        filename = function(){"DE_curve.csv"}, 
        content = function(fname){
          write.csv(data.outDE2(), fname,row.names = FALSE)
        }
      )
      
      
      
    }
    
    
  })
  
  
  
  
  
  mySUMMARY_DE2 <- reactiveValues(Data=NULL)
  mySTAT_DE2 <- reactiveValues(Data=NULL)
  
  observeEvent(input$startDE2,{
    
    OUT <- NULL
    x <- outdfDE2()[,input$xcolDE2]
    w <- outdfDE2()[,input$ycolDE2]
    
    lista <- c(C=input$thetaRDE2,A1=input$a1DE2,h1=input$h1_DE2,A2=input$a2DE2,h2=input$h2_DE2)
    
    m <- try(nls(w ~  C + A1 * exp(-x/h1) + A2 * exp(-x/h2), start=lista))
    if (class(m)=="try-error") {OUT <- OUT}
    OUT <- list(summary(m), m)
    mySUMMARY_DE2$fittingDE2 <- OUT
    
    
    if (class(m)=="nls") {
      STAT <- NULL
      res = residuals(m)
      MAPE = 100 * mean(abs(res)/(res + predict(m)))
      STAT <- format(data.frame("R2"=Rsq(m)$pseudo, AIC=AIC(m),MAPE), digits = 4)
      rownames(STAT) <- ""
      mySTAT_DE2$statDE2 <- STAT
    }
    
    
    
  })
  
  
  output$fittingDE2 <- renderPrint({
    
    OUT <- NULL
    if (!is.null(mySUMMARY_DE2$fittingDE2[[1]]) || class(mySUMMARY_DE2$fittingDE2[[1]])!="summary.nls") {OUT <- matrix(nrow=1,ncol=1,data=c("Try again!"));rownames(OUT) <- "";colnames(OUT) <- ""}
    if (class(mySUMMARY_DE2$fittingDE2[[1]])=="summary.nls") {OUT <- mySUMMARY_DE2$fittingDE2[[1]]$parameters[,-3]}
    if (is.null(mySUMMARY_DE2$fittingDE2[[1]])) {OUT <- matrix(nrow=1,ncol=1,data=c("Moves the slider input for a numerical starting"));rownames(OUT) <- "";colnames(OUT) <- ""}
    OUT
    
  })
  
  
  output$statDE2 <- renderPrint({
    
    STAT <- NULL
    if (!is.null(mySUMMARY_DE2$fittingDE2[[1]]) || class(mySUMMARY_DE2$fittingDE2[[1]])!="summary.nls") {STAT <- NULL}
    if (class(mySUMMARY_DE2$fittingDE2[[1]])=="summary.nls") {STAT <- mySTAT_DE2$statDE2}
    if (is.null(mySUMMARY_DE2$fittingDE2[[1]])) {STAT <- NULL}
    STAT
    
  })
  
  
  
 # end  
 
}



ui_fitsoilwater <- fluidPage(
  
  
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
    
 "fitsoilwater",
 

navbarMenu("Choose the soil water retention model",
           
           
           
           
           tabPanel("Input file field",h4("INPUT FILE FIELD"),
                    
                    
                    
                
                      column(4,wellPanel(
                        fluidRow(
                          
                          column(6,
                                 fileInput('infile', 'Choose data (.csv or .txt)',
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv')),
                             
                                 actionLink(inputId='ab1', label="File example (.csv)", 
                                              icon = icon("th"), 
                                              onclick ="window.open('https://ce99d4d6-d4c5-48a3-b911-9e83247054ca.filesusr.com/ugd/45a659_170e68a8ee1f474b9493bc67df713eed.csv?dn=MyWRCExample.csv', '_blank')")

                                 
                          ),
                          
                          column(6,
                                 radioButtons('sep2', 'File separator',
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t'),
                                              ',')),
                          column(12,
                                 actionLink(inputId='ab1', label="File example (.txt)", 
                                            icon = icon("th"), 
                                            onclick ="window.open('https://ce99d4d6-d4c5-48a3-b911-9e83247054ca.filesusr.com/ugd/45a659_6d8a9d623811410c9a42b948f6607730.txt?dn=MyWRC.txt', '_blank')"))
                          
                          
                          
                          ))),

                    
                  
                      column(8,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                                         fluidRow(
                                           
                                           verbatimTextOutput('contents')
                                           
                                           
                                         )))
                      
                      
                      
                      
          ),
                    
                    

# NAV 1 ------------------ Brooks-Corey           
         
           tabPanel("Brooks & Corey (1964)",h4("Brooks & Corey"),
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        
                        helpText(tags$p("Fit ",tags$strong("Brooks & Corey's (BC)")," water retention curve to the experimental data. 
                                        Input your data in the",tags$strong("INPUT FILE FIELD")," and then move the sliders to find a suitable set of starting parameters. 
                                        Get the red line as close of the points as possible. Then, try clicking on ",tags$strong("Estimate")," to obtain the best (least square) 
                                        fitting. If the model did not achieve convergence, you should try again with another set of starting parameters",
                                        
                                        style = "font-size: 100%;text-align:justify"))
                        
                      ))),
                    
                  
                    

                    column(3,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                                       fluidRow(
                                         column(6,
                                                selectInput('xcolBC2', 'h', "", selected = "")),
                                         column(6,
                                                selectInput('ycolBC2', HTML(paste0("&theta;")), "", selected = "")),
                                         
                                         verbatimTextOutput('contentsBC2')
                                         
                                         
                                       ))),
                    
                    
                    
                    column(2,wellPanel(h4(tags$p("Starting parameters",style = "font-size: 85%;text-align:justify")),
                                       
                                       sliderInput("thetaSBC2", HTML(paste0("&theta;",tags$sub("s") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                                   min = 0, max = 0.8000,
                                                   step = 0.001, value=0.560,tick=FALSE),
                                       
                                       sliderInput("thetaRBC2", HTML(paste0("&theta;",tags$sub("r") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                                   min = 0, max = 0.30,
                                                   step = 0.001, value=0.150,tick=FALSE),
                                       
                                       
                                       sliderInput("lambdaBC2", HTML(paste0("&lambda;")),
                                                   min = 0.1, max = 3,
                                                   step = 0.001, value=2,tick=FALSE),
                                       
                                       sliderInput("hbBC2", HTML(paste0("h",tags$sub("b"))),
                                                   min = 1, max = 100,
                                                   step = 1, value=30,tick=FALSE)

                           
                           
                           
                           
                    )),
                    
                    
                    
                    column(4,wellPanel(
                      h4("Water retention curve"),
                      plotOutput("plotBC2"),
                      
                      helpText(
                        HTML("A blue line will appear when the model has been successfully fitted"),
                        style = "font-size: 90%;text-align:justify"),
                      
                      fluidRow(
                        column(6,
                             sliderInput("ylimBC2", HTML(paste0("&theta;",tags$sub("lim"))),
                                  min = 0, max = 1,
                                  step = 0.01, value=0.6,tick=FALSE)),
                        column(6,
                      checkboxInput("redBC2","Remove red line", FALSE),
                      checkboxInput("blueBC2","Remove blue line", FALSE),
                      checkboxInput("expBC2","Remove data", FALSE),
                      checkboxInput("mBC2","Remove equation", FALSE)))
                      
                      
                    )         
                    ),
                    
                    
                    
                    column(3,wellPanel(
                      
                      actionButton(inputId = "startBC2",label = "Estimate",class = "btn btn-primary"),
                      br(),
                      br(),
                      verbatimTextOutput("fittingBC2"),                      
                      verbatimTextOutput("statBC2"),
                      helpText(
                        HTML(paste0("R",tags$sup("2"),": coefficient of determination;")),
                        HTML("AIC: Akaike Information Criterion;"),
                        HTML("MAPE: mean absolute percentage error"),
                        style = "font-size: 70%;text-align:justify"),
                      br(),
                      downloadButton("downloadBC2", "Download fitted data",class = "btn btn-primary")
                      
                    )),
                    
                    
                    
                    verticalLayout(
                      column(12,wellPanel(
                        h4("Useful links"),
                        
                        actionButton(inputId='ab1', label="Brooks & Corey (1964)", 
                                     icon = icon("th"), 
                                     onclick ="window.open('https://mountainscholar.org/bitstream/handle/10217/61288/HydrologyPapers_n3.pdf', '_blank')"),
                        
                        
                        actionButton(inputId='ab1', label="Pressure unit converter", 
                                     icon = icon("th"), 
                                     onclick ="window.open('http://www.unitconversion.org/unit_converter/pressure.html', '_blank')")
                        
                        
                      )))),
           

           
           
# NAVI 2 ------------------ van Genuchten
           
tabPanel("van Genuchten (1980)",h4("van Genuchten"),
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("Fit ",tags$strong("van Genuchten's (VG)")," water retention curve to the experimental data. 
                                        Input your data pairs in the ",tags$strong("INPUT FILE FIELD")," and then move the sliders to find a suitable set of starting parameters. 
                                        Get the red line as close of the points as possible. Then, try clicking on ",tags$strong("Estimate")," to obtain the best (least square) 
                                        fitting. If the model did not achieve convergence, you should try again with another set of starting parameters",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         
         
         
         column(3,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                            fluidRow(
                              column(6,
                                     selectInput('xcolVG2', 'h', "", selected = "")),
                              column(6,
                                     selectInput('ycolVG2', HTML(paste0("&theta;")), "", selected = "")),
                              
                              verbatimTextOutput('contentsVG2')
                              
                              
                            ))),
         
         

         column(2,wellPanel(h4(tags$p("Starting parameters",style = "font-size: 85%;text-align:justify")),
                            
                            
                            sliderInput("thetaSVG2", HTML(paste0("&theta;",tags$sub("s") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                        min = 0, max = 0.8000,
                                        step = 0.001, value=0.560,tick=FALSE),
                            
                            sliderInput("thetaRVG2", HTML(paste0("&theta;",tags$sub("r") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                        min = 0, max = 0.30,
                                        step = 0.001, value=0.150,tick=FALSE),
                            
                            sliderInput("alphaVG2", HTML(paste0("&alpha; (hPa",tags$sup("-1"),")")),
                                        min = 0, max = 0.20,
                                        step = 0.001, value=0.026,tick=FALSE),
                            
                            sliderInput("nVG2", "n",
                                        min = 1, max = 4,
                                        value = 2.145, step = 0.001,tick=FALSE),
                            br(),
                
                            checkboxInput("thetaSR2", "", value = FALSE),
                
                            helpText(
                              HTML(paste0("Check this box to consider ","&theta;",tags$sub("s")," and ","&theta;",tags$sub("r"),
                                          " from the experimental data (the algorithm will take the minimum and maximum values of water content).",
                                          " Only the parameters n and ","&alpha;"," will be estimated")),
                              style = "font-size: 90%;text-align:justify"))
                
                
 
                
                
         ),
         
         

         column(4,wellPanel(
           h4("Water retention curve"),
           plotOutput("plotVG2"),
           
           helpText(
             HTML("A blue line will appear when the model has been successfully fitted"),
             style = "font-size: 90%;text-align:justify"),
           fluidRow(
             column(6,
                  sliderInput("ylimVG2", HTML(paste0("&theta;",tags$sub("lim"))),
                              min = 0, max = 1,
                              step = 0.01, value=0.6,tick=FALSE)),
             column(6,
           checkboxInput("redVG2","Remove red line", FALSE),
           checkboxInput("blueVG2","Remove blue line", FALSE),
           checkboxInput("expVG2","Remove data", FALSE),
           checkboxInput("mVG2","Remove equation", FALSE)))
           
           
         )         
  ),
  
  
  
  column(3,wellPanel(
    
    actionButton(inputId = "startVG2",label = "Estimate",class = "btn btn-primary"),
    br(),
    br(),
    verbatimTextOutput("fittingVG2"),
    verbatimTextOutput("statVG2"),
    helpText(
      HTML(paste0("R",tags$sup("2"),": coefficient of determination;")),
      HTML("AIC: Akaike Information Criterion;"),
      HTML("MAPE: mean absolute percentage error"),
      style = "font-size: 70%;text-align:justify"),
    br(),
    downloadButton("downloadVG2", "Download fitted data",class = "btn btn-primary")
    
  )),
  
         

         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             actionButton(inputId='ab1', label="Genuchten (1980)", 
                          icon = icon("th"), 
                          onclick ="window.open('http://people.ucalgary.ca/~hayashi/glgy607/reading/van_Genuchten1980.pdf', '_blank')"),
             
             
             actionButton(inputId='ab1', label="Pressure unit converter", 
                          icon = icon("th"), 
                          onclick ="window.open('http://www.unitconversion.org/unit_converter/pressure.html', '_blank')")

             
           )))),








# NAVI 3 ------------------ Durner

tabPanel("Durner (1994) (Bimodal)",h4("Durner"),
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("Fit ",tags$strong("Durner's (DN)")," water retention curve to the experimental data. 
                                        Input your data pairs in the ",tags$strong("INPUT FILE FIELD")," and then move the sliders to find a suitable set of starting parameters. 
                                        Get the red line as close of the points as possible. Then, try clicking on ",tags$strong("Estimate")," to obtain the best (least square) 
                                        fitting. If the model did not achieve convergence, you should try again with another set of starting parameters",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         

         
         
         column(3,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                            fluidRow(
                              column(6,
                                     selectInput('xcolDN2', 'h', "", selected = "")),
                              column(6,
                                     selectInput('ycolDN2', HTML(paste0("&theta;")), "", selected = "")),
                              verbatimTextOutput('contentsDN2')
                              
                              
                            ))),

         
         
         column(2,wellPanel(h4(tags$p("Starting parameters",style = "font-size: 85%;text-align:justify")),
                            
                            
                            sliderInput("thetaSDN2", HTML(paste0("&theta;",tags$sub("s") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                        min = 0, max = 0.80,
                                        step = 0.001, value=0.60,tick=FALSE),
    
                            sliderInput("thetaRDN2", HTML(paste0("&theta;",tags$sub("r") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                                        min = 0, max = 0.30,
                                        step = 0.001, value=0.10,tick=FALSE),
                            
                            sliderInput("w1DN2", HTML(paste0("w",tags$sub("1"))),
                                        min = 0, max = 1,
                                        step = 0.001, value=0.75,tick=FALSE),
                            
                            sliderInput("alpha1DN2", HTML(paste0("&alpha;",tags$sub("1")," (hPa",tags$sup("-1"),")")),
                                        min = 0, max = 0.20,
                                        step = 0.001, value=0.019,tick=FALSE),
                            
                            sliderInput("n1DN2", HTML(paste0("n",tags$sub("1"),"")),
                                        min = 1, max = 10,
                                        step = 0.001,value = 3.98,tick=FALSE),
                            
                            
                            sliderInput("w2DN2", HTML(paste0("w",tags$sub("2"))),
                                        min = 0, max = 1,
                                        step = 0.001, value=0.23,tick=FALSE),
                            
                            sliderInput("alpha2DN2", HTML(paste0("&alpha;",tags$sub("2")," (hPa",tags$sup("-1"),")")),
                                        min = 0, max = 0.02,
                                        step = 0.00001, value=0.0005,tick=FALSE),
                            
                            sliderInput("n2DN2", HTML(paste0("n",tags$sub("2"),"")),
                                        min = 1, max = 10,
                                        step = 0.001,value = 5.05,tick=FALSE),
                            
                            br(),
                            
                            checkboxInput("thetaSR_DN2", "", value = FALSE),
                            
                            helpText(
                              HTML(paste0("Check this box to consider ","&theta;",tags$sub("s")," and ","&theta;",tags$sub("r"),
                                          " from the experimental data (the algorithm will take the minimum and maximum values of water content)")),
                              style = "font-size: 90%;text-align:justify"))
                
                
                
                
                
                
         ),
         
         
         
         column(4,wellPanel(
           h4("Water retention curve"),
           plotOutput("plotDN2"),
           
           helpText(
             HTML(paste0("WARNING! To facilitate the fitting, the algorithm considers ","&theta;",tags$sub("s")," from the measured data (maximum water content value).")),
             HTML("A blue line will appear when the model has been successfully fitted"),
             style = "font-size: 93%;text-align:justify"),
           fluidRow(
             column(6,
                  sliderInput("ylimDN2", HTML(paste0("&theta;",tags$sub("lim"))),
                              min = 0, max = 1,
                              step = 0.01, value=0.6,tick=FALSE)),
             column(6,
           checkboxInput("redDN2","Remove red line", FALSE),
           checkboxInput("blueDN2","Remove blue line", FALSE),
           checkboxInput("expDN2","Remove data", FALSE),
           checkboxInput("mDN2","Remove equation", FALSE)))
           
           
         )         
         ),
         
         
         
         column(3,wellPanel(
           
           actionButton(inputId = "startDN2",label = "Estimate",class = "btn btn-primary"),
           br(),
           br(),
           verbatimTextOutput("fittingDN2"),
           verbatimTextOutput("statDN2"),
           helpText(
             HTML(paste0("R",tags$sup("2"),": coefficient of determination;")),
             HTML("AIC: Akaike Information Criterion;"),
             HTML("MAPE: mean absolute percentage error"),
             style = "font-size: 70%;text-align:justify"),
             br(),
           downloadButton("downloadDN2", "Download fitted data",class = "btn btn-primary")
           
         )),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             actionButton(inputId='ab1', label="Durner (1994)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://agupubs.onlinelibrary.wiley.com/doi/10.1029/93WR02676', '_blank')"),
             
             
             actionButton(inputId='ab1', label="Pressure unit converter", 
                          icon = icon("th"), 
                          onclick ="window.open('http://www.unitconversion.org/unit_converter/pressure.html', '_blank')")
             
             
           )))),








# NAVI 4 ------------------  Groenevelt & Grant

tabPanel("Groenevelt & Grant (2004)", h4("Groenevelt & Grant"),
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("Fit ",tags$strong("Groenevelt & Grant's (GG)")," water retention curve to the experimental data. 
                                        Input your data pairs in the ",tags$strong("INPUT FILE FIELD")," and then move the sliders to find a suitable set of starting parameters. 
                                        Get the red line as close of the points as possible. Then, try clicking on ",tags$strong("Estimate")," to obtain the best (least square) 
                                        fitting. If the model did not achieve convergence, you should try again with another set of starting parameters",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         
         
         
         column(3,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                            fluidRow(
                              column(6,
                                     selectInput('xcolGG2', 'h', "", selected = "")),
                              column(6,
                                     selectInput('ycolGG2', HTML(paste0("&theta;")), "", selected = "")),
                              
                              verbatimTextOutput('contentsGG2')
                              
                              
                            ))),
         
         
         column(2,wellPanel(h4(tags$p("Starting parameters",style = "font-size: 85%;text-align:justify")),
                            
                            
                            sliderInput("k0GG2", HTML(paste0("k",tags$sub("0"))),
                                        min = 1, max = 20,
                                        step = 0.001, value=5.2,tick=FALSE),
                            
                            sliderInput("k1GG2", HTML(paste0("k",tags$sub("1"))),
                                        min = 0, max = 1,
                                        step = 0.001, value=0.55,tick=FALSE),
              
                            sliderInput("nGG2", "n",
                                        min = 1, max = 10,
                                        step = 0.001,value = 2.3,tick=FALSE),
                            
                            numericInput("x0GG2", HTML(paste0("pF",tags$sub("0"))),
                                        min = 4, max = 8,
                                        step = 0.1,value = 6.9),
                
                
                helpText(
                  HTML(paste0("pF",tags$sub("0")," represents the pF to anchor the ", 
                              "&theta; ","= 0,"," and it was suggested by Groenevelt & Grant (2004) as 6.9 (default)")),
                  style = "font-size: 83%;text-align:justify"))
         
                
                
         ),
         
         
         
         column(4,wellPanel(
           h4("Water retention curve"),
           plotOutput("plotGG2"),
           
           helpText(
             HTML("A blue line will appear when the model has been successfully fitted"),
             style = "font-size: 90%;text-align:justify"),
           fluidRow(
             column(6,
                  sliderInput("ylimGG2", HTML(paste0("&theta;",tags$sub("lim"))),
                              min = 0, max = 1,
                              step = 0.01, value=0.6,tick=FALSE)),
             column(6,
           checkboxInput("redGG2","Remove red line", FALSE),
           checkboxInput("blueGG2","Remove blue line", FALSE),
           checkboxInput("expGG2","Remove data", FALSE),
           checkboxInput("mGG2","Remove equation", FALSE)))
           
           
         )         
         ),
         
         
         
         column(3,wellPanel(
           
           actionButton(inputId = "startGG2",label = "Estimate",class = "btn btn-primary"),
           br(),
           br(),
           verbatimTextOutput("fittingGG2"),
           verbatimTextOutput("statGG2"),
           helpText(
             HTML(paste0("R",tags$sup("2"),": coefficient of determination;")),
             HTML("AIC: Akaike Information Criterion;"),
             HTML("MAPE: mean absolute percentage error"),
             style = "font-size: 70%;text-align:justify"),
             br(),
           downloadButton("downloadGG2", "Download fitted data",class = "btn btn-primary")
           
         )),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             actionButton(inputId='ab1', label="Groenevelt & Grant (2004)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2389.2004.00617.x', '_blank')"),
             
             
             actionButton(inputId='ab1', label="Pressure unit converter", 
                          icon = icon("th"), 
                          onclick ="window.open('http://www.unitconversion.org/unit_converter/pressure.html', '_blank')")
             
             
           )))),





# NAVI 5 ------------------ Dexter curve 

tabPanel("Dexter et al. (2008) (Bimodal)", h4("Dexter"),
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("Fit ",tags$strong("Dexter's (DE)")," water retention curve to the experimental data. 
                                        Input your data pairs in the ",tags$strong("INPUT FILE FIELD"),"and then move the sliders to find a suitable set of starting parameters. 
                                        Get the red line as close of the points as possible. Then, try clicking on ",tags$strong("Estimate")," to obtain the best (least square) 
                                        fitting. If the model did not achieve convergence, you should try again with another set of starting parameters",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         

         
         
         column(3,wellPanel(h4(tags$p("DATA",style = "font-size: 80%;text-align:justify")),
                            fluidRow(
                              column(6,
                                     selectInput('xcolDE2', 'h', "", selected = "")),
                              column(6,
                                     selectInput('ycolDE2', HTML(paste0("&theta;")), "", selected = "")),
                              
                              verbatimTextOutput('contentsDE2')
                              
                              
                            ))),
         
         
         
         
         column(2,wellPanel(h4(tags$p("Starting parameters",style = "font-size: 85%;text-align:justify")),
                            
                            
                            sliderInput("thetaRDE2",HTML(paste0("C (g",tags$sup("") ," g",tags$sup("-1"),")")),
                                        min = 0.01, max = 0.30,
                                        step = 0.0001, value=0.14,tick=FALSE),
                            
                            
                            sliderInput("a1DE2", HTML(paste0("A",tags$sub("1") ," (g",tags$sup("") ," g",tags$sup("-1"),")")),
                                        min = 0.01, max = 0.40,
                                        step = 0.0001, value=0.07,tick=FALSE),
                            
       
                            sliderInput("h1_DE2", HTML(paste0("h",tags$sub("1")," (hPa)")),
                                        min = 500, max = 10000,
                                        step = 1, value=4270,tick=FALSE),
                            
                            sliderInput("a2DE2", HTML(paste0("A",tags$sub("2") ," (g",tags$sup("") ," g",tags$sup("-1"),")")),
                                        min = 0.01, max = 0.50,
                                        step = 0.0001, value=0.33,tick=FALSE),
                            
                            sliderInput("h2_DE2", HTML(paste0("h",tags$sub("2")," (hPa)")),
                                        min = 10, max = 2000,
                                        step = 1, value=70,tick=FALSE),
                            helpText(
                              HTML("In the Dexter's water retention curve, the C parameter corresponds to the residual water content"),
                              style = "font-size: 70%;text-align:justify"))

                
         ),
         
         
         
         column(4,wellPanel(
           h4("Water retention curve"),
           plotOutput("plotDE2"),
           
           helpText(
             HTML("A blue line will appear when the model has been successfully fitted"),
             style = "font-size: 90%;text-align:justify"),
           fluidRow(
             column(6,
                  sliderInput("ylimDE2", HTML(paste0("&theta;",tags$sub("lim"))),
                              min = 0, max = 1,
                              step = 0.01, value=0.6,tick=FALSE)),
             column(6,
           checkboxInput("redDE2","Remove red line", FALSE),
           checkboxInput("blueDE2","Remove blue line", FALSE),
           checkboxInput("expDE2","Remove data", FALSE),
           checkboxInput("mDE2","Remove equation", FALSE)))
           
         )         
         ),
         
         
         
         column(3,wellPanel(
           
           actionButton(inputId = "startDE2",label = "Estimate",class = "btn btn-primary"),
           br(),
           br(),
           verbatimTextOutput("fittingDE2"),
           verbatimTextOutput("statDE2"),
           helpText(
             HTML(paste0("R",tags$sup("2"),": coefficient of determination;")),
             HTML("AIC: Akaike Information Criterion;"),
             HTML("MAPE: mean absolute percentage error"),
             style = "font-size: 70%;text-align:justify"),
             br(),
           downloadButton("downloadDE2", "Download fitted data",class = "btn btn-primary")
           
         )),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             actionButton(inputId='ab1', label="Dexter et al. (2008)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/abs/pii/S0016706107003229', '_blank')"),
             
             
             actionButton(inputId='ab1', label="Pressure unit converter", 
                          icon = icon("th"), 
                          onclick ="window.open('http://www.unitconversion.org/unit_converter/pressure.html', '_blank')")
             
             
           ))))),



tabPanel("About", "",
         
         
         verticalLayout(
           column(12,wellPanel(
             
             tags$p("This R app is an interactive web interface for fitting soil water retention models
             and integrate the set of functions for soil physical data 
                    analysis from the R package ",tags$em(tags$strong("soilphysics")),"", 
                    style = "font-size: 115%;text-align:justify"),
             
             
             actionButton(inputId='ab1', label="soilphysics", 
                          icon = icon("th"), 
                          onclick ="window.open('https://arsilva87.github.io/soilphysics/')"),
             
           ))),
         
         verticalLayout(
           column(12,wellPanel(
             tags$p("Developed by: Renato P. de Lima & Anderson R. da Silva", style = "font-size: 90%;")))),
         
         
         verticalLayout(
           column(12,wellPanel(
             tags$p("Suggestions and bug reports: renato_agro_@hotmail.com; anderson.silva@ifgoiano.edu.br", style = "font-size: 90%;")
             
             
             
             
           ))))

)



)


fitsoilwater_App <- function() {
  shinyApp(ui_fitsoilwater,server_fitsoilwater)
  }




