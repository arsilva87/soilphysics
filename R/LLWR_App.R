
server_LLWR <- function(input, output) {
  
   llwr <- function(Bd, air, critical.PR, h.FC, h.WP,p.density,a,b,c,d,e,f) {
      thetaAIR <- c()
      thetaCC <- c()
      thetaWP <- c()
      thetaPR <- c()
      for (j in 1:length(Bd)) {
         thetaAIR[j] <- (1 - (Bd[j]/p.density)) - air
         thetaCC[j] <- exp(a+b*Bd[j])*h.FC^(c)
         thetaWP[j] <- exp(a+b*Bd[j])*h.WP^(c)
         thetaPR[j] <- (critical.PR/(d*Bd[j]^f))^(1/e)
      }
      SL <- c()
      IL <- c()
      for (j in 1:length(Bd)) {
         if (thetaAIR[j] < thetaCC[j]) {
            SL[j] <- thetaAIR[j]
         }
         else if (thetaAIR[j] > thetaCC[j]) {
            SL[j] <- thetaCC[j]
         }
         if (thetaWP[j] > thetaPR[j]) {
            IL[j] <- thetaWP[j]
         }
         else if (thetaWP[j] < thetaPR[j]) {
            IL[j] <- thetaPR[j]
         }
      }
      LLWR<- (SL - IL)
      for (j in 1:length(LLWR)) {
         if (LLWR[j] < 0) {LLWR[j] <- 0}
      }
      IHO <- data.frame(BD=Bd,AIR=thetaAIR,FC=thetaCC,WP=thetaWP,PR=thetaPR,LS=SL,LI=IL,LLWR=LLWR)
      return(IHO)
   }
   
   

  
 # GRAFICO LLWR
  
 output$plot1 <- renderPlot({
    
   
    BD <- seq(min(input$BD),max(input$BD), len=500)
    
    IHO <- llwr(air=input$air,critical.PR=input$pr, h.FC=input$fc, h.WP=input$pwp,
                p.density=input$PD,
                Bd=BD, a=input$a, b=input$b, c=input$c, 
                d=input$d, e=input$e, f=input$f)
    
    
    cex <- 0.9
    plot(y=1,x=1, xlim=c(1,2),
         ylim=c(0,0.6), type="l",lwd=2, ylab="", xlab="",col="darkgray")
    
    mtext(expression(italic(theta)~(m^3~m^{-3})), 
          side = 2, line = 2.2, cex=cex, las=0)
    mtext(expression("Bulk density"~(Mg~m^{-3})), side = 1, line = 2.8, cex=cex)
    
    points(y=IHO$FC,x=BD, col="black",type="l",lwd=2)
    points(y=IHO$WP,x=BD, col="blue",type="l",lwd=2)
    points(y=IHO$PR,x=BD, col="red",type="l",lwd=2)
    points(y=IHO$AIR,x=BD, col="gray",type="l",lwd=2)
    
    LLWR.critical <- subset(x=IHO, subset= LLWR > 0)
    
    
    x1 <- LLWR.critical$BD 
    y1 <- LLWR.critical$LI
    LOW <- cbind(x1, y1)
    o <- order(x1, y1)
    LOW <- LOW[o, ]
    x1 <- LOW[,1]
    y1 <- LOW[,2]
    
    x2 <- LLWR.critical$BD
    y2 <- LLWR.critical$LS
    UP <- cbind(x2, y2)
    o <- order(x2, y2, decreasing = T)
    UP <- UP[o, ]
    x2 <- UP[,1]
    y2 <- UP[,2]
    
    polygon(x=c(x1,x2), y = c(y1,y2),
            col="gray90", border = T)
    
    
    
    
    legend(x="topright",                             
           legend=c(expression(theta[AFP]),expression(theta[FC]),
                    expression(theta[PR]),expression(theta[PWP])),  
           lty=c(1,1,1,1),                               
           col=c("gray","black","red","blue"),                                 
           lwd=c(2,2,2,2),                                               
           bty="o",                                    
           pch=c(NA,NA,NA,NA), cex=cex)
    
    
    points(y=IHO$FC,x=BD, col="black",type="l",lwd=2)
    points(y=IHO$WP,x=BD, col="blue",type="l",lwd=2)
    points(y=IHO$PR,x=BD, col="red",type="l",lwd=2)
    points(y=IHO$AIR,x=BD, col="gray",type="l",lwd=2)
    box(lwd=2)
   
  })
 
 
 output$plot2 <- renderPlot({
   
   
   BD <- seq(min(input$BD),max(input$BD), 0.01)
   
   IHO <- llwr(air=input$air,critical.PR=input$pr, h.FC=input$fc, h.WP=input$pwp,
               p.density=input$PD,
               Bd=BD, a=input$a, b=input$b, c=input$c, 
               d=input$d, e=input$e, f=input$f)
   
   
   cex <- 0.9
   plot(y=1,x=1, xlim=c(1,2),
        ylim=c(0,0.3), type="l",lwd=2, ylab="", xlab="",col="darkgray")
   
   mtext(expression(LLWR~(m^3~m^{-3})), 
         side = 2, line = 2.2, cex=cex, las=0)
   mtext(expression("Bulk density"~(Mg~m^{-3})), side = 1, line = 2.8, cex=cex)
   
   points(y=IHO$LLWR,x=IHO$BD, col="black",type="l",lwd=2)
   
   LLWR.critical <- subset(x=IHO, subset= LLWR <= 0)
   
   BD.cri <- round(LLWR.critical[1,1],2)
   
   legend("topright", 
          legend=c(expression(BD[critical]),BD.cri))
 
 
 })
 
 
 output$values <- renderTable({
 
   BD <- seq(min(input$BD),max(input$BD), 0.01)
   
   IHO <- llwr(air=input$air,critical.PR=input$pr, h.FC=input$fc, h.WP=input$pwp,
               p.density=input$PD,
               Bd=BD, a=input$a, b=input$b, c=input$c, 
               d=input$d, e=input$e, f=input$f)
   
   out <- data.frame(BD=IHO$BD, AFP=IHO$AIR,FC=IHO$FC,
                  PWP=IHO$WP,PR=IHO$PR,LLWR=IHO$LLWR)

   out
   
 })
 

 data.out <- reactive({
   
   
   BD <- seq(min(input$BD),max(input$BD), 0.01)
   
   IHO <- llwr(air=input$air,critical.PR=input$pr, h.FC=input$fc, h.WP=input$pwp,
               p.density=input$PD,
               Bd=BD, a=input$a, b=input$b, c=input$c, 
               d=input$d, e=input$e, f=input$f)
   
   out <- data.frame(BD=IHO$BD, AFP=IHO$AIR,FC=IHO$FC,
                     PWP=IHO$WP,PR=IHO$PR,LLWR=IHO$LLWR)
   
   out
   
 })
 
 
 
 
 output$downloadData <- downloadHandler(
   filename = function(){"LLWR.csv"}, 
   content = function(fname){
     write.csv(data.out(), fname,row.names = FALSE)
   }
 )
  
}



# ui-----------

ui_LLWR <- fluidPage(
  
  titlePanel("Least Limiting Water Range (LLWR)"),
  
    column(3,wellPanel(
      h4("SWR and SPR curves "),
      
      
      sliderInput("BD", HTML(paste0("Bulk density (Mg m",tags$sup("-3"),")")),
                  min = 1, max = 2,
                  step = 0.01, value=c(1.2,1.6), tick=FALSE),
      
      sliderInput("a", "a",
                  min = -1, max = 1,
                  step = 0.0001, value=-0.9396, tick=FALSE),
      
      
      sliderInput("b", "b (Bulk density effect)",
                  min = -0.2, max = 0.6,
                  value = 0.28, step = 0.0001,tick=FALSE),
      

      sliderInput("c", 'c (Water content effect)',
                  min = -0.2, max = -0.01,
                  value = -0.100, step = 0.0001,tick=FALSE),
      
      sliderInput("d", 'd ',
                  min = 0.1, max = 0.5,
                  value = 0.1112, step = 0.0001,tick=FALSE),
      
      sliderInput("e", 'e (Water content effect)',
                  min = -2, max = -1,
                  value = -1.1512, step = 0.0001,tick=FALSE),
      
      sliderInput("f", 'f (Bulk density effect)',
                  min = 1, max = 8,
                  value = 4.5, step = 0.01,tick=FALSE),
      
      
      helpText(tags$p("Move the slider input for assigning the water (a, b and c; da Silva's parameters) and penetration resistance (d, e and f; Busscher's parameters) 
                        curves parameters. See de Lima et al. (2016)",
                      style = "font-size: 92%;text-align:justify"))
    )),
      

  column(3,wellPanel(
    h4("Restriction thresholds"),
    
    
    sliderInput("air",  HTML(paste0("AFP (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                min = 0.05, max = 0.15,
                step = 0.01, value=0.10, tick=FALSE),
    
    
    sliderInput("fc", "FC (hPa)",
                min = 60, max = 330,
                value = 100, step = 1,tick=FALSE),
    
    
    sliderInput("pr", 'SPR (MPa)',
                min = 1, max = 5,
                value = 2.5, step = 0.1,tick=FALSE),
    
    sliderInput("pwp", 'PWP (hPa)',
                min = 10000, max = 20000,
                value = 15000, step = 100,tick=FALSE),
    

    sliderInput("PD", HTML(paste0("Particle density (Mg m",tags$sup("-3"),")")),
                min = 2.4, max = 2.8,
                value = 2.65, step = 0.01,tick=FALSE),
    
    helpText(tags$p("LEGEND: AFP: air-filled porosity; FC: field capacity; PWP: permanent wilting point; SPR: soil penetration resistance",
                    style = "font-size: 80%;text-align:justify"))
    
  )),
  
 downloadButton("downloadData", "Download data"),
 
  column(4, wellPanel(
    h4("Least limiting water range"),
    tabsetPanel(type = "tabs",
                tabPanel("Limits", plotOutput("plot1")),
                tabPanel("LLWR", plotOutput("plot2")),
                tabPanel("Data", tableOutput("values")))

                
  )         
  ),
  
  
 verticalLayout(
 column(12,wellPanel(
   h4("Related links [See Lima et al. (2016) for the parameters of SWR and SPR curves]"),

   actionButton(inputId='ab1', label="Lima et al. (2016)", 
                icon = icon("th"), 
                onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0168169915003403?via%3Dihub')"),
   

   actionButton(inputId='ab1', label="Leao and Silva (2005)", 
                icon = icon("th"), 
                onclick ="window.open('http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-90162004000600013')")
  

 ))),
 
 verticalLayout(
   column(12,wellPanel(
     h4("by Renato P. de Lima")
     
     
   )))
                
)


LLWR_App <- function() {
  shinyApp(ui_LLWR , server_LLWR)
  }
  
