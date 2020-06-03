server <- function(input, output) {
  
  # my function 
  part.sedimen <- function(d, h, g=9.81, v, Pd, Wd) {
    
    v <- v*10^-3
    d <- d*10^-6
    h <- h/100
    out0 <- (h*18*v)/( (d^2)*(Pd-Wd)*g)
    out1 <- out0/60
    out2 <- out1/60
    out3 <- out2/24
    rest <- data.frame(out0,out1,out2,out3)
    return(rest)
  }

  
  
 output$plot1 <- renderPlot({
   
    y <- part.sedimen(d=input$d, h=input$h, g=9.81, 
                      v=input$v, Pd=input$Pd, Wd=input$Wd)$out2
    x <- input$d
    lab <- c(1000,100,50,10,2,1)
    
    m <- function (x) 9.592e-01 + 2.041e-02*x
    tam <- m(input$d)

    plot(x=x,y=y,log="x",xaxt='n', pch=19,cex=tam, 
        col="brown", xlim=c(1,1000), ylim=c(0,30),xlab="",ylab="")
    axis(1,at=lab, labels=lab)
    mtext(expression("Soil particle diameter (d)"~(mu*m)),1,line=2.5)
    mtext("Sedimentation time (t) (hour)",2,line=2.5)
    text("soil particle",x=x,y=y+2, cex=0.9)
    abline(v=c(2,50), col=2)
    axis(3,at=c(1.2,8,300), labels=c("clay","silt","sand"))
  })
 
 

 output$values <- renderTable({
   
   y <- part.sedimen(d=input$d, h=input$h, g=9.81, 
                     v=input$v, Pd=input$Pd, Wd=input$Wd)
   x <- input$d
   
   data <- data.frame(Diameter=x,Seconds=y$out0,Minutes=y$out1,Hours=y$out2,Days=y$out3)
   
 })
 
 
}


# ==============================================
ui <- fluidPage(
  # App title ----
  titlePanel("Sedimentation time of soil particle (Stokes' law)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("d", HTML(paste0(" Soil particle diameter (&mu;m)")),
                  min = 2, max = 100,
                  step = 1, value=100, dragRange=TRUE),
  
      sliderInput("v", HTML(paste0("Fluid viscosity 10",tags$sup("-3"), "(kg m",tags$sup("-1")," s",tags$sup("-1"),")")),
                  min = 0.5, max = 1.5,
                  value = 1, step = 0.1),
      

      sliderInput("h", 'Fall height of soil particle (cm)',
                  min = 5, max = 25,
                  value = 20, step = 5),
      
      sliderInput("Pd", HTML(paste0("Particle density (kg m",tags$sup("-3"),")")),
                  min = 2400, max = 3000,
                  value = 2650, step = 50),
      
      sliderInput("Wd", HTML(paste0("Fluid density (kg m",tags$sup("-3"),")")),
                  min = 950, max = 1050,
                  value = 1000, step = 10)
      
    ),   
    # Main panel for displaying outputs ----
    mainPanel(      
      plotOutput('plot1'),
      tableOutput("values"))
  ),
  verticalLayout(
    column(12,wellPanel(
      tags$h4("by Renato P. de Lima")
    )))
)

particle.sedimentation_App <- function() {
  shinyApp(ui, server)
  }
