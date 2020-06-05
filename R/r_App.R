
ui_r <- fluidPage(
  
  
  # App title ----
  titlePanel("Pores that drain for a corresponding applied matric suction"),

    sidebarPanel(
      
      
      # -------
      sliderInput("h", 'Soil matric suction (cm)',
                  min = 1, max = 300,
                  value = 1, step = 1)

      # -------
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput('plot1'),tableOutput("values")),

  verticalLayout(
    column(12,wellPanel(
      h4("by Renato P. de Lima"),
    )))
  
)




server_r <- function(input, output) {
  r <- function(h) {
    h <- h/100
    out <- (2*0.072*1)/(1000*9.81*h)
    out <- out*1000000
    return(out)
  }

 output$plot1 <- renderPlot({ 
    y <- r(h=input$h)
    x <- input$h
    lab <- c(1,10,100,1000)
    plot(x=x,y=y,log="yx",xaxt='n', pch=19, cex=1,  yaxt='n',
        col="brown", xlim=c(1,1000), ylim=c(1,1500),xlab="",ylab="")
    axis(1,at=lab, labels=lab)
    axis(2,at=lab, labels=lab)
    mtext(expression("soil pore radius (r)"~(mu*m)),2,line=2.5)
    mtext("matric suction (h) (cm)",1,line=2.5) 
    x1 <- seq(input$h,300,length=1000)
    y1 <- r(x1)
    polygon(c(x1,rev(x1)),c(rep(1,length(x1)), rev(y1)),col="skyblue")
    axis(3,at=c(4,55,300),labels=c("macropores","mesopores","micropores"))
    axis(1,at=c(30))
    abline(v=c(30,100),col=2)
    points(x=x,y=y, pch=19, col=2)
  })
  
 output$values <- renderTable({   
   y <- r(input$h)
   x <- input$h
   data.frame(h=x,r=y)
 })
}


r_App <- function() {
  shinyApp(ui_r, server_r)
  }

