

ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Soil Aggregate-Size Distribution"),
  
         
  sidebarLayout(
    

    sidebarPanel(
      
      
      # -------
      sliderInput("g1", 'Soil aggregates mass (g) (Size class = 3 mm)',
                  min = 0, max = 50,
                  value = 20, step = 0.5),
      sliderInput("g2", 'Soil aggregates mass (g) (Size class = 1.5 mm)',
                  min = 0, max = 50,
                  value = 10, step = 0.5),
      sliderInput("g3", 'Soil aggregates mass (g) (Size class = 0.75 mm)',
                  min = 0, max = 50,
                  value = 5, step = 0.5),
      sliderInput("g4", 'Soil aggregates mass (g) (Size class = 0.375 mm)',
                  min = 0, max = 50,
                  value = 5, step = 0.5),
      sliderInput("g5", 'Soil aggregates mass (g) (Size class = 0.250 mm)',
                  min = 0, max = 50,
                  value = 5, step = 0.5),
      sliderInput("g6", 'Soil aggregates mass (g) (Size class = 0.178 mm)',
                  min = 0, max = 50,
                  value = 5, step = 0.5),
      sliderInput("g7", 'Soil aggregates mass (g) (Size class < 0.053 mm)',
                  min = 0, max = 50,
                  value = 5, step = 0.5)
      
      

      # -------
      
    ),
    
    # Main panel for displaying outputs ----
    column(8,mainPanel(
      
      plotOutput('plot1'),tableOutput("values"))
  )
  ),
  
  verticalLayout(
    column(12,wellPanel(
      h4("by Renato P. de Lima"),
      
      
    )))
  
)


server <- function(input, output) {
  
  # my function 
  aggreg <- function (dm.classes, aggre.mass) 
  {
    total.weight <- c()
    for (j in 1:length(aggre.mass[, 1])) {
      total.weight[j] <- sum(aggre.mass[j, ])
    }
    proportion <- matrix(nrow = length(aggre.mass[, 1]), ncol = length(aggre.mass[1, 
                                                                                  ]))
    for (j in 1:length(aggre.mass[, 1])) {
      for (k in 1:length(aggre.mass[1, ])) {
        proportion[j, k] <- (aggre.mass[j, k]/total.weight[j])
      }
    }
    SUM <- matrix(nrow = length(aggre.mass[, 1]), ncol = length(aggre.mass[1, 
                                                                           ]))
    for (j in 1:length(aggre.mass[, 1])) {
      for (k in 1:length(aggre.mass[1, ])) {
        SUM[j, k] <- (dm.classes[k] * proportion[j, k])
      }
    }
    DMP <- c()
    for (j in 1:length(aggre.mass[, 1])) {
      DMP[j] <- sum(SUM[j, ])
    }
    SUM2 <- matrix(nrow = length(aggre.mass[, 1]), ncol = length(aggre.mass[1, 
                                                                            ]))
    for (j in 1:length(aggre.mass[, 1])) {
      for (k in 1:length(aggre.mass[1, ])) {
        SUM2[j, k] <- (proportion[j, k] * log(dm.classes[k]))
      }
    }
    DMG <- c()
    for (j in 1:length(aggre.mass[, 1])) {
      DMG[j] <- exp(sum(SUM2[j, ]))
    }
    prop <- round(proportion * 100, 0)
    colnames(prop) <- as.character(dm.classes)
    aggregation.indices <- data.frame(MWD = DMP, GMD = DMG, 
                                      Total.Soil.Mass = total.weight)
    return(aggregation.indices)
  }

  
  
 output$plot1 <- renderPlot({
   
   classes <- c(3, 1.5, 0.75, 0.375, 0.250,0.178, 0.053)
   d <- data.frame(a=input$g1,b=input$g2,c=input$g3,
                    d=input$g4,e=input$g5,f=input$g6,g=input$g7)
   a <- aggreg(dm.classes = classes, aggre.mass = d)
   
   yDMP <- a$MWD
   yDMG <- a$GMD
   f <- function (x) 0.5 + 1.375*x
   tamDMP <- f(yDMP)
   tamDMG <- f(yDMG)
   
   par(cex=0.9, mfrow=c(2,1), mar=c(2,4,0.5,1))
   plot(x=1,y=1,xaxt='n', pch=19, cex=1,type="l",
        col="brown", xlim=c(1,1.8), 
        ylim=c(0,3),xlab="",ylab="")
   axis(1, at=c(1.2,1.6),cex.axis=0.8,
       labels=c("Mean weight-diameter (MWD)","Geometric mean diameter (GMD)"))
   points(x=1.2,y=yDMP, pch=19, col="brown", cex=tamDMP)
   points(x=1.6,y=yDMG,pch=19,col="brown", cex=tamDMG)
   mtext("Diameter of soil aggregates (mm)",2, line=2.5, cex=0.9)
   
   
   prop <- (d/a$Total.Soil.Mass)*100
   col <- c(1,1,1,1,2,2,2)
   par(cex=0.9, mar=c(4,4,1,1))
   prop <- as.numeric(prop)
   barplot(prop,ylim=c(0,100), xlim=c(0.4,8), col=col)
   axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7, 7.9),labels=as.factor(classes))
   mtext("Proportion of soil aggregates (%)",2, line=2.5, cex=0.9)
   mtext("Mean diameter of aggregates (mm)",1, line=2.5, cex=0.9)
   legend("topright",
          legend=c("> 0.25 mm (macroaggregates)",
                   "< 0.25 mm (microaggregates)","Tisdall & Oades (1982)"),
          col=c(1,2,NA), pch=15, cex=0.9)
   
    
  })
 
 
 
 output$values <- renderTable({
   
   classes <- c(3, 1.5, 0.75, 0.375, 0.25, 0.178, 0.053)
   d <- data.frame(a=input$g1,b=input$g2,c=input$g3,
                   d=input$g4,e=input$g5,f=input$g6,f=input$g7)
   a <- aggreg(dm.classes = classes, aggre.mass = d)
   
   data.frame(MWD=a$MWD,GMD=a$GMD,Soil.Total.Mass=a$Total.Soil.Mass)
   
 })
}


aggreg.stability_App <- function() {
  shinyApp(ui, server)
  }


