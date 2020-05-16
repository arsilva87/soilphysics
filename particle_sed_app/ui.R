


ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Sedimentation time of soil particle (Stokes' law)"),
  

         
  sidebarLayout(
    

    sidebarPanel(
      
      
      #-------
      

      sliderInput("d", "Soil particle diameter (micrometer)",
                  min = 2, max = 100,
                  step = 1, value=100, dragRange=TRUE),
      
      

      sliderInput("v", 'Fluid viscosity (N s/m^2)x(10^-3)',
                  min = 0.5, max = 1.5,
                  value = 1, step = 0.1),
      

      sliderInput("h", 'Fall height of soil particle (cm)',
                  min = 5, max = 25,
                  value = 20, step = 5),
      
      sliderInput("Pd", 'Particle density (kg/m^3)',
                  min = 2400, max = 3000,
                  value = 2650, step = 50),
      
      sliderInput("Wd", 'Fluid density (kg/m^3)',
                  min = 950, max = 1050,
                  value = 1000, step = 10)

      # -------
      
    ),
    
    # Main panel for displaying outputs ----
    

    mainPanel(
      
      plotOutput('plot1'),

      tableOutput("values"))
    
  ),
  
  verticalLayout(
    column(12,wellPanel(
      h4("by Renato P. de Lima"),
    ))),
  
  
)
