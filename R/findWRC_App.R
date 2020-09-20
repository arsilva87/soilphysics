
# -------------------------------------------------------------------
source("https://raw.githubusercontent.com/arsilva87/soilphysics/master/R/findWRC.R")

# ---------------------------------------------------------------------
ui_findWRC <- fluidPage(
   titlePanel("Automatically find the best fit WRC using maximum likelihood"),
   fluidRow(
      column(5, wellPanel(
         h4("Data input"),
         fileInput("file", "(.csv or .txt)",
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv')
                   ),
         rHandsontableOutput("tab")
      ),
      wellPanel(
        selectInput('w', 'Water content', ''),
        selectInput('h', 'Matric potential', ''),
        actionButton("run", "Run", icon = icon("r-project"))
      )
      ),
      column(5, wellPanel(
        h4("Model comparison"),
        verbatimTextOutput("fit")
      ))
   )
)

# ---------------------------------------------------------------------
server_findWRC <- function(session, input, output) {
  dataIn <- reactive( req(input$file) )
  output$tab <- renderRHandsontable({
    DF <- read.csv(dataIn()$datapath) 
    updateSelectInput(session, inputId = 'w',
                      choices = names(DF), selected = NULL)
    updateSelectInput(session, inputId = 'h',
                      choices = names(DF), selected = NULL)
    rhandsontable(DF)
  })
  mt <- reactive({
    DF = hot_to_r(input$tab)
    if(!is.null(input$tab$changes$changes)) {
      row.no <- unlist(input$tab$changes$changes)[1]
      col.no <- unlist(input$tab$changes$changes)[2]
      new.val <- unlist(input$tab$changes$changes)[4]
    }
    DF
  })
  observeEvent(input$run, {
    x <- mt()[, input$h]; y = mt()[, input$w]
    r <- findWRC(w = y, h = x)
    output$fit <- renderPrint( r )
  })
}

# Run the app
findWRC_App <- function() {
  shinyApp(ui_findWRC , server_findWRC)
}
