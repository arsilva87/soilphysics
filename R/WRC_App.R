findWRC = function(w, h) {
   y = w; x = h
   tS = max(y)
   tR = min(y)

   # Eqs
   models = c("van Genuchten", "Brooks & Corey", "Groenevelt & Grant", "Dexter")
   npar = c(2, 2, 3, 4)
   eqVG = expression( tR + (tS - tR) * (1 + (a * x)^n)^(-(1 - 1/n)) )
   eqBC = expression( ifelse(x < hb, tS, tR + (tS - tR) * (x/hb)^-lambda) )
   eqGG = expression( k1 * (exp(-k0/6.653^n) - exp(-k0/x^n)) )
   eqDE = expression( tR + a1 * exp(-x/p1) + a2 * exp(-x/p2) )
   exps = c(eqVG, eqBC, eqGG, eqDE)

   # -logliks
   nllVG = function(par) {
      e = y - eval(eqVG, list(a = par[1], n = par[2]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllBC = function(par) {
      e = y - eval(eqBC, list(hb = par[1], lambda = par[2]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllGG = function(par) {
      e = y - eval(eqGG, list(k0 = par[1], k1 = par[2], n = par[3]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }
   nllDE = function(par) {
      e = y - eval(eqDE, list(a1 = par[1], p1 = par[2],
         a2 = par[3], p2 = par[4]))
      -sum(dnorm(e, 0, sd(e), log = T))
   }

   # starting values
   grVG = expand.grid(a = seq(0.002, 0.2, len = 30),
      n = seq(1, 4, len = 30))
   grBC = expand.grid(hb = seq(0, 100, len = 50),
      lambda = seq(0.1, 3, len = 30))
   grGG = expand.grid(k0 = seq(1, 20, len = 30),
      k1 = seq(0, 1, len = 30),
      n = seq(1, 10, len = 20))
   grDE = expand.grid(a1 = seq(0.01, 0.4, len = 12),
      p1 = seq(1000, 10000, len = 20),
      a2 = seq(0.01, 0.5, len = 12),
      p2 = seq(10, 2000, len = 12))
   criVG = apply(grVG, 1, nllVG)
   criBC = apply(grBC, 1, nllBC)
   criGG = apply(grGG, 1, nllGG)
   criDE = apply(grDE, 1, nllDE)
   oVG = which.min(criVG)
   oBC = which.min(criBC)
   oGG = which.min(criGG)
   oDE = which.min(criDE)

   # MLEs
   mleVG = optim(par = grVG[oVG, ], nllVG, hessian = TRUE)
   mleBC = optim(par = grBC[oBC, ], nllBC, hessian = TRUE)
   mleGG = optim(par = grGG[oGG, ], nllGG, hessian = TRUE)
   mleDE = optim(par = grDE[oDE, ], nllDE, hessian = TRUE)
   pars = list(VG = mleVG$par, BC = mleBC$par, GG = mleGG$par, DE = mleDE$par)

   # Fitting criteria
   aic = c(VG = 2 * mleVG$value + 2*2,
      BC = 2 * mleBC$value + 2*2,
      GG = 2 * mleGG$value + 2*3,
      DE = 2 * mleDE$value + 2*4
   )
   std = list(VG = sqrt(diag(solve(mleVG$hessian))),
      BC = sqrt(diag(solve(mleBC$hessian))),
      GG = sqrt(diag(solve(mleGG$hessian))),
      DE = sqrt(diag(solve(mleDE$hessian)))
   )
   eVG = y - eval(eqVG, as.list(mleVG$par))
   eBC = y - eval(eqBC, as.list(mleBC$par))
   eGG = y - eval(eqGG, as.list(mleGG$par))
   eDE = y - eval(eqDE, as.list(mleDE$par))
   r2 = c(VG = 1 - sum(eVG^2)/sum((y - mean(y))^2),
      BC = 1 - sum(eBC^2)/sum((y - mean(y))^2),
      GG = 1 - sum(eGG^2)/sum((y - mean(y))^2),
      DE = 1 - sum(eDE^2)/sum((y - mean(y))^2)
   )
   mape = c(VG = 100 * mean(abs(eVG)/y),
      BC = 100 * mean(abs(eBC)/y),
      GG = 100 * mean(abs(eGG)/y),
      DE = 100 * mean(abs(eDE)/y)
   )

   # out
   dtf = data.frame(Rsq = r2, MAPE = mape, AIC = aic, npar)
   best_id = which.min(dtf[, "AIC"])
   best = paste("Best fit (AIC):", models[best_id])

   w_range = c(tR = tR, tS = tS)
   pa <- c(pars[[best_id]], w_range)
   pas <- as.list(pa)
   eq <- exps[best_id]
   for(j in 1:length(pas)) {
     eq <- gsub(names(pas)[j], round(pas[[j]], 5), eq, fixed = TRUE)
   }

   out = list(e = exps[best_id],
      fitting_criteria = dtf,
      best_fit = models[best_id], equation = eq,
      par = pars[[best_id]], std = std[[best_id]],
      w_range = w_range)
   return(out)
}

# ---------------------------------------------------------------------
ui_findWRC <- fluidPage(h3("Find the best-fit Water Retention Curve using maximum likelihood"),
  theme = shinytheme("spacelab"),
  windowTitle = "soilphysics | WRC",
  sidebarLayout(
       sidebarPanel(width = 3,
                    h4("Import the data"),
                    h6(tags$a(href="https://ce99d4d6-d4c5-48a3-b911-9e83247054ca.filesusr.com/ugd/45a659_170e68a8ee1f474b9493bc67df713eed.csv?dn=MyWRCExample.csv",
                              "(example)")),
                    fileInput("file", "(comma-separated values)",
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')
                    ),
                    selectInput('w', 'Water content', ''),
                    selectInput('h', 'Matric potential', ''),
                    actionButton("run", "Run", icon = icon("r-project"))
             ),
             mainPanel(
               tabsetPanel(tabPanel("Data", icon = icon("table"),
                                   h5("Input data (editable)"),
                                   rHandsontableOutput("tab")
                          ),
                          tabPanel("Results", icon = icon("calculator"),
                                   h5("Statistical comparison of models"),
                                   verbatimTextOutput("fit")
                          ),
                          tabPanel("Best-fit model", icon = icon("chart-line"),
                                   plotOutput("graph")
                          ),
                          tabPanel(icon("info-circle"),
                                   h6("Model legends: VG: van Genuchten,
                                      BC: Brooks & Corey,
                                      GG: Groenevelt & Grant,
                                      DE: Dexter"),
                                   h6("Statistics legends:
                                      Rsq: (pseudo) coefficient of determination,
                                      MAPE: mean absolute percentual error,
                                      AIC: Akaike information criterion,
                                      npar: number of estimated parameters,
                                      std: standard deviation"),
                                   h6("The parameters are estimated using the maximum
                                      likelihood method, based upon the normal
                                      distribution of model residuals. For optimization
                                      purposes, the values of the residual (theta_R)
                                      and saturation (theta_S) water content are
                                      not estimated. Instead, the minimum and maximum values
                                      are returned in the output 'w_range'"),
                                   h6("This app can also be run online: ",
                                      tags$a(href = "https://mybinder.org/v2/gist/arsilva87/59d2a78666cf13ab8fb60bafaf7e65b8/HEAD?urlpath=shiny",
                                             "click here")),
                                   h6("The fitsoilwater app offers another alternative to
                                      interactively fit water retention curves.",
                                      tags$a(href = "https://appsoilphysics.shinyapps.io/fitsoilwaterAPP/",
                                             "https://appsoilphysics.shinyapps.io/fitsoilwaterAPP/")),
                                   h6("Citation:
                                      da Silva, A. R., de Lima, R. P. (2022) soilphysics:
                                      Basic and Model-Based Soil Physical Analyses.
                                      R package version 5.0.",
                                   tags$a(href = "https://cran.r-project.org/package=soilphysics",
                                          "https://cran.r-project.org/package=soilphysics")),
                                   h6("Find out more about the soilphysics project: ",
                                      tags$a(href = "https://arsilva87.github.io/soilphysics/",
                                             "https://arsilva87.github.io/soilphysics/"))
                          )
               )
             )
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
    xy <- mt()[, c(input$h, input$w)]
    r <- findWRC(w = xy[,2], h = xy[,1])
    output$fit <- renderPrint( r[-1] )
    output$graph <- renderPlot({
      par(mar = c(4.5, 4.5, 2.5, 1), las = 1, cex = 1.3)
      plot(xy, cex = 2)
      pa <- c(r$par, r$w_range)
      pars <- as.list(pa)
      eq <- r$e
      f <- function(x) eval(eq, pars)
      curve(f, add = TRUE, col = "steelblue3", lwd = 4)
      for(j in 1:length(pars)) {
        eq <- gsub(names(pars)[j], round(pars[[j]], 4), eq, fixed = TRUE)
      }
      leg <- as.expression(paste0("y = ", eq))
      title(leg, cex.main = 0.9)
    })
  })
}

# Run the app
WRC_App <- function() {
  shinyApp(ui_findWRC , server_findWRC)
  }
