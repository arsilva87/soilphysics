findWRC = function(w, h,
                   mod2fit = c(TRUE, TRUE, TRUE, TRUE, TRUE))
{
  y = w; x = h
  tS = max(y)
  tR = min(y)
  stopifnot(any(mod2fit == TRUE))

  # Eqs
  models = c("van Genuchten", "Brooks & Corey",
             "Groenevelt & Grant", "Dexter", "FXW")
  npar = c(2, 2, 3, 4, 3)
  eqVG = expression( tR + (tS - tR)*(1 + (a * x)^n)^(-(1 - 1/n)) )
  eqBC = expression( ifelse(x < hb, tS, tR + (tS - tR)*(x/hb)^-lambda) )
  eqGG = expression( k1 * (exp(-k0/6.653^n) - exp(-k0/x^n)) )
  eqDE = expression( tR + a1 * exp(-x/p1) + a2 * exp(-x/p2) )
  eqFX = expression( tS * (1 - log(1 + x/1500)/log(1 + 6300000/1500)) *
                       (log(exp(1) + abs(a * x)^n))^-m )
  exps = c(eqVG, eqBC, eqGG, eqDE, eqFX)

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
  nllFX = function(par) {
    e = y - eval(eqFX, list(a = par[1], n = par[2], m = par[3]))
    -sum(dnorm(e, 0, sd(e), log = T))
  }


  # starting values
  if (mod2fit[1]) {
    grVG = expand.grid(a = seq(0.002, 0.2, len = 30),
                       n = seq(1, 4, len = 30))
    criVG = apply(grVG, 1, nllVG)
    oVG = which.min(criVG)
    mleVG = optim(par = grVG[oVG, ], nllVG, hessian = TRUE,
                  control = list(maxit = 1000))
  }
  if (mod2fit[2]) {
    grBC = expand.grid(hb = seq(0, 100, len = 50),
                       lambda = seq(0.1, 3, len = 30))
    criBC = apply(grBC, 1, nllBC)
    oBC = which.min(criBC)
    mleBC = optim(par = grBC[oBC, ], nllBC, hessian = TRUE,
                  control = list(maxit = 1000))
  }
  if (mod2fit[3]) {
    grGG = expand.grid(k0 = seq(1, 20, len = 30),
                       k1 = seq(0, 1, len = 30),
                       n = seq(1, 10, len = 20))
    criGG = apply(grGG, 1, nllGG)
    oGG = which.min(criGG)
    mleGG = optim(par = grGG[oGG, ], nllGG, hessian = TRUE,
                  control = list(maxit = 1000))
  }
  if (mod2fit[4]) {
    grDE = expand.grid(a1 = seq(0.01, 0.4, len = 12),
                       p1 = seq(500, 10000, len = 20),
                       a2 = seq(0.01, 0.5, len = 12),
                       p2 = seq(1, 2000, len = 12))
    criDE = apply(grDE, 1, nllDE)
    oDE = which.min(criDE)
    mleDE = optim(par = grDE[oDE, ], nllDE, hessian = TRUE,
                  control = list(maxit = 1000))
  }
  if (mod2fit[5]) {
    grFX = expand.grid(a = seq(0.002, 0.2, len = 30),
                       n = seq(1, 4, len = 30),
                       m = seq(0, 1, len = 20))
    criFX = apply(grFX, 1, nllFX)
    oFX = which.min(criFX)
    mleFX = optim(par = grFX[oFX, ], nllFX, hessian = TRUE,
                  control = list(maxit = 1000))
  }
  mles = list(VG = if(mod2fit[1]) mleVG else NA,
              BC = if(mod2fit[2]) mleBC else NA,
              GG = if(mod2fit[3]) mleGG else NA,
              DE = if(mod2fit[4]) mleDE else NA,
              FX = if(mod2fit[5]) mleFX else NA)

  pars = list(VG = if(mod2fit[1]) mleVG$par else NA,
              BC = if(mod2fit[2]) mleBC$par else NA,
              GG = if(mod2fit[3]) mleGG$par else NA,
              DE = if(mod2fit[4]) mleDE$par else NA,
              FX = if(mod2fit[5]) mleFX$par else NA)

  # Fitting criteria
  aic = c(VG = if(mod2fit[1]) 2*mleVG$value + 2*2 else NA,
          BC = if(mod2fit[2]) 2*mleBC$value + 2*2 else NA,
          GG = if(mod2fit[3]) 2*mleGG$value + 2*3 else NA,
          DE = if(mod2fit[4]) 2*mleDE$value + 2*4 else NA,
          FX = if(mod2fit[5]) 2*mleFX$value + 2*3 else NA
  )
  std = list(VG = if(mod2fit[1]) sqrt(diag(solve(mleVG$hessian))) else NA,
             BC = if(mod2fit[2]) sqrt(diag(solve(mleBC$hessian))) else NA,
             GG = if(mod2fit[3]) sqrt(diag(solve(mleGG$hessian))) else NA,
             DE = if(mod2fit[4]) sqrt(diag(solve(mleDE$hessian))) else NA,
             FX = if(mod2fit[5]) sqrt(diag(solve(mleFX$hessian))) else NA
  )
  eVG = if(mod2fit[1]) y - eval(eqVG, as.list(mleVG$par)) else NA
  eBC = if(mod2fit[2]) y - eval(eqBC, as.list(mleBC$par)) else NA
  eGG = if(mod2fit[3]) y - eval(eqGG, as.list(mleGG$par)) else NA
  eDE = if(mod2fit[4]) y - eval(eqDE, as.list(mleDE$par)) else NA
  eFX = if(mod2fit[5]) y - eval(eqFX, as.list(mleFX$par)) else NA

  r2 = c(VG = if(mod2fit[1]) 1 - sum(eVG^2)/sum((y - mean(y))^2) else NA,
         BC = if(mod2fit[2]) 1 - sum(eBC^2)/sum((y - mean(y))^2) else NA,
         GG = if(mod2fit[3]) 1 - sum(eGG^2)/sum((y - mean(y))^2) else NA,
         DE = if(mod2fit[4]) 1 - sum(eDE^2)/sum((y - mean(y))^2) else NA,
         FX = if(mod2fit[5]) 1 - sum(eFX^2)/sum((y - mean(y))^2) else NA
  )
  mape = c(VG = if(mod2fit[1]) 100*mean(abs(eVG)/y) else NA,
           BC = if(mod2fit[2]) 100*mean(abs(eBC)/y) else NA,
           GG = if(mod2fit[3]) 100*mean(abs(eGG)/y) else NA,
           DE = if(mod2fit[4]) 100*mean(abs(eDE)/y) else NA,
           FX = if(mod2fit[5]) 100*mean(abs(eFX)/y) else NA
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
             fitting_criteria = dtf[complete.cases(dtf), ],
             best_fit = models[best_id], equation = eq,
             par = pars[[best_id]], std = std[[best_id]],
             w_range = w_range)
  return(out)
}



#----
about <- fluidPage(
         withMathJax(),
         h6("Model equations:"),
         h6('- van Genuchten (VG)
            $$\\theta (m^3 m^{-3}) = \\theta_R + (\\theta_S - \\theta_R)(1 + (\\alpha x)^n)^{1/n -1}$$'),
         h6('- Brooks & Corey (BC)
            $$\\theta (m^3 m^{-3}) = \\theta_R + (\\theta_S - \\theta_R)(x_b/x)^{\\lambda}$$'),
         h6('- Groenevelt & Grant (GG)
            $$\\theta (m^3 m^{-3}) = k_1 \\exp(-k_0 / x_0^n) - k_1 \\exp(-k_0 / x^n)$$'),
         h6('- Dexter (DE)
            $$\\theta (m^3 m^{-3}) = \\theta_R + a_1 \\exp(-x/p_1) + a_2 \\exp(-x/p_2)$$'),
         h6('- FXW (Fredlung-Xing-Wang, Fredlung and Xing 1994; Wang et al. 2018)
            $$\\theta (m^3 m^{-3}) = \\theta_S \\left(1 - \\frac{\\log(1 + x/h_r)}{\\log(1 + h_0/h_r)}\\right)
                      [\\log(\\exp(1) + |a x|^n)]^{-m}$$'),
         h6('where $$h_r = 1500, h_0 = 6.3 \\times 10^6$$ cm
            (Schneider and Goss, 2012; Rudiyanto et al., 2021)'),
         h6("Please check the documentation of the functions 'soilwater'
            of package soilphysics to know more about each equation."),
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
                  h6("This app can run online: ",
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


# ---------------------------------------------------------------------
ui_findWRC <- dashboardPage(
  dashboardHeader(title = "WRC App: Maximum Likelihood fit of Water Retention Curve",
                  titleWidth = 600),
  dashboardSidebar(
    h6(tags$a(href = "https://arsilva87.github.io/soilphysics/",
              "Powered by: soilphysics")),
    sidebarMenu(
       menuItem("Import data", icon = icon("folder-open"),
                h6(tags$a(href="https://ce99d4d6-d4c5-48a3-b911-9e83247054ca.filesusr.com/ugd/45a659_170e68a8ee1f474b9493bc67df713eed.csv?dn=MyWRCExample.csv",
                          "(example)")),
                    fileInput("file", "(comma-separated values)",
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')
                    ),
                checkboxInput("semicolon", "Semicolon-separated"),
                checkboxInput("dec", "Comma as decimal")
       ),
       menuItem("Select columns", icon = icon("table"),
                selectInput('w', 'Water content (y)', ''),
                selectInput('h', 'Matric potential (x)', ''),
                checkboxInput("logh", "Log10( x )")
       ),
       menuItem("Models to fit", icon = icon("check"),
                checkboxInput("modVG", "van Genuchten", value = TRUE),
                checkboxInput("modBC", "Brooks & Corey", value = TRUE),
                checkboxInput("modGG", "Groenevelt & Grant", value = TRUE),
                checkboxInput("modDE", "Dexter", value = TRUE),
                checkboxInput("modFX", "FXW", value = TRUE)
       ),
       actionButton("run", "RUN", width = "40%",
                    icon = icon("r-project"))
    )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel(icon("home"),
        fluidRow(
          box(
            title = "Data", status = "warning", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            h5("Input data (editable, left-click)
               (right-click to insert or remove rows)"),
            rHandsontableOutput("tab")
          ),
          tabBox(title = "Results", width = 12,
            tabPanel("Statistics", icon = icon("calculator"),
              verbatimTextOutput("fit")
            ),
            tabPanel("Best-fit", icon = icon("chart-line"),
              plotOutput("graph", height = 480, width = 650)
            )
          )
        )
      ),
      tabPanel(icon("info"), about)
    )
  )
)



# ---------------------------------------------------
server_findWRC <- function(session, input, output) {
  dataIn <- reactive( req(input$file) )
  output$tab <- renderRHandsontable({
    dec <- ifelse(input$dec, ",", ".")
    if(input$semicolon) {
       DF <- read.csv2(dataIn()$datapath, dec = dec)
    } else {
       DF <- read.csv(dataIn()$datapath, dec = dec)
    }
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
    if (input$logh) {
      xy[,1] <- log10(xy[,1])
    }
    r <- findWRC(w = xy[,2], h = xy[,1],
                 mod2fit = c(input$modVG, input$modBC,
                             input$modGG, input$modDE, input$modFX))
    output$fit <- renderPrint( r[-1] )
    output$graph <- renderPlot({
      par(mar = c(4.5, 4.5, 2.5, 1), las = 1, cex = 1.3)
      plot(xy, cex = 2, log = "x")
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
