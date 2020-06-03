library(shiny)

# UI

ui <- fluidPage(
  
  
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

    "LLWR and LLMPR calculation",
    
    
    
    tabPanel("FC",
             
             
             verticalLayout(
               column(12,wellPanel(
                 
                 helpText(tags$p("FIELD CAPACITY SECTION: in this section, the user is able to calculate the matric potential at the field capacity 
                                  to be assigned as the field capacity threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                                 
                                 style = "font-size: 120%;text-align:justify"))
                 
               ))),
             
             
             titlePanel(tags$p("Field capacity (FC) restriction threshold",
                               style = "font-size: 90%;text-align:justify")),
             
             column(3,wellPanel(
               h4("Genuchten's parameters"),
               
               
               sliderInput("alpha1", HTML(paste0("&alpha; (hPa",tags$sup("-1"),")")),
                           min = 0.01, max = 0.5,
                           step = 0.0001, value=0.056, tick=FALSE),
               
               
               sliderInput("n1", "n",
                           min = 1, max = 4,
                           value = 2.5, step = 0.0001,tick=FALSE),
               
               helpText(tags$p("Move the slider input to calculate the matric potential (h) at the FC 
                               by assigning 'n' and 'alpha', which are Genuchten's water retention curve parameters",
                               style = "font-size: 83%;text-align:justify"))
               
               
             )),
             
            
             
             
             column(5,wellPanel(
               h4("h at the field capacity"),
               tabsetPanel(type = "tabs",
                           tabPanel("h at the FC", plotOutput("FC1")))           
                        
             )
          ),
             
             
          verticalLayout(
            column(12,wellPanel(
              h4("Useful links"),
              
          
              
              actionButton(inputId='ab1', label="Assouline & Or (2014)", 
                           icon = icon("th"), 
                           onclick ="window.open('https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2014WR015475')"),
              
              
              actionButton(inputId='ab1', label="Meskini-Vishkaee et al. (2018)", 
                           icon = icon("th"), 
                           onclick ="window.open('https://www.publish.csiro.au/SR/SR17025')"),
              
              
              
            ))),
          
          
          
          
  ),
  
  
  
  
  
  tabPanel("PWP",
           
           
           
           verticalLayout(
             column(12,wellPanel(
               
               helpText(tags$p("PERMANENT WILTING POINT SECTION: in this section, the user is able to calculate the matric potential at the hydraulic cut-off 
                                  to be assigned as permanent wilting point threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                               
                               style = "font-size: 115%;text-align:justify"))
               
             ))),
           
           
           titlePanel("Permanent wilting point (PWP) restriction threshold"),
           
           column(3,wellPanel(
             h4("Parameters"),
             
             
             sliderInput("C", HTML(paste0("C [DE] (g g",tags$sup("-1"),")")),
                         min = 0.01, max = 0.15,
                         step = 0.0001, value=0.08,tick=FALSE),
             
             sliderInput("A1", HTML(paste0("A1 [DE] (g g",tags$sup("-1"),")")),
                         min = 0.01, max = 0.10,
                         step = 0.0001, value=0.02,tick=FALSE),
             
             sliderInput("h1", "h1 (hPa) [DE]",
                         min = 1000, max = 10000,
                         step = 1, value=4470,tick=FALSE),
             
             sliderInput("A2", HTML(paste0("A2 [DE] (g g",tags$sup("-1"),")")),
                         min = 0.01, max = 0.50,
                         step = 0.0001, value=0.18,tick=FALSE),
             
             sliderInput("h2", "h2 (hPa) [DE]",
                         min = 10, max = 2000,
                         step = 1, value=1400,tick=FALSE),
             
          
             
             helpText(tags$p("Move the slider input to calculate the hydraulic cut-off of soil
                             by assigning the Dexter's [DE] water retention curve parameters (see Dexter et al. 2012)",
                             style = "font-size: 85%;text-align:justify"))
             
             
           )),
           
           
           
           column(3,wellPanel(
             h4(tags$p("Estimating the hydraulic cut-off (hco)",
                       style = "font-size: 85%;text-align:justify")),
             
             
             selectInput("PEDO.hco", "Pedo-transfer function", 
                         choices=c("Dexter et al. (2012)","Czyz et al. (2012)")),
             
             
             sliderInput("CChco", "Clay content (%)",
                         min = 10, max = 60,
                         step = 1, value=30,tick=FALSE),
             
             
             helpText(tags$p("Alternatively, if the parameters of Dexte's water retention curve are unknown, 
                              the hydraulic cut-off can be estimated using pedo-transfer functions",
                              style = "font-size: 85%;text-align:justify"))
             
             
           )),
           
           
           

           column(5,wellPanel(
             h4("Hydraulic cut-off"),
             tabsetPanel(type = "tabs",
                         tabPanel("by Dexte's WRC", plotOutput("PWP1")),
                         tabPanel("Pedo-transfer", tableOutput("PWPdata")))
             
    
             
           )
           ),
           
           
           verticalLayout(
             column(12,wellPanel(
               h4("Useful links"),
               

               
               actionButton(inputId='ab1', label="Dexter et al. (2012)", 
                            icon = icon("th"), 
                            onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0016706112000584')"),
               
               actionButton(inputId='ab1', label="Czyz et al. (2012)", 
                            icon = icon("th"), 
                            onclick ="window.open('https://www.publish.csiro.au/sr/sr12189')"),
               
               
               actionButton(inputId='ab1', label="Wiecheteck et al. (2020)", 
                            icon = icon("th"), 
                            onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0378377419308686')"),
               
               
               
             ))),
           
           
           
           
  ),
  
  
    

  
  
  
# AERATION 
  
  tabPanel("AFP",
           
           
           
           verticalLayout(
             column(12,wellPanel(
               
               helpText(tags$p("AIR-FILLED POROSITY SECTION: in this section, the user is able to calculate the minimal air-filled porosity as a function of 
                                  relative gas diffusivity to be assigned as aeration threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                               
                               style = "font-size: 115%;text-align:justify"))
               
             ))),
           
           
           
           
           titlePanel("Air-filled porosity (AFP) restriction thresholds"),
           
           column(3,wellPanel(
             h4("Millington & Quirk's inputs"),
             
             
             sliderInput("TP", HTML(paste0("Total porosity (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                         min = 0.2, max = 0.8,
                         step = 0.01, value=0.50, tick=FALSE),
             
             
             sliderInput("mim.gas.difusion", HTML(paste0("Minimal relative gas diffusivity (D",tags$sub("s"),"/","D",tags$sub("0"),")")),
                         min = 0.005, max = 0.02,
                         value = 0.01, step = 0.0001,tick=FALSE)
             
             
           )),
           
           
           
           column(5,wellPanel(
             h4("Minimal air-filled porosity"),
             tabsetPanel(type = "tabs",
                         tabPanel("Air-filled porosity", plotOutput("AFP1")))     
             
           )
           ),
           
           
           
           verticalLayout(
             column(12,wellPanel(
               h4("Useful links"),
               
               actionButton(inputId='ab1', label="Partridge et al. (2011)", 
                            icon = icon("th"), 
                            onclick ="window.open('https://www.tandfonline.com/doi/pdf/10.1080/10473289.1999.10463812')"),
               
               actionButton(inputId='ab1', label="Pulido-Moncada & Munkholm (2019)", 
                            icon = icon("th"), 
                            onclick ="window.open('https://acsess.onlinelibrary.wiley.com/doi/full/10.2136/sssaj2019.01.0023')"),
               

               
             ))),
           
           
           
  ),


  
# SOIL PENETRATION RESISTANCE 

navbarMenu("SPR",
           
           
          
           
      tabPanel("Taylor & Ratliff (1969)",
               

               verticalLayout(
                 column(12,wellPanel(
                   
                   helpText(tags$p("SOIL PENETRATION RESISTANCE SECTION: in this section, the user is able to calculate the limiting soil 
                                  penetration resistance for a given root elongation rate to be assigned as limiting soil penetration resistance threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                                   
                                   style = "font-size: 100%;text-align:justify"))
                   
                 ))),
                    
                    titlePanel("Soil penetration resistance (SPR) restriction threshold"),
                    
                    column(3,wellPanel(
                      h4("Inputs"),
                      
                      
                      selectInput("cropTaylor", "Crop", 
                                  choices=c("Peanuts","Cotton")),
                      
                      
                      sliderInput("root.rateTaylor", "Root elongation rate (%)",
                                  min = 30, max = 90,
                                  step = 1, value=30,tick=FALSE)
                      
                      
                    )),
                    
                    
                    
                    column(5,wellPanel(
                      h4("Critical SPR for a given root elongation rate"),
                      tabsetPanel(type = "tabs",
                                  tabPanel("Root elongation rate", plotOutput("plotTaylor")))        
                      
                    )
                    ),
               
               
               verticalLayout(
                 column(12,wellPanel(
                   h4("Useful links"),
                   
                   actionButton(inputId='ab1', label="Taylor & Ratliff (1969)", 
                                icon = icon("th"), 
                                onclick ="window.open('https://journals.lww.com/soilsci/Citation/1969/08000/Root_Elongation_Rates_of_Cotton_and_Peanuts_As_A.6.aspx')"),
                   
                   
                   actionButton(inputId='ab1', label="Bengough (1997)", 
                                icon = icon("th"), 
                                onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0022519396903677')"),
                   
                   
                   actionButton(inputId='ab1', label="Bengough et al. (2011)", 
                                icon = icon("th"), 
                                onclick ="window.open('https://www.ncbi.nlm.nih.gov/pubmed/21118824')"),
                   
                   
                   
                 ))),
  ),
                    
                    
                    
                    
           
tabPanel("Veen & Boone (1991)",
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("SOIL PENETRATION RESISTANCE SECTION: in this section, the user is able to calculate the limiting soil 
                                  penetration resistance for a given root elongation rate to be assigned as limiting soil penetration resistance threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         
         
         titlePanel("Soil penetration resistance (SPR) restriction threshold"),
         
         column(3,wellPanel(
           h4("Inputs"),
           
           
           selectInput("MPVeen", "Matric potential (MPa)", 
                       choices=c("-0.01 MPa","-0.10 MPa","-0.63 MPa")),
           
           
           sliderInput("root.rateVeen", "Root elongation rate (%)",
                       min = 10, max = 90,
                       step = 1, value=30,tick=FALSE)
           
           
         )),
         
         
         
         column(5,wellPanel(
           h4("Critical SPR for a given root elongation rate"),
           tabsetPanel(type = "tabs",
                       tabPanel("Root elongation rate", plotOutput("plotVeen")))        
           
         )
         ),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             
             
             actionButton(inputId='ab1', label="Veen & Boone (1990)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/pii/0167198790900318')"),
             
             
             actionButton(inputId='ab1', label="Bengough (1997)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0022519396903677')"),
             
             
             actionButton(inputId='ab1', label="Bengough et al. (2011)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.ncbi.nlm.nih.gov/pubmed/21118824')"),
             
             
           ))),
         ),




tabPanel("Moraes et al. (2018)",
         
         
         
         verticalLayout(
           column(12,wellPanel(
             
             helpText(tags$p("SOIL PENETRATION RESISTANCE SECTION: in this section, the user is able to calculate the limiting soil 
                                  penetration resistance for a given root elongation rate to be assigned as limiting soil penetration resistance threshold of the LLWR and LLMPR. This methodology is 
                                  described in de Lima et al. (2020). Optionally, it is possible to assign your own threshold values. 
                                  See LLWR and LLMPR section.",
                             
                             style = "font-size: 100%;text-align:justify"))
             
           ))),
         
         
         titlePanel("Critical SPR for a given root elongation rate"),
         
         column(4,wellPanel(
           h4("Inputs"),
           
           
           selectInput("fMoraes", "Soil structure condiction", 
                       choices=c("Soil without biopores (-0.4325)",
                                 "Soil with biopores (-0.3000)")),
           
           
           sliderInput("root.rateMoraes", "Root elongation rate (%)",
                       min = 10, max = 90,
                       step = 1, value=30,tick=FALSE)
           
           
         )),
         
         
         
         column(5,wellPanel(
           h4("SPR restriction thresholds"),
           tabsetPanel(type = "tabs",
                       tabPanel("Root elongation rate", plotOutput("plotMoraes")))        
           
         )
         ),
         
         
         
         verticalLayout(
           column(12,wellPanel(
             h4("Useful links"),
             
             

             
             actionButton(inputId='ab1', label="Bengough (1997)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0022519396903677')"),
             
             
             actionButton(inputId='ab1', label="Bengough et al. (2011)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://www.ncbi.nlm.nih.gov/pubmed/21118824')"),
             
             
             actionButton(inputId='ab1', label="Moraes et al. (2018)", 
                          icon = icon("th"), 
                          onclick ="window.open('https://link.springer.com/article/10.1007/s11104-018-3656-z')"),
             
             
             
           ))),
          )



),



    tabPanel("LLWR and LLMPR",
             
             titlePanel("Least Limiting Water (LLWR) and Matric Potential (LLMPR) Ranges"),
             
             column(3,wellPanel(
               h4("Genuchten's parameters"),
               
               
               sliderInput("thetaS2", HTML(paste0("&theta;",tags$sub("s") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                           min = 0.2, max = 0.7,
                           step = 0.01, value=0.55, tick=FALSE),
               
               sliderInput("thetaR2", HTML(paste0("&theta;",tags$sub("r") ," (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                           min = 0.01, max = 0.3,
                           step = 0.01, value=0.11, tick=FALSE),
               
               
               sliderInput("alpha2", HTML(paste0("&alpha; (hPa",tags$sup("-1"),")")),
                           min = 0.01, max = 0.5,
                           value = 0.13, step = 0.0001,tick=FALSE),
               
               
               sliderInput("n2", "n",
                           min = 1, max = 4,
                           step = 0.01, value=1.3, tick=FALSE),
               
               sliderInput("d", "d",
                           min = 0.001, max = 0.5,
                           step = 0.001, value=0.03, tick=FALSE),
               
               sliderInput("e", "e",
                           min = -3, max = -1,
                           step = 0.001, value=-2.9, tick=FALSE),
               
               
               sliderInput("f", "f",
                           min = 1, max = 8,
                           value = 3.54, step = 0.0001,tick=FALSE),
               
               
               helpText(tags$p("Move the slider input for assigning the water (Genuchten's parameters) and penetration resistance (d, e and f are the Busscher's parameters) curves (See de Lima et al., 2020)",
                               style = "font-size: 92%;text-align:justify"))

             )),
             
 
             
             
             column(3,wellPanel(
               h4("Restriction thresholds"),
               
               
               sliderInput("air", HTML(paste0("AFP (m",tags$sup("3") ," m",tags$sup("-3"),")")),
                           min = 0.05, max = 0.25,
                           step = 0.001, value=0.10, tick=FALSE),
               
               
               sliderInput("fc", "FC (hPa)",
                           min = 10, max = 330,
                           value = 100, step = 1,tick=FALSE),
               
               
               sliderInput("pr", 'SPR (MPa)',
                           min = 1, max = 7,
                           value = 4, step = 0.1,tick=FALSE),
               
               sliderInput("pwp", 'PWP (hPa)',
                           min = 10000, max = 20000,
                           value = 15000, step = 100,tick=FALSE),
              
               
               sliderInput("PD", HTML(paste0("Particle density (Mg m",tags$sup("-3"),")")),
                           min = 2.4, max = 2.8,
                           value = 2.65, step = 0.01,tick=FALSE),
               
               helpText(tags$p("Assign your own restriction thresholds, or calculate them 
                                in the previous sections. Further in de Lima et al. (2020). 
                                LEGEND: AFP: air-filled porosity; FC: field capacity; PWP: permanent wilting point; SPR: soil penetration resistance",
                               style = "font-size: 88%;text-align:justify"))
               
             )),
             
             
             
             
             column(5,wellPanel(
               h4("LLWR and LLMPR"),
               tabsetPanel(type = "tabs",
                           tabPanel("LLWR", plotOutput("plot5")),
                           tabPanel("LLMPR", plotOutput("plot6")),
                           tabPanel("Limits", tableOutput("values3")),
                           tabPanel("Range", tableOutput("values4")))  
                      
             )
             ),
             
             
             verticalLayout(
               column(12,wellPanel(
                 h4("Useful links"),
                 
                 
                 
                 actionButton(inputId='ab1', label="Nomeclature of this section", 
                              icon = icon("th"), 
                              onclick ="window.open('https://github.com/arsilva87/soilphysics/raw/master/man/figures/NOMENCLATURE%20FOR%20THE%20SECTIONS%20LLWR%20AND%20LLMPR.pdf')"),
                 
                 actionButton(inputId='ab1', label="Silva et al. (1994)", 
                              icon = icon("th"), 
                              onclick ="window.open('https://acsess.onlinelibrary.wiley.com/doi/abs/10.2136/sssaj1994.03615995005800060028x')"),
                 
                 
                 actionButton(inputId='ab1', label="Lima et al. (2016)", 
                              icon = icon("th"), 
                              onclick ="window.open('https://www.sciencedirect.com/science/article/pii/S0168169915003403')"),
                 
                 
                 actionButton(inputId='ab1', label="R package soilphysics", 
                              icon = icon("th"), 
                              onclick ="window.open('https://arsilva87.github.io/soilphysics/')"),
                 
                 
                 
               ))),
             
        ),
    
    
    
  tabPanel("About", "",
  
  
  verticalLayout(
    column(12,wellPanel(
      
      tags$p("This R app is an interactive web interface for calculation of the least limiting water range (LLWR) and least limiting matric 
              potential range (LLMPR) and integrate the set of functions for soil physical data analysis of the R soilphysics package.", 
              style = "font-size: 90%;text-align:justify"),

  
      actionButton(inputId='ab1', label="soilphysics", 
                   icon = icon("th"), 
                   onclick ="window.open('https://arsilva87.github.io/soilphysics/')"),
      
    ))),
  
  
  
  
  verticalLayout(
    column(12,wellPanel(
      tags$p("Developed by Renato P. de Lima; Cassio A. Tormena; Getulio C. Figueiredo; Anderson R. da Silva; & Mario M. Rolim", style = "font-size: 90%;"),
      
    ))),
  
  
  verticalLayout(
    column(12,wellPanel(
      tags$p("Suggestions and bug reports: renato_agro_@hotmail.com", style = "font-size: 90%;"),
      
      
      
      
    ))),
  
)
  
  
  
    
  )
)





# SERVER


server <- function(input, output) {
  
  
hFC <-   function (alpha, n) (1/alpha)*((n-1)/n)^((1-2*n)/n)



AIR.critical <- function (mim.gas.difusion, thetaS)  (mim.gas.difusion*(thetaS)^2)^(1/(10/3))



PR.Moraes <- function (root.elongation.rate,x) {
  
  f <- c()
  if (x=="Soil without biopores (-0.4325)") {f <- -0.4325} 
  if (x=="Soil with biopores (-0.3000)") {f <- -0.3000}
  out <- log(root.elongation.rate)/f
  return(out)
}



PR.Veen <- function (root.elongation.rate, suction) {
  
  
  maxS1 <- 47.72 # -0.01
  maxS2 <- 45.2 # -0.1
  maxS3 <- 30.36 # 0.63
  maxS <- c()
  if (suction=="-0.01 MPa") {maxS <- maxS1}
  if (suction=="-0.10 MPa") {maxS <- maxS2}
  if (suction=="-0.63 MPa") {maxS <- maxS3}
  
  S <- c()
  if (suction=="-0.01 MPa") {S <- -0.01}
  if (suction=="-0.10 MPa") {S <- -0.10}
  if (suction=="-0.63 MPa") {S <- -0.63}
  
  
  RET <- (root.elongation.rate/100)*maxS
  out <- (RET - (28*S) - 48)/-12
  return (out)
}








hydraulicCutOff.2 <- function (theta_R,A1,A2,h1,h2) {
    
    wh <- function(x) theta_R + A1 * exp(-x/h1) + A2 * exp(-x/h2)
    fh <- function(x) A1/h1 * (x/h1 - 1) * exp(-x/h1) + A2/h2 * (x/h2 - 1) * exp(-x/h2)
    gh <- function(x) A1/h1 * exp(-x/h1) + A2/h2 * exp(-x/h2)
    dfh <- function(x) A1/h1^2 * (2 - x/h1) * exp(-x/h1) + A2/h2^2 * (2 - x/h2) * exp(-x/h2)
    kh <- function(x) ( log(10)^2 * x * fh(x) )/( 1 + x^2 * log(10)^2 * gh(x)^2 )^(3/2)
    hm <- optimize(kh, lower=1,upper=100000, maximum = T)$`maximum` # hPa
    
    return(hm)
}
  
  







llwr_llmpr <- function (thetaR, thetaS, alpha, n, d, e, f, critical.PR, 
                        PD,h.FC, h.PWP, air.porosity)
  
{
  m = 1 - 1/n
  vanG.matric <- function(theta, thetaR, thetaS, alpha, n) {
    S <- (theta - thetaR)/(thetaS - thetaR)
    f <- n/(1 - n)
    h <- (1/alpha) * ((S^f) - 1)^(1/n)
    out <- data.frame(theta, h)
    return(out)
  }
  
  BD <- round((1 - thetaS) * PD, 2)
  
  spr <- function(d, e, f, BD, critical.PR) {
    m <- (critical.PR/(d * BD^f))^(1/e)
    return(m)
  }
  
  thetaAIR <- (thetaS - air.porosity)
  thetaFC <- (thetaR + ((thetaS - thetaR)/(1 + (alpha * (h.FC))^n)^m))
  thetaPWP <- (thetaR + ((thetaS - thetaR)/(1 + (alpha * (h.PWP))^n)^m))
  thetaPR <- spr(d = d, e = e, f = f, BD = BD, critical.PR = critical.PR)
  hAIR <- vanG.matric(theta = thetaAIR, thetaR = thetaR, thetaS = thetaS, 
                      alpha = alpha, n = n)$h
  hPR <- vanG.matric(theta = thetaPR, thetaR = thetaR, thetaS = thetaS, 
                     alpha = alpha, n = n)$h
  if (hPR ==  "NaN" || hPR > 15000 || hPR == "NA" || hPR == Inf) (hPR <- 15000)
  
  SL <- c()
  SL.out <- c()
  IL <- c()
  IL.out <- c()
  for (j in 1:length(thetaS)) {
    if (thetaAIR[j] < thetaFC[j]) {
      SL[j] <- thetaAIR[j]
      SL.out[j] <- thetaFC[j]
    }
    else if (thetaAIR[j] > thetaFC[j]) {
      SL[j] <- thetaFC[j]
      SL.out[j] <- thetaAIR[j]
    }
    if (thetaPWP[j] > thetaPR[j]) {
      IL[j] <- thetaPWP[j]
      IL.out[j] <- thetaPR[j]
    }
    else if (thetaPWP[j] < thetaPR[j]) {
      IL[j] <- thetaPR[j]
      IL.out[j] <- thetaPWP[j]
    }
  }
  SL_LLWR <- SL
  SL_LLWR.out <- SL.out
  IL_LLWR <- IL
  IL_LLWR.out <- IL.out
  
  SL_LLMPR <- vanG.matric(theta = SL_LLWR, thetaR = thetaR, 
                          thetaS = thetaS, alpha = alpha, n = n)$h
  SL_LLMPR.out <- vanG.matric(theta = SL_LLWR.out, thetaR = thetaR, 
                              thetaS = thetaS, alpha = alpha, n = n)$h
  IL_LLMPR <- vanG.matric(theta = IL_LLWR, thetaR = thetaR, 
                          thetaS = thetaS, alpha = alpha, n = n)$h
  IL_LLMPR.out <- vanG.matric(theta = IL_LLWR.out, thetaR = thetaR, 
                              thetaS = thetaS, alpha = alpha, n = n)$h
  LLWR <- (SL_LLWR - IL_LLWR)
  LLMPR <- (IL_LLMPR - SL_LLMPR)
  
  if (LLWR < 0) {LLWR <- 0}
  if (LLMPR ==  "NaN" || LLMPR < 0 || LLMPR == "NA") {LLMPR <- 0}
  matric <- round(c(hAIR, h.FC, h.PWP, hPR), 2)
  theta <- round(c(thetaAIR, thetaFC, thetaPWP, thetaPR), 4)
  out1 <- data.frame(theta = theta, potential = matric)
  rownames(out1) <- c("AIR", "FC", "PWP", 
                      "PR")
  Limits <- round(data.frame(Upper = SL_LLWR, Lower = IL_LLWR, 
                             Range = LLWR), 4)
  Limits[2, ] <- round(c(SL_LLMPR, IL_LLMPR, LLMPR), 2)
  rownames(Limits) <- c("LLWR", "LLMPR")
  out <- list(CRITICAL_LIMITS = out1, LLRW_LLMPR = Limits)
  return(out)
}
















# NAVEGACAO 1 ------------------------------------------------------------------


output$FC1 <- renderPlot({
  
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(1, 1000), log="y", xlim=c(1,4),
       ylab = "", xlab = "", yaxt='n',
       type = "l")
  mtext(expression("h at the field capacity "~ (hPa)),2,line=2)
  mtext("n (Genuchten's parameters)",1,line=2.3)
  axis(2,at=c(1,10,100,1000))
  mtext("Genuchten's water retention curve", 3,line=1.5)
  
  y <- hFC(alpha=input$alpha1, n=seq(1,4,len=100))
  
  points(x=seq(1,4,len=100),y=y, type="l", lwd=2)
  h.FC <- hFC(alpha=input$alpha1, n=input$n1)
  
  segments(x0=input$n1,x1=input$n1,y0=0.1,y1=h.FC)
  segments(x0=-1,x1=input$n1,y0=h.FC,y1=h.FC)
  points(x=input$n1,y=h.FC, col=2, pch=15)
  
  
  
  legend("topright",legend=c("hFC",round(h.FC,2)))
  
  
})



# NAVEGACAO 2  -----------------------------------------------------------------

output$PWP1 <- renderPlot({
  
  
  DE <- function(theta_R,x,A1,A2,h1,h2) theta_R + A1 * exp(-x/h1) + A2 * exp(-x/h2)
  h <- seq(log10(1), log10(100000),len=100)
  y <- DE(x=10^h,theta_R = input$C,A1=input$A1,A2=input$A2,h1=input$h1,h2=input$h2)
  par(cex=0.9)
  plot(x=1,y=1,type="l", ylab="", xlab="", 
       xaxt="n", ylim=c(0,0.6),xlim=c(1,100000), log="x")
  x <- c(1,10,100,1000,10000,100000)
  axis(1, at=x, labels=as.character(x))
  points(x=10^h,y=y, type="l", lwd=2)
  mtext("Water content (w)", 2,line=2.4)
  mtext("| Matric potential | (h) (hPa)", 1,line=2.8)
  mtext("Dexter's water retention curve", 3,line=1.5)
  
  
  h.cut.off <- hydraulicCutOff.2(theta_R = input$C, 
                                 A1=input$A1,A2=input$A2,
                                 h1=input$h1,h2=input$h2)
  w <- DE(x=h.cut.off,theta_R = input$C,A1=input$A1,A2=input$A2,
          h1=input$h1,h2=input$h2)
  
  segments(x0=h.cut.off,x1=h.cut.off,y0=-1,y1=w)
  segments(x0=0.1,x1=h.cut.off,y0=w,y1=w)
  points(x=h.cut.off,y=w, col=2, pch=15)
  
  
  legend("topright",legend=c("hco",round(h.cut.off,0)))
  
  
  
})




output$PWPdata <- renderTable({
  
  hco <- c()
  if (input$PEDO.hco=="Dexter et al. (2012)") {hco <- 3.956 + 0.0129*input$CChco}
  if (input$PEDO.hco=="Czyz et al. (2012)") {hco <- 3.514 + 0.0250*input$CChco}

  
  m <- matrix(nrow=1,ncol=3)
  m[1,1] <- input$PEDO.hco
  m[1,2] <- round(hco,2)
  m[1,3] <- round(10^hco,0)
  colnames(m) <- c("PTF","hco (pF)", "hco (hPa)")
  m
  
})






# NAVEGACAO 3  -----------------------------------------------------------------



output$AFP1 <- renderPlot({
  
  
  AIR.cri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                          thetaS=input$TP)
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(0, 0.5), xlim=c(0.005,0.02),
       ylab = "", xlab = "", 
       type = "l")
  mtext(expression("Minimal air-filled porosity"~(m^3~m^-3)),2,line=2)
  mtext("Relative gas diffusivity (-)",1,line=2.3)
  
  y <- AIR.critical(mim.gas.difusion=seq(0.005,0.02,len=100), 
                    thetaS=input$TP)
  points(x=seq(0.005,0.02,len=100),y=y, type="l", lwd=2)
  AFPcri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                         thetaS=input$TP)
  
  segments(x0=input$mim.gas.difusion,x1=input$mim.gas.difusion,y0=-1,y1=AFPcri)
  segments(x0=-1,x1=input$mim.gas.difusion,y0=AFPcri,y1=AFPcri)
  points(x=input$mim.gas.difusion,y=AFPcri, col=2, pch=15)
  
  
  legend("topright",legend=c(expression(AFP[minimal]),round(AFPcri,4)))
  
  
})




output$plotTaylor <- renderPlot({
  
  if (input$cropTaylor =="Peanuts") {
    
    c <- 2.694
    b <- -0.084
    a <- 0.0007
    Q <- seq(3,48,len=50)
    f <- function (x) c + b*x + a*x^2
    y <- f(x=Q)
    ymax <- 2.694000
    plot(x=Q/10,y=(y/ymax )*100)
    
    data <- data.frame(Q=Q/10,y=(y/ymax)*100)
    R <- input$root.rateTaylor
    RET <- (y/ymax)*100
    value <- RET[which.min(abs(RET - R))]
    pos <- match(value,RET)
    PR.cri <- data[pos,1]
    
    plot(x=Q/10,y=(y/ymax )*100, ylab="", xlab="", 
         type="l", lwd=2, xlim=c(0,6), ylim=c(0,100))
    mtext("SPR (MPa)",1,line=2.5)
    mtext("Root elongation rate (%)",2,line=2.5)
    segments(x0=PR.cri,x1=PR.cri,y0=-10,y1=R, col=2)
    segments(x0=-1,x1=PR.cri,y0=R,y1=R, col=2)
    points(x=PR.cri,y=R, pch=15,col=2)
    legend("topright",legend=c(expression(SPR[critical]),round(PR.cri,2)))
    
    
  }
  
  
  
  
  if (input$cropTaylor =="Cotton") {
    
    c <- 3.523
    b <- -0.294
    a <- 0.008
    Q <- seq(1.2,20,len=50)
    f <- function (x) c + b*x + a*x^2
    y <- f(x=Q)
    ymax <- 3.5230
    plot(x=Q/10,y=(y/ymax )*100)
    
    data <- data.frame(Q=Q/10,y=(y/ymax)*100)
    R <- input$root.rateTaylor
    RET <- (y/ymax)*100
    value <- RET[which.min(abs(RET - R))]
    pos <- match(value,RET)
    PR.cri <- data[pos,1]
    
    plot(x=Q/10,y=(y/ymax )*100, ylab="", xlab="", 
         type="l", lwd=2, xlim=c(0,2), ylim=c(0,100))
    mtext("SPR (MPa)",1,line=2.5)
    mtext("Root elongation rate (%)",2,line=2.5)
    segments(x0=PR.cri,x1=PR.cri,y0=-10,y1=R, col=2)
    segments(x0=-1,x1=PR.cri,y0=R,y1=R, col=2)
    points(x=PR.cri,y=R, pch=15,col=2)
    
    legend("topright",legend=c(expression(SPR[critical]),round(PR.cri,2)))
    
  }
  
  

  
  
})





output$plotMoraes <- renderPlot({
  

  c.PR <- PR.Moraes(root.elongation.rate=input$root.rateMoraes/100,
                   x=input$fMoraes) 
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(0, 100), xlim=c(0,15),
       ylab = "", xlab = "", 
       type = "l")
  mtext("SPR (MPa)",1,line=2.5)
  mtext("Root elongation rate (%)",2,line=2.5)
  
  
  fator <- c()
  if (input$fMoraes=="Soil without biopores (-0.4325)") {fator <- -0.4325} 
  if (input$fMoraes=="Soil with biopores (-0.3000)") {fator <- -0.3000}
  
  
  f <- function (x,Q) exp(x*Q)
  y <- f(x=fator,Q=seq(0.1,15,len=100))
  points(x=seq(0.1,15,len=100),y=y*100, type="l", lwd=2)
  Qcri <- (log(input$root.rateMoraes/100))/fator
  
  segments(x0=Qcri,x1=Qcri,y0=-10,y1=input$root.rateMoraes)
  segments(x0=-10,x1=Qcri,y0=input$root.rateMoraes,y1=input$root.rateMoraes)
  points(x=Qcri,y=input$root.rateMoraes, col=2, pch=15)
  
  legend("topright",legend=c(expression(SPR[critical]),round(Qcri,2)))
  
  
})



output$plotVeen <- renderPlot({
  
  
  S <- c()
  if (input$MPVeen=="-0.01 MPa") {S <- -0.01}
  if (input$MPVeen=="-0.10 MPa") {S <- -0.10}
  if (input$MPVeen=="-0.63 MPa") {S <- -0.63}
  
  
  maxS1 <- 47.72 # -0.01
  maxS2 <- 45.2 # -0.1
  maxS3 <- 30.36 # 0.63
  maxS <- c()
  if (input$MPVeen=="-0.01 MPa") {maxS <- maxS1}
  if (input$MPVeen=="-0.10 MPa") {maxS <- maxS2}
  if (input$MPVeen=="-0.63 MPa") {maxS <- maxS3}
  
  
  
  
  c.PR <- PR.Veen(root.elongation.rate=input$root.rateVeen, 
                  suction=input$MPVeen)
  
  
  lim <- c()
  if (input$MPVeen=="-0.01 MPa") {lim <- 4}
  if (input$MPVeen=="-0.10 MPa") {lim <- 4}
  if (input$MPVeen=="-0.63 MPa") {lim <- 2.5}
  
  
  par(cex = 0.9)
  plot(y = 1, x = 1, ylim = c(0, 100), xlim=c(0,lim),
       ylab = "", xlab = "", 
       type = "l")
  mtext("SPR (MPa)",1,line=2.5)
  mtext("Root elongation rate (%)",2,line=2.5)
  

  
  Q <- c()
  if (input$MPVeen=="-0.01 MPa") {Q <- seq(0,3.5,len=100)}
  if (input$MPVeen=="-0.10 MPa") {Q <- seq(0,3.5,len=100)}
  if (input$MPVeen=="-0.63 MPa") {Q <- seq(0,2.5,len=100)}
  
  
  y <- ((48 + 28*S - 12*Q)/maxS)*100
  points(x=Q,y=y, type="l", lwd=2)
  
  segments(x0=c.PR,x1=c.PR,y0=-10,y1=input$root.rateVeen)
  segments(x0=-10,x1=c.PR,y0=input$root.rateVeen,y1=input$root.rateVeen)
  points(x=c.PR,y=input$root.rateVeen, col=2, pch=15)
  
  legend("topright",legend=c(expression(SPR[critical]),round(c.PR,2)))

  
})





output$values2 <- renderTable({
  
  c.PR <- PR.critical(root.elongation.rate=input$root.rate/100,
                      x=input$rate.factor) 
  
  AIR.cri <- AIR.critical(mim.gas.difusion=input$mim.gas.difusion, 
                          thetaS=input$TP)
  
  
  out <- data.frame("Critical SPR"=c.PR, "Critical AFP" = AIR.cri)
  out
  
})



# ------------------------------------------------------------------------------











# NAVEGACAO 3  -----------------------------------------------------------------



output$plot5 <- renderPlot({
  
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2, 
                     d=input$d, e=input$e, f=input$f, 
                     critical.PR=input$pr, 
                     PD=input$PD,
                     h.FC=input$fc, h.PWP=input$pwp, 
                     air.porosity=input$air)
  
  par(cex = 0.9, mar = c(4, 6, 2, 6))
  plot(y = 1, x = 1, xlim = c(0, 1), pch = 15, ylim=c(0,0.8),
       ylab = "", xlab = "",xaxt = "n", 
       type = "l", cex = 0.9)
  mtext(expression(theta~(m^3~m^-3)), side = 2, 
        line = 2.2, las = 3, cex = 0.9)
  
  la <- round((1 - input$thetaS2) * input$PD, 2)
  mtext(expression("Bulk density" ~ (Mg~m^3)), side = 1, line = 2.2, cex = 0.9)
  
  axis(1, at = 0.5, labels = la)
  x <- c(0, 1, 1, 0)
  yU <- LLWR$LLRW_LLMPR[1,1]
  yL <- LLWR$LLRW_LLMPR[1,2]
  y <- c(yL, yL, yU, yU)
  
  RANGE <- LLWR$LLRW_LLMPR[1,3]
  
  if (RANGE > 0) {  polygon(x, y, col = "gray")}
  
  points(x = rep(0.5, 4), 
         y = c(LLWR$CRITICAL_LIMITS[1,1], LLWR$CRITICAL_LIMITS[2,1], 
               LLWR$CRITICAL_LIMITS[3,1], LLWR$CRITICAL_LIMITS[4,1]), 
         pch = 15)
  points(x = rep(0.5, 2), 
         y = c(LLWR$LLRW_LLMPR[1,1], LLWR$LLRW_LLMPR[1,2]), col = 2, 
         pch = 15)
  f <- 0.15
  labels = c(expression(theta[AFP]), expression(theta[FC]), 
             expression(theta[PWP]), expression(theta[SPR]))
  text(labels, x = c(0.5 + f, 0.5 - f, 0.5 - f, 0.5 + f), 
       y = c(LLWR$CRITICAL_LIMITS[1,1], LLWR$CRITICAL_LIMITS[2,1], 
             LLWR$CRITICAL_LIMITS[3,1], LLWR$CRITICAL_LIMITS[4,1]), cex = 1.2)
  
  legend("topright",legend=c("LLWR",round(RANGE,4)))
  
  
})





output$plot6 <- renderPlot({
  
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  
  
  par(cex = 0.9, mar = c(4, 6, 2, 6))
  plot(y = 1, x = 1, xlim = c(0, 1), log = "y", pch = 15, ylim=c(100000,1),
       ylab = "", xlab = "", xaxt = "n", 
       type = "l", cex = 0.9)
  mtext(expression(h~(hPa)), side = 2, line = 2.2, las = 3, cex = 0.9)
  axis(2, at = c(1, 10, 100, 1000, 10000,100000), 
       labels=c(1, 10, 100, 1000, 10000,expression(10^5)))
  
  
  la <- round((1 - input$thetaS2) * input$PD, 2)
  mtext(expression("Bulk density" ~ (Mg~m^3)), side = 1, line = 2.2, cex = 0.9)
  
  axis(1, at = 0.5, labels = la)
  x <- c(0, 1, 1, 0)
  yU <- LLWR$LLRW_LLMPR[2,1]
  yL <- LLWR$LLRW_LLMPR[2,2]
  y <- c(yL, yL, yU, yU)
  
  RANGE <- LLWR$LLRW_LLMPR[2,3]
  
  if (RANGE > 0) {  polygon(x, y, col = "gray")}
  
  points(x = rep(0.5, 4), 
         y = c(LLWR$CRITICAL_LIMITS[1,2], LLWR$CRITICAL_LIMITS[2,2], 
               LLWR$CRITICAL_LIMITS[3,2], LLWR$CRITICAL_LIMITS[4,2]), 
         pch = 15)
  points(x = rep(0.5, 2), 
         y = c(LLWR$LLRW_LLMPR[2,1], LLWR$LLRW_LLMPR[2,2]), col = 2, 
         pch = 15)
  f <- 0.15
  labels = c(expression(h[AFP]), expression(h[FC]), 
             expression(h[PWP]), expression(h[SPR]))
  text(labels, x = c(0.5 + f, 0.5 - f, 0.5 - f, 0.5 + f), 
       y = c(LLWR$CRITICAL_LIMITS[1,2], LLWR$CRITICAL_LIMITS[2,2], 
             LLWR$CRITICAL_LIMITS[3,2], LLWR$CRITICAL_LIMITS[4,2]), cex = 1.2)
  
  legend("topright",legend=c("LLMPR",round(RANGE,4)))
  
  
})




output$values3 <- renderTable({
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  data <- matrix(nrow=2,ncol=5)
  
  waterlimits <- format(c(LLWR$CRITICAL_LIMITS[1,1],
                        LLWR$CRITICAL_LIMITS[2,1],
                        LLWR$CRITICAL_LIMITS[3,1], 
                        LLWR$CRITICAL_LIMITS[4,1]),digits = 4)

  data[1,] <- c("LLWR", waterlimits)
  
  
  hlimits <- format(c(LLWR$CRITICAL_LIMITS[1,2],
               LLWR$CRITICAL_LIMITS[2,2],
               LLWR$CRITICAL_LIMITS[3,2], 
               LLWR$CRITICAL_LIMITS[4,2]),digits = 1)
  
  data[2,] <- c("LLMPR",hlimits)
  colnames(data) <- c("-","AFP","FC","PWP","SPR")
  data
  
  })



output$values4 <- renderTable({
  
  LLWR <- llwr_llmpr(thetaR=input$thetaR2, thetaS=input$thetaS2, 
                     alpha=input$alpha2, n=input$n2,
                     d=input$d, e=input$e, f=input$f, 
                     PD=input$PD,
                     critical.PR=input$pr, h.FC=input$fc, 
                     h.PWP=input$pwp, air.porosity=input$air)
  
  data <- matrix(nrow=2,ncol=4)
  
  waterlimits <- format(c(LLWR$LLRW_LLMPR[1,1],
                          LLWR$LLRW_LLMPR[1,2], 
                          LLWR$LLRW_LLMPR[1,3]),digits = 4)
  
  data[1,] <- c("LLWR", waterlimits)
  
  
  hlimits <- format(c(LLWR$LLRW_LLMPR[2,1],
                      LLWR$LLRW_LLMPR[2,2], 
                      LLWR$LLRW_LLMPR[2,3]),digits = 1)
  
  data[2,] <- c("LLMPR",hlimits)
  colnames(data) <- c("-","Upper","Lower","Range")
  data
  
})

}


shinyApp(ui, server)
