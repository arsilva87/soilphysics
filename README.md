# About

In this package you will find functions for modelling the load bearing capacity and the soil penetration resistance, and for predicting the stress applied by agricultural machines in the soil profile. The package allows one to model the soil water retention through six different models. There are some useful and easy-to-use functions to perform parameter estimation of these models. Methods to obtain the preconsolidation stress are available, such as the standard of Casagrande (1936) and so on. It is possible to quantify soil water availability for plants through the Least Limiting Water Range approach as well as the Integral Water Capacity. Moreover, it is possible to determine the water suction at the point of hydraulic cut-off. Also, users can deal with the high-energy-moisture-characteristics (HEMC) methodology proposed by Pierson & Mulla (1989), which is used to analyze the aggregate stability. There is a function to determine the soil critical moisture and the maximum bulk density for one or more samples, based on the Proctor (1933) compaction test. Other utilities like a function to calculate the soil liquid limit, the void ratio and to determine the maximum curvature point are available. 

# Instalation

You can install and load the released version of soilphysics from GitHub with:

```r
install.packages("devtools")

devtools::install_github("arsilva87/soilphysics")

library(soilphysics)
```

## Soil compaction tools

Add text here...

```r
script here

```

![](<man/figures/SMP.jpeg>)

## Soil water tools

Add text here...

```r
iwc(theta_R = 0.166, theta_S = 0.569, alpha = 0.029, n = 1.308, 
  a = 0.203, b = 0.256, hos = 200, graph = TRUE)
                     IWC       EI         h.Range
EKa(h, hos)    0.0144000   0.9600  66.43 - 139.49
EK(h, hos)     0.0405000   5.3700    139.49 - 330
C(h, hos)      0.0846000  49.4800   330 - 2471.44
ER(h, hos)     0.0288000  87.1200 2471.44 - 15000
ERKdry(h, hos) 0.0006000   4.9100   12000 - 15000
Sum            0.1689139 147.8336       0 - 15000

```

## Miscellanea

Add text for other functions...

# Spin-off

A shiny app for simulating soil compaction: <https://renatoagro.shinyapps.io/PredCompAPP/>

A shiny app for the assessment of compaction risk: <https://renatoagro.shinyapps.io/PredCompAPP/>

# Citation and references
Da Silva, A.R.; De Lima, R.P. (2015) soilphysics: an R package to determine soil preconsolidation pressure. *Computers and Geosciences*, 84: 54-60. DOI: <https://doi.org/10.1016/j.cageo.2015.08.008>

De Lima, R.P.; Da Silva, A.R.; Da Silva, A.P.; Leao, T.P.; Mosaddeghi, M.R. (2016) soilphysics: an R package for calculating soil water availability to plants by different soil physical indices. *Computers and Eletronics in Agriculture*, 120: 63-71. DOI: <https://doi.org/10.1016/j.compag.2015.11.003>

Da Silva, A.R.; De Lima, R.P. (2017) Determination of maximum curvature point with the R package soilphysics. *International Journal of Current Research*, 9: 45241-45245.  

# Contributions and bug reports

soilphysics is an ongoing project. Then, contributions are very welcome. If you have a question or have found a bug, please open an ![Issue](<https://github.com/arsilva87/soilphysics/issues>) or reach out directly by e-mail: <anderson.silva@ifgoiano.edu.br> or <renato_agro_@hotmail.com>.
