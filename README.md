# About
In this package you will find a series of functions for soil physics data analysis. These functions includes five models of water retention curve, seven methods of soil precompression stress, least limiting water range (LLWR), Integral Water Capacity (IWC), soil penetration resistance curve by Busscher's model, calculation of Soil Aggregate-Size Distribution, S Index, critical soil moisture and maximum bulk density using data from Proctor test, calculation of equivalent pore radius as a function of soil water tension, simulation of sedimentation time of soil particles through Stokes' law, simulation of soil pore size distribution, calculation of the hydraulic cut-off introduced by  Dexter et al. (2008) and simulation of soil compaction induced by agricultural field traffic. Other utilities like functions to calculate the void ratio and to determine the maximum curvature point are available. 

# Instalation

You can install and load the released version of soilphysics from GitHub with:

```r
install.packages("devtools")

devtools::install_github("arsilva87/soilphysics")

library(soilphysics) 
```

## Soil compaction tools

Unsing the funtion soilDeformation, it is possible calculates the bulk density variation as a function of the applied mean normal stress using critical state theory, by O'Sullivan and Robertson (1996). 

```r

# Usage
soilDeformation(stress = 300,
                p.density = 2.67,
	        iBD = 1.55, 
                N = 1.9392, 
                CI = 0.06037, 
                k = 0.00608, 
                k2 = 0.01916,
                m = 1.3,graph=TRUE,ylim=c(1.4,2.0))
		
# Results
   iBD    fBD     vi     vf   I%
1 1.55 1.6385 1.7226 1.6295 5.71

```

![](<man/figures/SMP.jpeg>)


## Soil water tools

Quantifying the soil water availability for plants through the IWC approach:

```r

# Usage
iwc(theta_R = 0.166, theta_S = 0.569, alpha = 0.029, n = 1.308, 
    a = 0.203, b = 0.256, hos = 200, graph = TRUE)
 
# Results
                     IWC       EI         h.Range
EKa(h, hos)    0.0144000   0.9600  66.43 - 139.49
EK(h, hos)     0.0405000   5.3700    139.49 - 330
C(h, hos)      0.0846000  49.4800   330 - 2471.44
ER(h, hos)     0.0288000  87.1200 2471.44 - 15000
ERKdry(h, hos) 0.0006000   4.9100   12000 - 15000
Sum            0.1689139 147.8336       0 - 15000

```

Quantifying the soil water availability for plants through the LLWR approach:

```r
# Usage
data(skp1994)
ex1 <- with(skp1994,
	 llwr(theta = W, h = h, Bd = BD, Pr = PR,
		particle.density = 2.65, air = 0.1,
		critical.PR = 2, h.FC = 100, h.WP = 15000))
```		
![](https://github.com/arsilva87/soilphysics/blob/master/man/figures/LLWR.jpg)


## Precompression stress

Estimating the precompression stress by several methods:
![](https://github.com/arsilva87/soilphysics/blob/master/man/figures/sigmaPfluxo.jpg)
```r
pres <- c(1, 12.5, 25, 50, 100, 200, 400, 800, 1600)
VR <- c(0.846, 0.829, 0.820, 0.802, 0.767, 0.717, 0.660, 0.595, 0.532)

# Usage
sigmaP(VR, pres, method = "casagrande", n4VCL = 2)

# Results
Preconsolidation stress: 104.2536
Method: casagrande, with mcp equal to 1.7885
Compression index: 0.2093
Swelling index: 0.0179 
```

## Soil water retention curve

Fitting water retention curve using van Genuchten's model

```r
h <- c(0.001, 50.65, 293.77, 790.14, 992.74, 5065, 10130, 15195)
w <- c(0.5650, 0.4013, 0.2502, 0.2324, 0.2307, 0.1926, 0.1812, 0.1730)

# Usage
fitsoilwater(theta=w, x=h, ylim=c(0.1,0.6))

# Results
Parameters:
        Estimate Std. Error t value Pr(>|t|)    
theta_R  0.16761    0.01272  13.179 0.000192 ***
theta_S  0.56531    0.01092  51.786 8.32e-07 ***
alpha    0.04748    0.01177   4.035 0.015671 *  
n        1.52926    0.09579  15.965 9.00e-05 ***
---
```
![](https://github.com/arsilva87/soilphysics/blob/master/man/figures/WRC.png)

## Miscellanea

Add text for other functions...

# Spin-off

A shiny app for simulating soil compaction: <https://renatoagro.shinyapps.io/PredCompAPP/>

A shiny app for the assessment of compaction risk: <https://renatoagro.shinyapps.io/trafficAPP/>

Sedimentation time of soil particle (Stokes' law): <https://renatoagro.shinyapps.io/stokesapp/>

Exploring water retention curve using van Genuchten's model: <https://soilphysics.shinyapps.io/wrcAPP/>

Soil Aggregate-Size Distribution: <https://renatoagro.shinyapps.io/Agre/>

Least Limiting Water Range (LLWR): <https://soilphysics.shinyapps.io/LLWRAPP/>

Water suction at the point of hydraulic cut-off (Dexter et al. 2012): <https://soilphysics.shinyapps.io/h_cutoff/>



# Citation and references
Da Silva, A.R.; De Lima, R.P. (2015) soilphysics: an R package to determine soil preconsolidation pressure. *Computers and Geosciences*, 84: 54-60. DOI: <https://doi.org/10.1016/j.cageo.2015.08.008>

De Lima, R.P.; Da Silva, A.R.; Da Silva, A.P.; Leao, T.P.; Mosaddeghi, M.R. (2016) soilphysics: an R package for calculating soil water availability to plants by different soil physical indices. *Computers and Eletronics in Agriculture*, 120: 63-71. DOI: <https://doi.org/10.1016/j.compag.2015.11.003>

Da Silva, A.R.; De Lima, R.P. (2017) Determination of maximum curvature point with the R package soilphysics. *International Journal of Current Research*, 9: 45241-45245.  



# Contributions and bug reports

soilphysics is an ongoing project. Then, contributions are very welcome. If you have a question or have found a bug, please open an ![Issue](<https://github.com/arsilva87/soilphysics/issues>) or reach out directly by e-mail: <anderson.silva@ifgoiano.edu.br> or <renato_agro_@hotmail.com>.
