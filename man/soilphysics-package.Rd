\name{soilphysics-package}
\alias{soilphysics-package}
\alias{soilphysics}
\docType{package}

\title{ Soil Physical Analysis }

\description{
Basic and model-based soil physical analyses. 
}

\details{
\tabular{ll}{
Package: \tab soilphysics\cr
Type: \tab Package\cr
Version: \tab 4.1\cr
Date: \tab 2021-11-21\cr
License: \tab GPL (>= 2)\cr
}
Functions for modelling the load bearing capacity and the penetration resistance, and for predicting the 
stress applied by agricultural machines in the soil profile. The package allows one to model the soil water 
retention through six different models. There are some useful and easy-to-use functions to perform parameter 
estimation of these models. Methods to obtain the preconsolidation stress are available, such as the standard 
of Casagrande (1936) and so on. It is possible to quantify soil water availability for plants through the Least Limiting Water Range 
approach as well as the Integral Water Capacity. Moreover, it is possible to determine the water suction 
at the point of hydraulic cut-off. Also, users can deal with the high-energy-moisture-characteristics 
(HEMC) methodology proposed by Pierson and Mulla (1989), which is used to analyze the aggregate stability. 
There is a function to determine the soil critical moisture and the maximum bulk density for one or more 
samples, based on the Proctor (1933) compaction test. Other utilities like a function to calculate the 
soil liquid limit, the void ratio and to determine the maximum curvature point are available.
}

\note{
\emph{soilphysics} is an ongoing project.
We welcome any and all criticism, comments and suggestions.
}

\author{
Anderson Rodrigo da Silva, Renato Paiva de Lima

Maintainer: Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\references{
da Silva, A.R.; de Lima, R.P. (2015) soilphysics: an R package to determine soil preconsolidation pressure. 
\emph{Computers and Geosciences}, 84: 54-60.

de Lima, R.P.; da Silva, A.R.; da Silva, A.P.; Leao, T.P.; Mosaddeghi, M.R. (2016) soilphysics: an R package 
for calculating soil water availability to plants by different soil physical indices.
\emph{Computers and Eletronics in Agriculture}, 120: 63-71.

da Silva, A.R.; Lima, R.P. (2017) Determination of maximum curvature point with the R package 
soilphysics. \emph{International Journal of Current Research}, 9: 45241-45245. 

De Lima, R.P.; Da Silva, A.R.; Da Silva, A.P. (2021) soilphysics: An R package for simulation of soil compaction induced by agricultural field traffic. 
	\emph{SOIL and TILLAGE RESEARCH}, 206: 104824. 

}
