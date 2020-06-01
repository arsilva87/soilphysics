\name{soilStrength4}
\alias{soilStrength4}

\title{ Estimation of the Load Bearing Capacity by precompression stress from readily available
soil properties}

\description{
Calculates the soil strength using precompression stress using the pedo-transfer function from Lebert & Horn (1991)
}

\usage{ 
soilStrength4(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,OM=1.5,C=30,phi=36,texture="Clay>35", pF=1.8)
}

\arguments{
  \item{BD}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}. 
  \item{texture}{the soil texture classification}. 
}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built.
}

\references{
Saffih-Hdadi, K., Défossez, P., Richard, G., Cui, Y. J., Tang, A. M., & Chaplain, V., 2009. 
A method for predicting soil susceptibility to the compaction of surface layers as a function of water content and bulk density. 
\emph{Soil and Tillage Research}, 105:93-106.


}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 
}

\seealso{
\code{\link{stressTraffic}}
}

\examples{
# EXEMPLE 1
soilStrength3(bulk.density=1.1, water.content=40, texture="VeryFine")
soilStrength3(bulk.density=1.2, water.content=20, texture="Fine")
soilStrength3(bulk.density=1.3, water.content=15, texture="MediumFine")
soilStrength3(bulk.density=1.4, water.content=15, texture="Medium")
soilStrength3(bulk.density=1.5, water.content=10, texture="Coarse")


# EXEMPLE 2
water.content <- seq(5,30,len=20) # range of water content from 5 to 30 %
out <- soilStrength3(bulk.density=1.4, water.content=water.content, texture="MediumFine")
plot(x=water.content,y=out, 
     ylab="Precompression stress (kPa)", xlab="Water content (%)")

# End (not run)
}
