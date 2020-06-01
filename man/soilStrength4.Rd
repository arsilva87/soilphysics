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
  \item{AC}{ a numeric vector containing the values of volumetric air capacity at the specified pF, \%}. 
  \item{AWC}{ a numeric vector containing the values of volumetric available water at the specified pF, \%}. 
  \item{PWP}{ a numeric vector containing the values of volumetric non available water capacity (pF > 4.2), \%}.
  \item{Ks}{ a numeric vector containing the values of saturated hydraulic conductivity, \eqn{10^3 cm s^{-1}}. 
  \item{OM}{ a numeric vector containing the values of organic matter, \%}.
  \item{C}{ a numeric vector containing the values of cohesion at the specified pF, \kPa}.
  \item{phi}{ a numeric vector containing the values of angle of internal friction at the specified pF, \degree}.
  \item{texture}{the soil texture classification. See details}. 
  \item{texture}{the 1.8 or 2.5 value pF}. 
}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built. The default for this function is the values given in the application example by Horn & Fleige (2003).
In the 'texture' argument, the user must pass the textural classification 'Sand','SandLoam', 'Silt', 'Clay<35' or 'Clay>35'. See exemples.
}

\references{
Lebert, M., Horn, R., 1991. 
A method to predict the mechanical strength of agricultural soils. 
\emph{Soil and Tillage Research}, 19:275-256.
 
Horn, R., Fleige, H., 2003. 
A method for assessing the impact of load on mechanical stability and on physical properties of soils. 
\emph{Soil and Tillage Research}, 73:89–99.


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

soilStrength4(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,OM=1.5,C=30,phi=36,texture="Clay>35", pF=1.8) # Exemple from Horn & Fleige (2003), Table 7


# EXEMPLE 2 - computing the effect of avaiable water capacity - (AWC)
AWC <- seq(from=5,to=25, len=20) # range of avaiable water capacity from 5 to 20 %
out <- soilStrength4(BD=1.55,AC=10,AWC=AWC,PWP=26,Ks=0.29,OM=1.5,C=30,phi=36,texture="Clay>35", pF=1.8)
plot(x=AWC,y=out, 
     ylab="Precompression stress (kPa)", xlab="Avaiable water capacity (%)")


# End (not run)
}
