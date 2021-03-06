\name{soilStrength4}
\alias{soilStrength4}

\title{Estimation of precompression stress by Lebert and Horn (1991)}

\description{
It calculates the soil strength through precompression stress using the pedo-transfer function from Lebert and Horn (1991)
}

\usage{ 
soilStrength4(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,
              OM=1.5,C=30,phi=36,texture="Clay>35", pF=1.8)
}

\arguments{
  \item{BD}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}} 
  \item{AC}{ a numeric vector containing the values of volumetric air capacity at the specified pF, \eqn{\%}} 
  \item{AWC}{ a numeric vector containing the values of volumetric available water at the specified pF, \eqn{\%}} 
  \item{PWP}{ a numeric vector containing the values of volumetric non available water capacity (pF > 4.2), \eqn{\%}} 
  \item{Ks}{ a numeric vector containing the values of saturated hydraulic conductivity, \eqn{10^3 cm s^{-1}} }
  \item{OM}{ a numeric vector containing the values of organic matter, \eqn{\%}} 
  \item{C}{ a numeric vector containing the values of cohesion at the specified pF, kPa}
  \item{phi}{ a numeric vector containing the values of angle of internal friction at the specified pF, degree}
  \item{texture}{the soil texture classification. See details} 
  \item{pF}{the '1.8' or '2.5' value pF} 
}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns '0' for soil properties values beyond the range for which the function was built. The default for this function is the values given in the application example by Horn and Fleige (2003).
In the 'texture' argument, the user must pass the textural classification 'Sand','SandLoam', 'Silt', 'Clay<35' or 'Clay>35'. See examples.
}

\references{
Lebert, M., Horn, R. (1991) A method to predict the mechanical strength of agricultural soils. 
\emph{Soil and Tillage Research}, 19: 275-256.
 
Horn, R., Fleige, H. (2003) A method for assessing the impact of load on mechanical stability and on physical properties of soils. 
\emph{Soil and Tillage Research}, 73: 89-99.
}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 
}

\seealso{
\code{\link{stressTraffic}}
}
  
  
\examples{
soilStrength4(BD=1.55,AC=10,AWC=15,PWP=26,Ks=0.29,OM=1.5,
              C=30,phi=36,texture="Clay>35", pF=1.8) # Exemple from Horn and Fleige (2003), Table 7

# End (not run)
}


