\name{soilStrength5}
\alias{soilStrength5}

\title{ Estimation of the Load Bearing Capacity by precompression stress from readily available
soil properties}

\description{
Calculates the soil strength using precompression stress using the pedo-transfer function from Imhoff et al. (2004)
}

\usage{ 
soilStrength5(bulk.density,  water.content, clay.content)
}

\arguments{
  \item{bulk.density}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}. 
  \item{water.content}{ a numeric vector containing the values of water content, \(g/g).
  \item{clay.content}{ a numeric vector containing the values of clay content, \%. 
}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built.
}

\references{
Imhoff, S., Da Silva, A. P., Fallow, D., 2004. 
Susceptibility to Compaction, Load Support Capacity, and Soil Compressibility of Hapludox. 
\emph{Soil Science Society of America Journal }, 68:17-24.


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

soilStrength5(clay.content=60, water.content=0.30, bulk.density=1.25)
soilStrength5(clay.content=35, water.content=0.23, bulk.density=1.40)
soilStrength5(clay.content=20, water.content=0.10, bulk.density=1.60)



# EXEMPLE 2
water.content <- seq(0.1,0.30,len=20) # range of water content from 0.1 to 0.30 (g g^-1)
out <- soilStrength5(clay.content=20, water.content=water.content , bulk.density=1.60)
plot(x=water.content,y=out, 
     ylab="Precompression stress (kPa)", xlab="Water content")


# End (not run)
}
