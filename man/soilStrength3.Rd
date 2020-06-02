\name{soilStrength3}
\alias{soilStrength3}

\title{ Estimation of precompression stress by Saffih-Hdadi et al. (2009)}

\description{
It calculates the precompression stress using the pedo-transfer function from Saffih-Hdadi et al. (2009)
}

\usage{ 
soilStrength3(bulk.density, water.content, texture=c("VeryFine","Fine","MediumFine","Medium","Coarse"))

}

\arguments{
  \item{bulk.density}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}}
  \item{water.content}{ a numeric vector containing the values of gravimetric water content, \eqn{\%}}
  \item{matric.suction}{ the soil texture group. See exemples}

}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built.
}

\references{
Saffih-Hdadi, K.; Defossez, P.; Richard, G.; Cui, Y. J.; Tang, A. M.; Chaplain, V, 2009.
A method for predicting soil susceptibility to the compaction of surface layers as a function of water content and bulk density 
\emph{Geoderma}, 115:96-103.


}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 
}

\seealso{
\code{\link{stressTraffic}}
}

\examples{
  
# EXAMPLE 1

soilStrength2(bulk.density=1.46, matric.suction=100, clay.content=0.3)


# EXAMPLE 2

matric.suction <- seq(from=60,to=1000,len=50) # range of matric suction from 60 to 1200 hPa
out <- soilStrength2(bulk.density=1.46, matric.suction=matric.suction, clay.content=0.3)
plot(x=matric.suction,y=out, 
     ylab="Precompression stress (kPa)", xlab="Matric suction (hPa)")

# End (not run)
}
