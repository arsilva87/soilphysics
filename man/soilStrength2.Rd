\name{soilStrength2}
\alias{soilStrength2}

\title{ Estimation of precompression stress by Schjonning and Lamande (2018)}

\description{
It calculates the precompression stress using the pedo-transfer function from Schjonning and Lamande (2018)
}

\usage{ 
soilStrength2(bulk.density,  matric.suction, clay.content)
}

\arguments{
  \item{clay.content}{ a numeric vector containing the values of clay content, \eqn{%}}
  \item{matric.suction}{ a numeric vector containing the values of matric suction, hPa}
  \item{bulk.density}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}}
}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built.
}

\references{
Schjonning, P.; Lamande, M., 2018. 
Models for prediction of soil precompression stress from readily available soil properties. 
\emph{Geoderma}, 320:115-125.


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
