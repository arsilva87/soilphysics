\name{compressive_properties2}
\alias{compressive_properties2}

\title{ Estimation of compressive properties by Keller and Arvidsson (2007)}

\description{
It calculates the compressive parameters N and lambda using the pedo-transfer function from Keller and Arvidsson (2007)
}

\usage{ 
compressive_properties2(particle.density, bulk.density)
}

\arguments{
  \item{particle.density}{ a numeric vector containing the values of particle density, \eqn{Mg m^{-3}}}
  \item{bulk.density}{ a numeric vector containing the values of bulk density, \eqn{Mg m^{-3}}}
}

\value{
  \item{N}{the specific volume at \eqn{p = 1 kPa}, N}
  \item{CI}{the compression index, lambda}
}

\details{
In Keller and Arvidsson (2007), the recompression index, kappa, was found as 0.042 for all soil.
}

\references{
Keller, T., Arvidsson, J., 2007. 
Compressive properties of some Swedish and Danish structured agricultural soils measured in uniaxial compression tests. 
\emph{European Journal of Soil Science }, 58: 1373-1381.

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

 compressive_properties2(particle.density=2.65, bulk.density=1.5)

# EXAMPLE 2

BD <- seq(from=1.2,to=1.8, by=0.01) # range of bulk density from 1.2 to 1.8
out <- compressive_properties2(particle.density=2.65, bulk.density=BD)

plot(x=BD,y=out$N, ylab="N", xlab="Bulk density") # for N
plot(x=BD,y=out$CI, ylab="Compression index (CI)", xlab="Bulk density") # for compression index

# End (not run)
}
