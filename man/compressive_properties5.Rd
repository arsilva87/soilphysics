\name{compressive_properties5}
\alias{compressive_properties5}

\title{ Estimation of compressive properties (from soil compression curve) from readily available soil properties}

\description{
Calculates the compressive parameteres N, lambda and kappa using the pedo-transfer function from O’Sullivan et al. (1999)
}

\usage{ 
compressive_properties5(water.content, soil=c("SandyLoam","ClayLoam"))
}

\arguments{
  \item{water.content}{ a numeric vector containing the values of water content, \(g/g,%). 
  \item{soil}{the the soil texture group 'SandyLoam' or 'ClayLoam'. See exemples.
}

\value{
  \item{N}{the specific volume at p = 1 kPa, N}
  \item{CI}{the compression index, lambda}
  \item{k}{the recompression index, kappa}
}

\details{
See de O’Sullivan et al. (1999).
}

\references{
O'sullivan, M. F., Henshall, J. K., & Dickson, J. W., 1999.
A simplified method for estimating soil compaction. 
\emph{Soil and Tillage Research}, 49:325-335.

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
water.content <- 15
compressive_properties5(water.content=water.content, soil="SandyLoam") 
compressive_properties5(water.content=water.content, soil="ClayLoam") 


# EXEMPLE 2 - SandyLoam

water.content <- seq(from=5,to=20,len=20)
out <- compressive_properties5(water.content=water.content, soil="SandyLoam") 
plot(x=water.content ,y=out$N, ylab="N", xlab="Bulk density") # plot for N
plot(x=water.content ,y=out$lambda, ylab="lambda", xlab="Bulk density") # plot for compression index, lambda
plot(x=water.content ,y=out$kappa, ylab="kappa", xlab="Bulk density") # plot for recompression index, kappa


# EXEMPLE 3 - ClayLoam 

water.content <- seq(from=10,to=25,len=20)
out <- compressive_properties5(water.content=water.content, soil="ClayLoam") 
plot(x=water.content ,y=out$N, ylab="N", xlab="Bulk density") # plot for N
plot(x=water.content ,y=out$lambda, ylab="lambda", xlab="Bulk density") # plot for compression index, lambda
plot(x=water.content ,y=out$kappa, ylab="kappa", xlab="Bulk density") # plot for recompression index, kappa





# End (not run)
}
