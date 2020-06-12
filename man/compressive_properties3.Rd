\name{compressive_properties3}
\alias{compressive_properties3}

\title{Estimation of compressive properties by de Lima et al. (2018)}

\description{
It calculates the compressive parameters N, lambda and kappa using the pedo-transfer function from de Lima et al. (2018)
}

\usage{ 
compressive_properties3(bulk.density, matric.suction, soil=c("SandyLoam","SandyClayLoam"))
}

\arguments{
  \item{bulkd.density}{ a numeric vector containing the values of bulk density, \eqn{Mg m^{-3}}} 
  \item{matric.suction}{ a numeric vector containing the values of matric suction, hPa} 
  \item{soil}{ the soil texture group 'SandyLoam' or 'SandyClayLoam'. See exemples}
}

\value{
  \item{N}{the specific volume at \eqn{p = 1 kPa}, N}
  \item{CI}{the compression index, lambda}
  \item{k}{the recompression index, kappa}
}

\details{
Pedo-transfer function deveploed under no-till condition. See de Lima et al. (2018)
}

\references{
de Lima, R. P., da Silva, A. P., Giarola, N. F., da Silva, A. R., Rolim, M. M., Keller, T., 2018.
Impact of initial bulk density and matric suction on compressive properties of two Oxisols under no-till. 
\emph{Soil and Tillage Research}, 175:168-177.

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

compressive_properties3(bulk.density=1.5, matric.suction=100, soil="SandyLoam")
compressive_properties3(bulk.density=1.5, matric.suction=100, soil="SandyClayLoam")


# EXAMPLE 2 for SandyLoam soil

matric.suction <- seq(from=30,to=1000,len=100)
out <- compressive_properties3(bulk.density=1.5, matric.suction=matric.suction, soil="SandyLoam")
plot(x=matric.suction,y=out$N, ylab="N", xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, ylab="kappa", xlab="Matric suction (hPa)", log="x") # plot for kappa


# EXAMPLE 3 for SandyClayLoam soil

matric.suction <- seq(from=30,to=1000,len=100)
out <- compressive_properties3(bulk.density=1.5, 
                               matric.suction=matric.suction, soil="SandyClayLoam")
plot(x=matric.suction,y=out$N, ylab="N", 
     xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, 
     ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, 
     ylab="kappa", xlab="Matric suction (hPa)", log="x") # plot for kappa


# End (not run)
}
