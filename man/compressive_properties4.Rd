\name{compressive_properties4}
\alias{compressive_properties4}

\title{ Estimation of compressive properties (from soil compression curve) from readily available soil properties}

\description{
Calculates the compressive parameteres N, lambda and kappa using the pedo-transfer function from de Lima et al. (2020)
}

\usage{ 
compressive_properties4(matric.suction, soil=c("PloughLayer","PloughPan"))
}

\arguments{
  \item{matric.suction}{ a numeric vector containing the values of matric suction, \hPa. 
  \item{soil}{the soil compaction state 'PloughLayer' or 'PloughPan'. See exemples.
}

\value{
  \item{N}{the specific volume at p = 1 kPa, N}
  \item{CI}{the compression index, lambda}
  \item{k}{the recompression index, kappa}
}

\details{
Pedo-transfer function developed for a sandy loam soil texture. See de Lima et al. (2018)
}

\references{
de Lima, R. P., Rolim, M. M., da C. Dantas, D., da Silva, A. R., & Mendonça, E. A., 2020.
Compressive properties and least limiting water range of plough layer and plough pan in sugarcane fields. 
\emph{Soil Use and Management}, x:xx-xx.

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

compressive_properties4(matric.suction=100, soil="PloughLayer")
compressive_properties4(matric.suction=100, soil="PloughPan")



# EXEMPLE 2 for "PloughLayer"

matric.suction <- seq(from=10,to=10000,len=100)
out <- compressive_properties4(matric.suction=matric.suction, soil="PloughLayer")
plot(x=matric.suction,y=out$N, ylab="N", xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for kappa


# EXEMPLE 3 for "PloughPan"

matric.suction <- seq(from=10,to=10000,len=100)
out <- compressive_properties4(matric.suction=matric.suction, soil="PloughPan")
plot(x=matric.suction,y=out$N, ylab="N", xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for kappa




# End (not run)
}
