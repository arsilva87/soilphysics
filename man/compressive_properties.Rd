\name{compressive_properties}
\alias{compressive_properties}

\title{Compressive parameters by Defossez et al. (2003) }

\description{
It calculates the compressive parameters N and lambda using the pedo-transfer function from Defossez et al. (2003)
}

\usage{ 
compressive_properties(water.content, soil=c("Loess", "Calcareous"))
}

\arguments{
  \item{water.content}{ a numeric vector containing the values of gravimetric water content, \eqn{%}}
  \item{soil}{the soil group 'Loess' or 'Calcareous'. See exemples}
}

\value{
  \item{N}{the specific volume at \eqn{p = 1 kPa}, N}
  \item{CI}{the compression index, lambda}
}

\details{
In Defossez et al. (2003), the recompression index, kappa, was assumed as 0.0058 for both soil group.
}

\references{
Defossez, P., Richard, G., Boizard, H., & O'Sullivan, M. F., 2003. 
Modeling change in soil compaction due to agricultural traffic as function of soil water content. 
\emph{Geoderma}, 116:89-105.


}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 
}

\seealso{
\code{\link{stressTraffic}}
}

\examples{
# EXAMPLE 1 - For Loess and Calcareous soil

water.content <- 25
compressive_properties(water.content=water.content, soil="Loess") 
compressive_properties(water.content=water.content, soil="Calcareous") 

# EXAMPLE 2 - For Loess soil

water.content <- seq(from=5,to=30,len=20)
out <- compressive_properties(water.content=water.content, soil="Loess") 
plot(x=water.content ,y=out$N, ylab="N", xlab="Bulk density") # plot for N
plot(x=water.content ,y=out$CI, ylab="Lambda", xlab="Bulk density") # plot for compression index


# EXAMPLE 3 - For Calcareous soil

water.content <- seq(from=5,to=30,len=20)
out <- compressive_properties(water.content=water.content, soil="Calcareous") 
plot(x=water.content ,y=out$N, ylab="N", xlab="Bulk density") # plot for N
plot(x=water.content ,y=out$CI, ylab="Lambda", xlab="Bulk density") # plot for compression index

# End (not run)
}
