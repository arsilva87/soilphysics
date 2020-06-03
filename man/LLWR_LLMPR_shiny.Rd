\name{LLWR_LLMPR_App}
\alias{LLWR_LLMPR_App}

\title{A shiny APP for LLWR and LLMPR calculation}

\description{A shiny APP for LLWR and LLMPR calculation}

\usage{
LLWR_LLMPR_shiny()
}
\arguments{
  \item{Inputs}{Available on the interactive web page}

}
\value{LLWR and LLMPR}
\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }

\seealso{
\code{\link{stressTraffic}}
}

\examples{
# EXAMPLE 1

compressive_properties4(matric.suction=100, soil="PloughLayer")
compressive_properties4(matric.suction=100, soil="PloughPan")



# EXAMPLE 2 for "PloughLayer"

matric.suction <- seq(from=10,to=10000,len=100)
out <- compressive_properties4(matric.suction=matric.suction, soil="PloughLayer")
plot(x=matric.suction,y=out$N, ylab="N", xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for kappa


# EXAMPLE 3 for "PloughPan"

matric.suction <- seq(from=10,to=10000,len=100)
out <- compressive_properties4(matric.suction=matric.suction, soil="PloughPan")
plot(x=matric.suction,y=out$N, ylab="N", xlab="Matric suction (hPa)", log="x") # plot for N
plot(x=matric.suction,y=out$lambda, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for lambda
plot(x=matric.suction,y=out$k, ylab="lambda", xlab="Matric suction (hPa)", log="x") # plot for kappa




# End (not run)
}

