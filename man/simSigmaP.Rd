\name{simSigmaP}
\alias{simSigmaP}

\title{ Simulating Preconsolidation Stress }

\description{
Simulating preconsolidation pressure, compression and swelling indices, based on a 
	multivariate Gaussian distribution for the parameters of the compression curve.}

\usage{
simSigmaP(voidratio, stress, 
	what.out = c("sigmaP", "CI", "SI"), 
	method = c("casagrande", "VCLzero", "reg1", "reg2", "reg3", "reg4", "pacheco"), 
	n4VCL = 3, nsim = 100)
}

\arguments{
  \item{voidratio}{ a numeric vector containing void ratio (or bulk density) values.}
  \item{stress}{ a numeric vector containing the applied stress sequence.}
  \item{what.out}{ a character indicating which \code{sigmaP()} output should be simulated.
	It must be on of "sigmaP" (default), "CI" of "SI".}
  \item{method}{ a character vector indicating which methods should be used.}
  \item{n4VCL}{ the number of points for calculating the slope of the soil Virgin Compression Line (VCL), 
	which is obtained by linear regression. Default is 3.}
  \item{nsim}{ the number of simulations. Default is 100. Warning: it may cause time demanding.}
}

\value{
A numeric matrix containing the simulated values for each method selected as input.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{sigmaP}}, \code{\link{plotCIsigmaP}}
}

\examples{
# input data: stress and void ratio
pres <- c(1, 12.5, 25, 50, 100, 200, 400, 800, 1600)
VR <- c(1.43, 1.41, 1.40, 1.39, 1.35, 1.31, 1.25, 1.18, 1.12)

# simulation (may take a few seconds)
simres <- simSigmaP(VR, pres, nsim = 50)
head(simres)

# plot percentile confidence intervals
ci <- plotCIsigmaP(simres, conf.level = 0.95, 
	shade.col = "blue", ordered = TRUE)
print(ci)

# End (Not run)
}