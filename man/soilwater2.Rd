\name{soilwater2}
\alias{soilwater2}
\title{ Soil Water Retention, based on the Groenevelt & Grant (2004) formula}

\description{
Function to calculate the soil water content based on the Groenevelt & Grant (2004) model. 
It is based on thermodynamic principles. Therefore, it is appropriate for the case in which 
thermodynamic equilibrium has been attained by diffusion of water. 
In this case, the water retention curve is given by:

\deqn{\theta = k_1 \exp(-k_0 / x_0^n) - k_1 \exp(-k_0 / x^n)}

where \eqn{x = log h} (pore water suction), and h is in units of hPa 
}

\usage{
soilwater2(x, x0 = 6.653, k0, k1, n)
}

\arguments{
  \item{x}{ a numeric vector containing pF values.}
  \item{x0}{ the value of pF (pore water suction) at which the soil water content becomes zero. The default is 6.653.}
  \item{k0}{ a parameter value.}
  \item{k1}{ a parameter value.}
  \item{n}{ a parameter value.}
}

\value{
The the soil water content.
}

\references{
Groenevelt & Grant (2004). A newmodel for the soil-water retention curve that solves the problem 
	of residualwater contents. \emph{European Journal of Soil Science}, 55:479-485.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{fitsoilwater2}}, \code{\link{soilwater}}
}

\examples{
pF <- 0:7
soilwater2(pF, k0 = 1.867, k1 = 0.426, n = 2.358)

# End (not run)
}
