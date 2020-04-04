\name{soilwater3}
\alias{soilwater3}

\title{ Soil Water Retention, based on the Dexter's (2008) formula }

\description{
Function to calculate the soil water content based on the Dexter's (2008) formula.
It is based on the segregation of pore space in two categories what are called bi-modal pore size distributions. 
In this model, the pore space is divided into two parts: the textural porosity which occurs between the primary 
mineral particles, and the structural porosity which occurs between micro aggregates and/or any other compound particles. 
This is called the double-exponential (DE) water retention equation, given by:
\deqn{\theta = \theta_R + a_1 \exp(-x/p_1) + a_2 \exp(-x/p_2) }

where \eqn{\theta} is the gravimetric water content.
}

\usage{
soilwater3(x, theta_R, a1, p1, a2, p2)
}

\arguments{
  \item{x}{a numeric vector containing the values of applied air pressure.}
  \item{theta_R}{ a parameter that represents the residual water content.}
  \item{a1}{ a parameter that represents the drainable part of the textural pore space in units of 
	gravimetric water content at saturation.}
  \item{p1}{ a parameter that represents the applied air pressures characteristic for displacement of water 
	from the textural pore space.}
  \item{a2}{ a parameter that represents the total structural pore space in units of gravimetric water 
	content at saturation.}
  \item{p2}{ a parameter that represents the applied air pressure that is characteristic for displacing 
	water from the structural pores.}
}

\value{
The the soil water content.
}

\references{
Dexter et al. (2008). A user-friendly water retention function that takes account of the textural
	and structural pore spaces in soil. \emph{Geoderma}, 143:243-253.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{fitsoilwater3}}, \code{\link{soilwater}}, \code{\link{soilwater2}}
}

\examples{
soilwater3(x = 0, theta_R = 0.058, a1 = 0.233, p1 = 3.274, a2 = 0.070, p2 = 1.78)
soilwater3(x = 3, theta_R = 0.058, a1 = 0.233, p1 = 3.274, a2 = 0.070, p2 = 1.78)
soilwater3(x = 4, theta_R = 0.058, a1 = 0.233, p1 = 3.274, a2 = 0.070, p2 = 1.78)

# End (not run)
}
