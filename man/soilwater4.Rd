\name{soilwater4}
\alias{soilwater4}

\title{ Soil Water Retention, based on Power Models }

\description{
Function to calculate the soil water content based on the following formulas:
\deqn{\theta = \exp(a + b*Bd) \psi^c}  (Silva et al., 1994)

\deqn{\theta = a \psi^c}  (Ross et al., 1991)

where \eqn{\theta} is the soil water content.
}

\usage{
soilwater4(psi, Bd, a, b, c, model = c("Silva", "Ross"))
}

\arguments{
  \item{psi}{ a numeric vector containing values of water potential (Psi).}
  \item{Bd}{ a numeric vector containing values of dry bulk density.}
  \item{a}{ a model-fitting parameter. See details.}
  \item{b}{ a model-fitting parameter. See details.}
  \item{c}{ a model-fitting parameter. See details.}
  \item{model}{ a character; the model to be used for calculating the soil water content. It must be one of the
	two: \code{"Silva"} (default) or \code{"Ross"}.}
}

\details{
The parameters "a" and "c" have the same meaning in both models, but be aware that the parameter "a" of the model
employed by Silva et al. (1994) is parameter "a" of the Ross et al. (1991) in a log10 scale.
}

\value{
The the soil water content.
}

\references{
Ross et al. (1991). Equation for extending water-retention curves to dryness.
	\emph{Soil Science Society of America Journal}, 55:923-927. 

Silva et al. (1994). Characterization of the least limiting water range of soils. 
	\emph{Soil Science Society of America Journal}, 58:1775-1781.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{fitsoilwater4}}, \code{\link{soilwater}}, \code{\link{soilwater2}}, \code{\link{soilwater3}}
}

\examples{

# End (not run)
}
