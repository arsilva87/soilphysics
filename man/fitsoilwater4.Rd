\name{fitsoilwater4}
\alias{fitsoilwater4}

\title{ Self-starting Nls Power Models for Soil Water Retention }

\description{
Function to self start the following nonlinear power models for soil water retention: 

\deqn{\theta = \exp(a + b*Bd) \psi^c}  (Silva et al., 1994)

\deqn{\theta = a \psi^c}  (Ross et al., 1991)

where \eqn{\theta} is the soil water content.

\code{fitsoilwater()} creates initial estimates (by log-linearization) of the parameters a, b and c and uses them
to provide its least-squares estimates through \code{\link{nls}}.

}

\usage{
fitsoilwater4(theta, psi, Bd, model = c("Silva", "Ross"))
}

\arguments{
  \item{theta}{ a numeric vector containing values of soil water content.}
  \item{psi}{ a numeric vector containing values of water potential (Psi).}
  \item{Bd}{ a numeric vector containing values of dry bulk density.}
  \item{model}{ a character; the model to be used for calculating the soil water content. It must be one of the
	two: \code{"Silva"} (default) or \code{"Ross"}.}
}

\value{
A "nls" object containing the fitted model.
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
data(skp1994)
# Example 1
ex1 <- with(skp1994,
    fitsoilwater4(theta = W, psi = h, model = "Ross"))
ex1
summary(ex1)

# Example 2
ex2 <- with(skp1994,
    fitsoilwater4(theta = W, psi = h, Bd = BD, model = "Silva"))
ex2
summary(ex2)

# Not run
}