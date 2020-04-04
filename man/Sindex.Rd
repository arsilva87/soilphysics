\name{Sindex}
\alias{Sindex}

\title{ The S Index }

\description{
Function to calculate the S index (Dexter, 2004) for evaluating the soil
physical quality based on the \emph{Water Retention Curve} (van Genuchten, 1980).

\deqn{ S = - n * (\theta_S - \theta_R) * (1 + 1/m)^{-(1 + m)} }
}

\usage{
Sindex(theta_R, theta_S, alpha, n, m = 1 - 1/n, vcov = NULL,
	nsim = 999, conf.level = 0.95, graph = TRUE, ...)
}

\arguments{
  \item{theta_R}{ the residual water content. }
  \item{theta_S}{ the water content at saturation. }
  \item{alpha}{ a scale parameter of the van Genuchten's formula. }
  \item{n}{ a shape parameter in van Genuchten's formula. }
  \item{m}{ a shape parameter in van Genuchten's Formula. Default is \eqn{1-1/n} (Mualem, 1976). }
  \item{vcov}{ optional (default is \code{NULL}); a variance-covariance matrix of the estimates which is
	used to perform	Monte Carlo simulations of the parameters \code{theta_R}, \code{theta_S}, \code{alpha} and
	\code{n} for building a simulated confidence interval of the S index (in modulus).}
  \item{nsim}{ the number of Monte Carlo simulations; default is 999. It is used only if \code{vcov}
	is specified. }
  \item{conf.level}{ the confidence level; default is 0.95. It is used only if \code{vcov}
	is specified.}
  \item{graph}{ logical; if TRUE (defaul), the soil water retention curve is plotted. }
  \item{\dots}{ further graphical arguments. }
}

\value{
A list of
  \item{h_i }{ the modulus of the water potential at the inflection point. }
  \item{theta_i }{ the water content at the inflection point. }
  \item{S.index }{ the modulus of the S index. }
  \item{PhysicalQuality }{A character indicating the soil physical quality, as proposed by Dexter (2004). }
  \item{simCI }{the simulated confidence interval. It is stored only if \code{vcov} is specified. }
  \item{conf.level }{the confidence level for the simulated confidence interval. It is stored only if \code{vcov} is specified. }
}

\references{
Dexter, A. R. (2004). Soil physical quality Part I. Theory,
   effects of soil texture, density, and organic matter,
   and effects on root growth. \emph{Geoderma}, 120:201-214.

Genuchten, M. T. van. (1980). A closed form equation for predicting the
hydraulic conductivity of unsaturated soils. \emph{Soil Science Society of
America Journal}, 44:892-898.

Mualem, Y. (1976). A new model for predicting the hydraulic conductivity
of unsaturated porous media, \emph{Water Resource Research}, 12:513-522.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{soilwater}}, \code{\link{fitsoilwater}}
}

\examples{
# Dexter (2004, Table 1)
Sindex(0, 0.395, 0.0217, 1.103, xlim = c(0, 1000))
Sindex(0, 0.335, 0.0616, 1.139, xlim = c(0, 1000))
# ...
Sindex(0, 0.226, 0.0671, 1.581, xlim = c(0, 1000))

# End (not run)
}
