\name{soilwater}
\alias{soilwater}

\title{ Soil Water Retention, based on the van Genuchten's (1980) formula }

\description{
Function to calculate the soil water content based on the
van Genuchten's (1980) formula:
\deqn{\theta = \theta_R + (\theta_S - \theta_R)(1 + (\alpha x)^n)^{-m}}
}

\usage{
soilwater(x, theta_R, theta_S, alpha, n, m = 1 - 1/n,
    saturation.index = FALSE)
}

\arguments{
  \item{x}{ the matric potential. }
  \item{theta_R}{ the residual water content. }
  \item{theta_S}{ the water content at saturation. }
  \item{alpha}{ a scale parameter of the van Genuchten's formula. }
  \item{n}{ a shape parameter in van Genuchten's formula. }
  \item{m}{ a shape parameter in van Genuchten's Formula. Default is \eqn{1-1/n} (Mualem, 1976). }
  \item{saturation.index}{ logical; if FALSE (default) the outcome is the soil water content,
	otherwise the saturation index is returned. }
}

\value{
The the soil water content or the saturation index (a value between 0 and 1).
}

\references{
Genuchten, M. T. van. (1980). A closed form equation for predicting the
hydraulic conductivity of unsaturated soils. \emph{Soil Science Society of
America Journal}, 44:892-898.

Mualem, Y. (1976). A new model for predicting the hydraulic conductivity
of unsaturated porous media. \emph{Water Resources Research}, 12:513-522.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
(code adapted from the function swc(), package \emph{soilwater} (Cordano \emph{et al}., 2012).)
}

\seealso{
\code{\link{fitsoilwater}}
}

\examples{
# example 1
soilwater(x = 0.1, theta_R = 0.06, theta_S = 0.25, alpha = 21, n = 2.08)
curve(soilwater(x, theta_R = 0.06, theta_S = 0.25, alpha = 21, n = 2.08))

# example 2 (punctual predictions)
p <- seq(0, 1, length.out = 10)
m <- soilwater(x = p, theta_R = 0.06, theta_S = 0.25,
	alpha = 21, n = 2.08)
points(m ~ p, type = "b", col = "red")

# End (not run)
}
