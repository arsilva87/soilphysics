\name{soilwater5}
\alias{soilwater5}

\title{ Soil Water Retention, based on the modified van Genuchten's formula }

\description{
Function to calculate the soil water content based on the modified van Genuchten's formula,
as suggested by Pierson and Mulla (1989):
\deqn{\theta = \theta_R + (\theta_S - \theta_R)(1 + (\alpha x)^n)^{-m} + b_0 + b_1 x + b_2 * x^2}
}

\usage{
soilwater5(x, theta_R, theta_S, alpha, n, m = 1 - 1/n, b0, b1, b2)
}

\arguments{
  \item{x}{ the matric potential.}
  \item{theta_R}{ the residual water content. }
  \item{theta_S}{ the water content at saturation. }
  \item{alpha}{ a scale parameter of the van Genuchten's formula. }
  \item{n}{ a shape parameter in van Genuchten's formula. }
  \item{m}{ a shape parameter in van Genuchten's Formula. Default is \eqn{1-1/n} (Mualem, 1976). }
  \item{b0}{ a parameter added to the van Ganuchten's formula.}
  \item{b1}{ a parameter added to the van Ganuchten's formula.}
  \item{b2}{ a parameter (of quadratic term) added to the van Ganuchten's formula.}
}

\value{ The the soil water content or the saturation index (a value between 0 and 1). }

\references{
Pierson, F.B.; Mulla, D.J. (1989) An Improved Method for Measuring Aggregate Stability
of a Weakly Aggregated Loessial Soil. \emph{Soil Sci. Soc. Am. J.}, 53:1825--1831.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{fitsoilwater5}}
}

\examples{
soilwater5(x = 20, theta_R = 0.2735, theta_S = 0.478, alpha = 0.1029, 
	n = 9.45, b0 = 0.2278, b1 = -0.0165, b2 = 0.000248)
curve(soilwater5(x, theta_R = 0.2735, theta_S = 0.478, alpha = 0.1029, 
	n = 9.45, b0 = 0.2278, b1 = -0.0165, b2 = 0.000248), 
	from = 0, to = 40,
	ylab = "Water content",
	xlab = "Matric potential")

# End (Not run)
}
