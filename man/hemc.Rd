\name{hemc}
\alias{hemc}

\title{ High-Energy-Moisture-Characteristics Aggregate Stability }

\description{
A function to determine the modal suction, volume of drainable pores, structural index and stability ratio 
using the high-energy-moisture-characteristics (HEMC) method by Pierson & Mulla (1989), which is used to analyze 
the aggregate stability. Before using \code{hemc()}, the user may estimate the parameters of the Modified van 
Genuchten's Model through the function \code{fitsoilwater5()}. 
}

\usage{
hemc(x, theta_R, theta_S, alpha, n, b1, b2, 
	graph = TRUE, from = 1, to = 30, 
	xlab = expression(Psi ~ (J~kg^{-1})), 
	ylab = expression(d ~ theta/d ~ Psi), ...)
}

\arguments{
  \item{x}{a vector containing matric potential values.}
  \item{theta_R}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{theta_S}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{alpha}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{n}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{b1}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{b2}{ a numeric vector of length two containing the parameter values in the following orde: fast and slow.}
  \item{graph}{logical; if TRUE (default), a graphical solution is shown).}
  \item{from}{the lower limit for the x-axis}
  \item{to}{the lower limit for the x-axis}
  \item{xlab}{a label for the x-axis}
  \item{ylab}{a label for the y-axis}
  \item{\dots}{further graphical arguments}
}

\value{
A list of a two objects: 1) a matrix containing the Modal Suction, the Volume od Drainable Pores (VDP) and the Structural Index for
both, fast and slow wetting; and 2) the value of Stability Ratio.
}

\references{
Pierson, F.B.; Mulla, D.J. (1989). An Improved Method for Measuring Aggregate Stability of a Weakly 
	Aggregated Loessial Soil. \emph{Soil Sci. Soc. Am. J.}, 53:1825--1831.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{fitsoilwater5}}
}

\examples{
hemc(x = seq(1, 30), theta_R = c(0.27, 0.4), theta_S = c(0.65, 0.47), 
	alpha = c(0.1393, 0.0954), n = c(6.37, 7.47), 
	b1 = c(-0.008421, -0.011970), b2 = c(0.0001322, 0.0001552))

# End (Not run)
}
