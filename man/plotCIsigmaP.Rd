\name{plotCIsigmaP}
\alias{plotCIsigmaP}

\title{ Percentile Confidence Intervals for Simulated Preconsolidation Stress }

\description{
Build and plot percentile confidence intervals for preconsolidation stress simulated from
\code{\link{simSigmaP}}.
}

\usage{
plotCIsigmaP(msim, conf.level = 0.95, shade.col = "orange", 
	ordered = TRUE, xlim = NULL, xlab = expression(sigma[P]), 
	las = 1, mar = c(4.5, 6.5, 2, 1), ...)
}

\arguments{
  \item{msim}{ an object of class \code{"simSigmaP"}.}
  \item{conf.level}{ the confidence level for the intervals.}
  \item{shade.col}{ a character or number indicating the color of the shaded area delimiting each CI. 
	See \code{\link{colors}}.}
  \item{ordered}{ logical; should the intervals be displayed according to the value of the simulated mean?}
  \item{xlim}{ optional; a numeric vector of length two containing the limits of the \emph{x}-axis.}
  \item{xlab}{ optional; a character indicating the \emph{x}-axis label.}
  \item{las}{ optional; see \code{\link{par}}.}
  \item{mar}{ optional; see \code{\link{par}}.}
  \item{\dots}{ further graphical parameters; see \code{\link{par}}.}
}

\value{
A numeric matrix containing the simulated mean, coefficient of variation, 
	lower and upper CI limits and the name of the method used to calculate
	the preconsolidation stress.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{simSigmaP}}, \code{\link{sigmaP}}
}

\examples{
# input data: stress and void ratio
pres <- c(1, 12.5, 25, 50, 100, 200, 400, 800, 1600)
VR <- c(1.43, 1.41, 1.40, 1.39, 1.35, 1.31, 1.25, 1.18, 1.12)

# simulation (may take a few seconds)
simres <- simSigmaP(VR, pres, nsim = 50)
head(simres)

# percentile confidence intervals
ci <- plotCIsigmaP(simres, conf.level = 0.95, 
	shade.col = "blue", ordered = TRUE)
print(ci)

# End (Not run)
}