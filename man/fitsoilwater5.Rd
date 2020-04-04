\name{fitsoilwater5}
\alias{fitsoilwater5}

\title{ Interactive Estimation of the Modified van Genuchten's Model Parameters }

\description{
An interactive graphical adjustment of the soil water retention curve via the van Genuchten's formula, 
modified by Pierson and Mulla (1989). The nonlinear least-squares estimates can be achieved taking the 
graphical initial values.
It may be useful to estimate the parameters needed in the high-energy-moisture-characteristics (HEMC) method, 
which is used to analyze the aggregate stability. 
}

\usage{
fitsoilwater5(theta, x, theta_S, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{theta}{ a numeric vector containing the values of soil water content. }
  \item{x}{  a numeric vector containing the matric potential values. }
  \item{theta_S}{ an offset; a value for the parameter \code{theta_S}, the water content at saturation. See details.}
  \item{xlab}{  a label for the x axis; if is NULL, the label "Matric potential" is used. }
  \item{ylab}{  a label for the y axis; if is NULL, the label "Soil water content" is used. }
  \item{\dots}{  further graphical arguments; see \code{\link{par}}. }
}

\details{
The parameter theta_S must be passed as an argument. It is recommended to consider it as the highest water content 
value in the data set or the water content at saturation.
}

\value{
A plot of \code{theta} versus \code{x} and the curve of the current fitted model
according to the adjusted parameters in an external interactive panel.
Pressing the button "NLS estimates" a \code{\link{nls}} summary of the
fitted model is printed on console whether convergence is achieved, otherwise
a warning box of "No convergence" is shown.
}

\references{
Pierson, F.B.; Mulla, D.J. (1989) An Improved Method for Measuring Aggregate Stability
of a Weakly Aggregated Loessial Soil. \emph{Soil Sci. Soc. Am. J.}, 53:1825--1831.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{nls}}, \code{\link{soilwater5}}
}


\examples{
h <- seq(0.1, 40, by = 2)
w <- c(0.735, 0.668, 0.635, 0.612, 0.559, 0.462, 0.369, 0.319, 0.296, 0.282, 
	0.269, 0.256, 0.249, 0.246, 0.239, 0.236, 0.229, 0.229, 0.226, 0.222)
plot(w ~ h)

# suggestions of starting values: thetaR = 0.35, alpha = 0.1, n = 10, 
# b0 = 0.02, b1 = -0.0057, b2 = 0.00004 (Not run)

fitsoilwater5(theta = w, x = h, theta_S = 0.70)

# End (Not run)
}
