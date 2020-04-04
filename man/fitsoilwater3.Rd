\name{fitsoilwater3}
\alias{fitsoilwater3}

\title{ Interactive Estimation of the Dexter's (2008) Model Parameters }

\description{
An interactive graphical adjustment of the soil water retention curve through the Dexter's (2008) formula. 
The nonlinear least-squares estimates can be achieved taking the graphical initial values.
}

\usage{
fitsoilwater3(theta, x, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{theta}{ a numeric vector containing the values of soil water content.}
  \item{x}{ a numeric vector containing the values of applied air pressure.}
  \item{xlab}{  a label for the x axis; if is NULL, the label "pF" is used. }
  \item{ylab}{  a label for the y axis; if is NULL, the label "Soil water content" is used. }
  \item{\dots}{  further graphical arguments; see \code{\link{par}}. }
}

\value{
A plot of \code{theta} versus \code{x} and the curve of the current fitted model
according to the adjusted parameters in an external interactive panel.
Pressing the button "NLS estimates" a \code{\link{nls}} summary of the
fitted model is printed on console whether convergence is achieved, otherwise
a warning box of "No convergence" is shown.
}

\references{
Dexter et al. (2008). A user-friendly water retention function that takes account of the textural
	and structural pore spaces in soil. \emph{Geoderma}, 143:243--253.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{soilwater3}}, \code{\link{nls}}, \code{\link{fitsoilwater2}}
}

\examples{
# data extracted from Liu et al. (2011)
h <- c(0.001, 50.65, 293.77, 790.14, 992.74, 5065, 10130, 15195)
w <- c(0.5650, 0.4013, 0.2502, 0.2324, 0.2307, 0.1926, 0.1812, 0.1730)
fitsoilwater3(w, h)

# End (not run)
}
