\name{fitsoilwater2}
\alias{fitsoilwater2}

\title{ Interactive Estimation of the Groenevelt and Grant (2004) Model Parameters }

\description{
An interactive graphical adjustment of the soil water retention curve via Groenevelt and Grant (2004) formula.
The nonlinear least-squares estimates can be achieved taking the graphical initial
values.
}

\usage{
fitsoilwater2(theta, x, x0 = 6.653, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{theta}{ a numeric vector containing the values of soil water content.}
  \item{x}{ a numeric vector containing pF (pore water suction) values. See \code{\link{soilwater2}}.}
  \item{x0}{ the value of pF at which the soil water content becomes zero. The default is 6.653.}
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
Groenevelt & Grant (2004). A newmodel for the soil-water retention curve that solves the problem 
	of residualwater contents. \emph{European Journal of Soil Science}, 55:479-485.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{nls}}, \code{\link{soilwater2}}, \code{\link{soilwater}}
}

\examples{
w <- c(0.417, 0.354, 0.117, 0.048, 0.029, 0.017, 0.007, 0)
pF <- 0:7
fitsoilwater2(w, pF)

# End (not run)
}
