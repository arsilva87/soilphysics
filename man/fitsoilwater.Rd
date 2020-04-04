\name{fitsoilwater}
\alias{fitsoilwater}

\title{ Interactive Estimation of van Genuchten's (1980) Model Parameters }

\description{
An interactive graphical adjustment of the soil water retention curve via van Genuchten's (1980) formula.
The nonlinear least-squares estimates can be achieved taking the graphical initial
values.
}

\usage{
fitsoilwater(theta, x, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{theta}{ a numeric vector containing the values of soil water content. }
  \item{x}{  a numeric vector containing the matric potential values. }
  \item{xlab}{  a label for the x axis; if is NULL, the label "Matric potential" is used. }
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
Genuchten, M. T. van. (1980). A closed form equation for predicting the
hydraulic conductivity of unsaturated soils. \emph{Soil Science Society of
America Journal}, 44:892-898.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{nls}}, \code{\link{soilwater}}
}

\examples{
# Liu et al. (2011)
h <- c(0.001, 50.65, 293.77, 790.14, 992.74, 5065, 10130, 15195)
w <- c(0.5650, 0.4013, 0.2502, 0.2324, 0.2307, 0.1926, 0.1812, 0.1730)
fitsoilwater(w, h)

# End (not run)
}
