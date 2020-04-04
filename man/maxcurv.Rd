\name{maxcurv}
\alias{maxcurv}

\title{ Maximum Curvature Point }

\description{
Function to determine the maximum curvature point of an univariate nonlinear
function of x.
}

\usage{ maxcurv(x.range, fun, 
	method = c("general", "pd", "LRP", "spline"), 
	x0ini = NULL, 
	graph = TRUE, ...) 
}

\arguments{
  \item{x.range}{ a numeric vector of length two, the range of x. }
  \item{fun}{ a function of x; it must be a one-line-written function, with no curly braces '\{\}'. }
  \item{method}{ a character indicating one of the following: "general" - for evaluating the general 
	curvature function (\emph{k}), "pd" - for evaluating perpendicular distances from a secant line,
	"LRP" - a NLS estimate of the maximum curvature point as the breaking point of Linear Response Plateau model,
	"spline" - a NLS estimate of the maximum curvature point as the breaking point of a piecewise linear spline.
	See details.}
  \item{x0ini}{ an initial \emph{x}-value for the maximum curvature point. Required only when "LRP" or "spline" are used.}
  \item{graph}{ logical; if TRUE (default) a curve of \code{fun} is plotted. }
  \item{\dots}{ further graphical arguments. }
}

\details{
The method "LRP" can be understood as an especial case of "spline". And both models are fitted via \code{\link{nls}}.
The method "pd" is an adaptation of the method proposed by Lorentz et al. (2012). The "general" method should be 
preferred for finding \emph{global} points. On the other hand, "pd", "LRP" and "spline" are suitable for finding 
\emph{local} points of maximum curvature.
}

\value{
A list of
  \item{fun}{ the function of x. }
  \item{x0}{ the x critical value. }
  \item{y0}{ the y critical value. }
  \item{method}{ the method of determination (input). }
}

\references{
Lorentz, L.H.; Erichsen, R.; Lucio, A.D. (2012). Proposal method for plot size estimation in crops. 
	\emph{Revista Ceres}, 59:772--780. 
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{function}}, \code{\link{curve}}
}

\examples{
# Example 1: an exponential model
f <- function(x) exp(-x)
maxcurv(x.range = c(-2, 5), fun = f)

# Example 2: Gompertz Growth Model
Asym <- 8.5
b2 <- 2.3
b3 <- 0.6
g <- function(x) Asym * exp(-b2 * b3 ^ x)
maxcurv(x.range = c(-5, 20), fun = g)

# using "pd" method
maxcurv(x.range = c(-5, 20), fun = g, method = "pd")

# using "LRP" method
maxcurv(x.range = c(-5, 20), fun = g, method = "LRP", x0ini = 6.5)

# Example 3: Lessman & Atkins (1963) model for optimum plot size
a = 40.1
b = 0.72
cv <- function(x) a * x^-b
maxcurv(x.range = c(1, 50), fun = cv)

# using "spline" method
maxcurv(x.range = c(1, 50), fun = cv, method = "spline", x0ini = 6)

# End (not run)
}
