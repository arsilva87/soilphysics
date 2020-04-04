\name{fun2form}
\alias{fun2form}

\title{ Converting Function to Formula }

\description{
An accessorial function to convert an object of \code{class} 'function' to an object
of \code{class} 'formula'.
}

\usage{ fun2form(fun, y = NULL) }

\arguments{
  \item{fun}{ a object of class 'function'. It must be a one-line-written function, with no curly braces "\{\}". }
  \item{y}{ optional; a character defining the lef side of the formula, \code{y = fun()}. }
}

\value{ An object of class \code{\link{formula}}. }

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\section{Warning }{ 
Numerical values into \code{fun} with three or more digits may cause miscalculation.
}

\seealso{
\code{\link{function}}, \code{\link{formula}}
}

\examples{
g <- function(x) Asym * exp(-b2 * b3 ^ x) # Gompertz Growth Model
fun2form(g, "y")

# f1 <- function(w) {exp(w)} # error
# fun2form(f1, "x") 
f2 <- function(w) exp(w) # ok
fun2form(f2, "x")

# End (not run)
}
