\name{SSlbc}
\alias{SSlbc}

\title{ Self-Starting Nls Load Bearing Capacity Model }

\description{
A  \code{\link{selfStart}} model that evaluates the Load Bearing Capacity
(Dias Jr., 1994) function and its gradient. It has an initial attribute
that creates initial estimates of the parameters b0 and b1.
}

\usage{ SSlbc(theta, b0, b1) }

\arguments{
  \item{theta}{ a numeric vector of soil moisture values at which to evaluate the model. }
  \item{b0}{ a numeric parameter. }
  \item{b1}{ a numeric parameter. }
}

\value{
a numeric vector with the same length of \code{theta}. It is the value of the
expression \eqn{10^{(b0 + b1 * \theta)}}. Also, the gradient matrix with respect
to the parameters is attached as an attribute named \emph{gradient}.
}

\references{
Dias Junior, M. S. (1994). \emph{Compression of three soils under longterm
   tillage and wheel traffic}. 1994. 114p. Ph.D. Thesis - Michigan
   State University, East Lansing.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{getInitiallbc}}, \code{\link{fitlbc}}, \code{\link{selfStart}},
\code{\link{nls}}, \code{\link{sigmaP}}
}

\examples{
data(compaction)
attach(compaction)
ss <- SSlbc(Mois, 2.79, -2.33)
ss[1:50] # prediction
PS # original data of preconsolidation stress
ss # prediction and gradient

# End (not run)
}
