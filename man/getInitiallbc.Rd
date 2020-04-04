\name{getInitiallbc}
\alias{getInitiallbc}

\title{ Get Initial Parameter Estimates for the Load Bearing Capacity Model }

\description{
This is a \code{\link{getInitial}} function that evaluates initial parameter estimates
for the Load Bearing Capacity model via \code{\link{SSlbc}}.
}

\usage{ getInitiallbc(theta, sigmaP) }

\arguments{
  \item{theta}{ a numeric vector containing values of soil moisture. }
  \item{sigmaP}{ a numeric vector containing values of preconsolidation stress. }
}

\value{ A numeric vector containing the estimates of the parameters b0 and b1. }

\references{
Dias Junior, M. S. (1994). \emph{Compression of three soils under longterm
   tillage and wheel traffic}. 1994. 114p. Ph.D. Thesis - Michigan
   State University, East Lansing.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{getInitial}}, \code{\link{SSlbc}}, \code{\link{nls}}, \code{\link{sigmaP}}
}

\examples{
data(compaction)
attach(compaction)
getInitiallbc(theta = Mois, sigmaP = PS)

# End (not run)
}
