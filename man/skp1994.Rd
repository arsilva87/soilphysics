\name{skp1994}
\alias{skp1994}
\docType{data}

\title{LLWR Data Set}

\description{
Data set presented by Leao et al. (2005), for determining the Least Limiting Water Range.
}

\usage{data(skp1994)}

\format{
  A data frame with 64 observations on the following 4 variables:
  \describe{
    \item{\code{BD}}{a numeric vector containing soil bulk density values, in Mg/m3.}
    \item{\code{W}}{a numeric vector containing volumetric water content values, in m3/m3.}
    \item{\code{PR}}{a numeric vector containing penetration resistance values, in MPa.}
    \item{\code{h}}{a numeric vector containing matric head values, in cm.}
  }
}

\source{
Leao et al. (2005). An Algorithm for Calculating the Least Limiting Water Range of Soils. 
	\emph{Agronomy Journal}, 97:1210-1215.
}

\examples{
data(skp1994)
summary(skp1994)
}

\keyword{datasets}

