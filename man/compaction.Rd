\name{compaction}
\alias{compaction}
\docType{data}

\title{ Soil Compaction Data Set }

\description{
This data set refers to physical soil variables related to soil compaction.
}

\usage{ data(compaction) }

\format{
  A data frame with 50 observations on the following 4 variables.
  \describe{
    \item{\code{PR}}{a numeric vector containing soil penetration resistance values (MPa). }
    \item{\code{BD}}{a numeric vector containing soil bulk density values (g / cm^3). }
    \item{\code{Mois}}{a numeric vector containing soil moisture values (cm^3 / cm^3). }
    \item{\code{PS}}{a numeric vector containing soil preconsolidation stress values (kPa). }
  }
}

\source{ Simulated data. }

\examples{
data(compaction)
summary(compaction)
}

\keyword{datasets}
