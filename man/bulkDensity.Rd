\name{bulkDensity}
\alias{bulkDensity}
\docType{data}

\title{ Soil Bulk Density Data Set }

\description{
This data set refers to five observations of soil bulk density and soil moisture per sample.
There are four soil samples.
}

\usage{ data(bulkDensity) }

\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Id}}{ a factor with levels \code{s1} \code{s2} \code{s3} \code{s4}, the 'ID' of each soil sample. }
    \item{\code{MOIS}}{ a numeric vector containing soil moisture values (cm^3 / cm^3). }
    \item{\code{BULK}}{ a numeric vector containing soil bulk density values (g / cm^3). }
  }
}

\source{ Simulated data. }

\examples{
data(bulkDensity)
summary(bulkDensity)
}

\keyword{datasets}
