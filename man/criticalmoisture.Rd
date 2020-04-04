\name{criticalmoisture}
\alias{criticalmoisture}
\alias{maxbulkdensity}

\title{ Critical Moisture and Maximum Bulk Density }

\description{
Function to determine the soil Critical Moisture and the Maximum Bulk Density 
based on the Proctor (1933) compaction test. It estimates compaction curve
by fitting a quadradtic regression model.
}

\usage{
criticalmoisture(theta, Bd, samples = NULL, graph = TRUE, ...)

maxbulkdensity(theta, Bd, samples = NULL, graph = TRUE, ...)
}

\arguments{
  \item{theta}{ a vector containing the soil moisture values. }
  \item{Bd}{ a vector containing the the soil bulk density values. }
  \item{samples}{ optional; a vector indicating the multiple samples. Default is NULL (one sample). See details. }
  \item{graph}{ logical; if TRUE (default), the soil compaction curve is plotted. }
  \item{\dots}{ further graphical arguments. }
}

\details{ If \code{samples} is ispecified, then it must has the same length of \code{theta} and \code{Bd}. }

\value{
An object of class 'criticalmoisture', i.e., a matrix containing the quadratic model
coefficients (rows 1 to 3), the R-squared (row 4), the sample size (row 5), the critical
soil moisture (row 6) and the maximum bulk density (row 7), per sample.
}

\references{
Proctor, E. R. (1933). Design and construction of rolled earth dams. \emph{Eng. News Record}, 3: 245-284, 286-289, 348-351, 372-376.

Silva, A. P. et al. (2010). Indicadores da qualidade fisica do solo. In: Jong Van Lier, Q. (Ed). \emph{Fisica do solo}.
  Vicosa (MG): Sociedade Brasileira de Ciencia do Solo. p.541-281.
}

\note{
\code{maxbulkdensity} is just an alias of \code{criticalmoisture}.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{maxcurv}}
}

\examples{
# example 1 (1 sample)
mois <- c(0.083, 0.092, 0.108, 0.126, 0.135)
bulk <- c(1.86, 1.92, 1.95, 1.90, 1.87)
criticalmoisture(theta = mois, Bd = bulk)

# example 2 (4 samples)
data(bulkDensity)
attach(bulkDensity)
criticalmoisture(theta = MOIS, Bd = BULK, samples = Id)

# End (not run)
}
