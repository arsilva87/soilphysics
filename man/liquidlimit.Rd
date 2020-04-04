\name{liquidlimit}
\alias{liquidlimit}

\title{ Soil Liquid Limit }

\description{
Function to determine the soil Liquid Limit by using the Sowers (1965) method.
\deqn{LL = \theta * (n / 25) ^ {0.12}}.
}

\usage{ liquidlimit(theta, n) }

\arguments{
  \item{theta}{ the soil mositure value corresponding to \code{n} drops. }
  \item{n}{ the number of drops. }
}


\value{ The soil moisture value corresponding to the Liquid Limit. }

\references{
Sowers, G. F. (1965). Consistency. In: BLACK, C.A. (Ed.). \emph{Methods of soil analysis}.
Madison: American Society of Agronomy. Part 1, p.391-399. (Agronomy, 9).

Sowers, G. F. (1965). Consistency. In: KLUTE, A. (Ed.). 2 ed. \emph{Methods of soil analysis}.
Madison: American Society of Agronomy. Part 1, p.545-566.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{ \code{\link{criticalmoisture}} }

\examples{
liquidlimit(theta = 0.34, n = 22)

M <- c(0.34, 0.29, 0.27, 0.25, 0.20)
N <- c(22, 24, 25, 26, 28)
liquidlimit(theta = M, n = N)

# End (not run)
}
