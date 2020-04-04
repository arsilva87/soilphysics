\name{fitlbc}
\alias{fitlbc}

\title{ Parameter Estimation of the Load Bearing Capacity Model }

\description{
This function creates initial parameter estimates of the nonlinear Load
Bearing Capacity (Dias Jr., 1994) model, i.e., \eqn{\sigma_P = 10 ^ {(b0 + b1 * \theta)}},
by using two methods: a getInitial method or a log-linearization. Then,
it uses them to provide its least-squares estimates via \code{\link{nls}}.
}

\usage{ fitlbc(theta, sigmaP, ...) }

\arguments{
  \item{theta}{ a numeric vector containing soil moisture values. }
  \item{sigmaP}{ a numeric vector containing values of soil preconsolidation stress. }
  \item{\dots}{ further arguments to \code{\link{nls}}. }
}

\value{
A \code{\link{nls}} object.
}

\references{
Dias Junior, M. S. (1994). \emph{Compression of three soils under longterm
   tillage and wheel traffic}. 1994. 114p. Ph.D. Thesis - Michigan
   State University, East Lansing.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{sigmaP}}, \code{\link{fitbusscher}},
\code{\link{maxcurv}}, \code{\link{Rsq}}
}

\examples{
data(compaction)
attach(compaction)
out <- fitlbc(theta = Mois, sigmaP = PS)
summary(out)
Rsq(out)
curve(10^(coef(out)[1] + coef(out)[2]*x))

# End (not run)
}
