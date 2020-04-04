\name{fitbusscher}
\alias{fitbusscher}

\title{ Self-starting Nls Busscher's (1990) Model for Soil Penetration Resistance }

\description{
Function to self start the nonlinear Busscher's (1990) model for penetration
resistance, i.e., \eqn{Pr = b0 * (\theta ^ {b1}) * (Bd ^ {b2})}. It creates initial
estimates (by log-linearization) of the parameters b0, b1 and b2 and uses them
to provide its least-squares estimates through \code{\link{nls}}.
}

\usage{ fitbusscher(Pr, theta, Bd, ...) }

\arguments{
  \item{Pr}{ a numeric vector containing penetration resistance values. }
  \item{theta}{ a numeric vector containing soil moisture values at which to evaluate the model. }
  \item{Bd}{ a numeric vector containing bulk density values at which to evaluate the model. }
  \item{\dots}{ further arguments to \code{\link{nls}}. }
}

\value{
A \code{\link{nls}} output (see \code{help(nls)}).
}

\references{
Busscher, W. J. (1990). Adjustment of flat-tipped penetrometer resistance
data to common water content. \emph{Transactions of the ASAE}, 3:519-524.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{fitlbc}}, \code{\link{nls}}, \code{\link{summary.nls}},
\code{\link{predict.nls}}, \code{\link{Rsq}}
}

\examples{
data(compaction)
attach(compaction)
out <- fitbusscher(Pr = PR, theta = Mois, Bd = BD)
summary(out)
Rsq(out)

# 3D plot
X <- seq(min(Mois), max(Mois), len = 30) # theta
Y <- seq(min(BD), max(BD), len = 30) # Bd
f <- function(x, y) coef(out)[1] * (x^coef(out)[2]) * (y^coef(out)[3])
Z <- outer(X, Y, f)
persp(X, Y, Z,
	xlab = "Soil moisture",
	ylab = "Soil bulk density",
	zlab = "Penetration resistance",
	ticktype = "detailed",
	phi = 20, theta = 30)

# End (not run)
}
