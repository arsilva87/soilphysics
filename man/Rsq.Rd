\name{Rsq}
\alias{Rsq}

\title{ Multiple R-squared }

\description{
Function to calculate the \emph{multiple R-squared} and the \emph{adjusted
R-squared} from a fitted model via \code{\link{lm}} or \code{\link{aov}}, i.e., linear models.
For a model fitted via \code{\link{nls}}, nonlinear models, the \emph{pseudo R-squared} is
returned.
}

\usage{ Rsq(model) }

\arguments{
  \item{model}{ a model fitted via \code{\link{lm}}, \code{\link{aov}} or \code{\link{nls}}. }
}

\value{
A list of
  \item{R.squared}{ the multiple R-squared (for linear models) or the Pseudo R-squared (for nonlinear models). }
  \item{adj.R.squared}{ the adjusted R-squared. }
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{lm}}, \code{\link{summary.lm}}, \code{\link{aov}},
 \code{\link{nls}}
}

\examples{
# example 1 [linear model]
y <- rnorm(10)
x <- 1:10
fit <- lm(y ~ x)
summary(fit)
Rsq(fit)

# example 2 [nonlinear model for Load Bearing Capacity]
data(compaction)
attach(compaction)
out <- fitlbc(theta = Mois, sigmaP = PS)
summary(out)
Rsq(out)

# End (not run)
}
