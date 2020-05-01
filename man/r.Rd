\name{r}
\alias{r}

\title{ Equation of capillary }

\description{ The equivalent pore radius as a function of soil water tension. }

\usage{
r(h, surface.tension.water=0.072, water.density=1000, water.pore.contact.angle=0)
}

\arguments{
  \item{h}{ The water tension (hPa). }
  \item{surface.tension.water}{ Surface tension of water (N/m). }
  \item{water.density}{ Density of water (kg/m^3). }
  \item{water.pore.contact.angle}{ Water pore contact angle (degrees). }
}

\value{ The equivalent pore radius, in micrometer.. }

\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }

\references{
Ghiberto, P. J., Imhoff, S., Libardi, P. L., Silva, A. P. D., Tormena, C. A., Pilatti, M. A. (2015). 
Soil physical quality of Mollisols quantified by a global index. \emph{Scientia Agricola}, 72(2):167-174.
}

\examples{
x <- seq(log10(1), log10(15000), len=50)
h <- 10^x
y <- r(h=h)
plot(x=h, y=y, log="yx", xlab="h (hPa)", yaxt='n', ylab="", ylim=c(0.1, 1500))
ax <- c(0.1, 1, 10, 100, 1000, 1500)
axis(2,at=ax, labels=ax)
mtext("Pore radius"~ (mu*m), 2, line=2.5)

# End (not run)
}
