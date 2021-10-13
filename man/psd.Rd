\name{psd}
\alias{psd}

\title{ Soil Pore Size Distribution }

\description{ The unimodal soil pore size distribution based on van Genuchten's model. }
\usage{
psd(thetaS, thetaR, alpha, n, h)
}
\arguments{
  \item{thetaS}{ the water content at saturation. }
  \item{thetaR}{ the residual water content. }
  \item{alpha}{ the scale parameter of the van Genuchten's model (hPa-1). }
  \item{n}{ the shape parameter in van Genuchten's formula. }
  \item{h}{ a vector of water tension (hPa) on the range of water retention curve. }
}
\value{ A numeric vector containing the soil pore size distribution as a function of soil water tension. }
\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }
\references{
Ghiberto, P. J., Imhoff, S., Libardi, P. L., Silva, A. P. D., Tormena, C. A., Pilatti, M. A. (2015). 
Soil physical quality of Mollisols quantified by a global index. \emph{Scientia Agricola}, 72(2):167-174.
Asgarzadeh, H., Mosaddeghi, M. R., Nikbakht, A. M. (2014). 
SAWCal: A user-friendly program for calculating soil available water quantities and physical quality indices. \emph{Computers and Electronics in Agriculture}, 109:86-93.
}
\examples{
# EXAMPLE 1
x <- seq(log10(1),log10(15000),len=100)
h <- 10^x
y <- psd(thetaR = 0.15,thetaS = 0.55, alpha = 0.048, n = 1.5, h=h)
plot(x=h,y=y, log="x", xlab="h (hPa)", ylab=expression(delta*theta/delta*h), ylim=c(0,0.005))
# EXAMPLE 2
x <- seq(log10(1),log10(15000),len=100)
h <- 10^x
y <- psd(thetaR = 0.20,thetaS = 0.61, alpha = 0.1232, n = 1.3380,h=h)
plot(x=h,y=y, log="x", xlab="h (hPa)", ylab=expression(delta*theta/delta*h), ylim=c(0,0.01))
# EXAMPLE 3
x <- seq(log10(1),log10(15000),len=100)
h <- 10^x
y <- psd(thetaR = 0.154,thetaS = 0.600, alpha = 0.103, n = 2.365,h=h)
plot(x=h,y=y, log="x", xlab="h (hPa)", ylab=expression(delta*theta/delta*h), ylim=c(0,0.03))
ax <- c(1,10,100,1000,10000)
radius <- r(h=ax)
axis(3,at=ax, labels=round(radius,2))
mtext("Equivalent pore radius"~(mu*m),3,line=2.5, cex=0.9)
# End (not run)
}
