\name{K.h}
\alias{K.h}

\title{ Unsaturated Hydraulic Conductivity }

\description{ A closed-form analytical expressions for calculating the relative unsaturated hydraulic conductivity as a function of 
soil water tension (h) based on van Genuchten's water retention curve. }

\usage{
Ks(Ks, alpha, n, h)
}

\arguments{
  \item{Ks}{ Saturated hydraulic conductivity (e.g. cm/day). }
  \item{alpha}{ The scale parameter of the van Genuchten's model (hPa^-1). }
  \item{n}{ The shape parameter in van Genuchten's formula. }
  \item{h}{ The water tension (hPa). }
}

\value{ numeric, the value of unsaturated hydraulic conductivity. }

\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }

\references{
Guarracino, L. (2007). Estimation of saturated hydraulic conductivity Ks from the van Genuchten 
shape parameter alpha. \emph{Water Resources Research}, 43(11).

Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of 
unsaturated soils 1. \emph{Soil Science Society of America Journal} 44(5):892-898.
}

\examples{
# EXAMPLE 1
Kr(Ks = 1.06*10^2, alpha = 0.048, n = 1.5,h=100)

# EXAMPLE 2
x <- seq(log10(1), log10(1000),len=100)
h <- 10^x
y <- Kr(Ks = 1.06*10^2, alpha = 0.048, n = 1.5,h=h)
plot(x=h,y=y, log="yx", xlab="h (hPa)", yaxt='n',
    ylab="", ylim=c(0.001,100), xlim=c(1,10000))
mtext(expression(K[s] ~ (cm~d^-1)), 2, line=2)
ax <- c(0.001, 0.01, 0.1, 1, 10, 100)
axis(2,at=ax, labels=ax)

# End (not run)
}
