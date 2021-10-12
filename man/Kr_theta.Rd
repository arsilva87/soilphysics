\name{Kr_theta}
\alias{Kr_theta}

\title{ Unsaturated Hydraulic Conductivity as a function of water content}

\description{ A closed-form analytical expressions for calculating the relative unsaturated hydraulic conductivity as a function of 
soil water content based on van Genuchten's water retention curve. }
\usage{
Kr_theta(theta, thetaS, thetaR, n, Ks, f=0.5)
}
\arguments{
  \item{theta}{ The volumetric water content (m^3/m^3). }
  \item{thetaS}{ The volumetric water content at the saturation (m^3/m^3). }
  \item{thetaR}{ The volumetric residual water content (m^3/m^3). }
  \item{n}{ The shape parameter in van Genuchten's formula. }
  \item{Ks}{ Saturated hydraulic conductivity (e.g. cm/day). }
  \item{f}{ The pore-connectivity parameter. Default 0.5  [Mualem, 1976]. }
}
\value{ numeric, the value of unsaturated hydraulic conductivity. }
\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }
\references{
Guarracino, L. (2007). Estimation of saturated hydraulic conductivity Ks from the van Genuchten 
shape parameter alpha. \emph{Water Resources Research}, 43(11).

Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of 
unsaturated soils. \emph{Soil Science Society of America Journal} 44(5):892-898.

Mualem, Y. (1976). A new model for predicting the hydraulic conductivity of unsaturated porous 
media. \emph{Water Resour. Res.} 43(11):  513-522,
}
\examples{
# EXEMPLE 1
Kr_theta(theta=0.45,thetaS=0.5,thetaR=0.15, 
         n = 2, Ks = 1.06*10^2, f=0.5)

# EXEMPLE 2
thetaS <- 0.50
thetaR <- 0.15
theta <- seq(thetaS, thetaR, len=50)
y <- Kr_theta(theta=theta,thetaS=thetaS,thetaR=thetaR, 
              n = 2, Ks = 1.06*10^2, f=0.5)
             
# Just for this example, we are removing the "0" value 
# for plotting the graph in log scale, sence log10(0) results in "-Inf"
Kr <- y[-50]
w <- theta[-50]

plot(x=w,y=Kr,xlab=expression(theta~(m^3~m^-3)),
     ylim=c(0.001,100), log="y",yaxt='n',
     ylab="", xlim=c(0.15,0.50))
mtext(expression(K[r] ~ (cm~d^-1)), 2, line=2)
ax <- c(0.001, 0.01, 0.1, 1, 10, 100)
axis(2,at=ax, labels=ax)
# End (not run)
}
