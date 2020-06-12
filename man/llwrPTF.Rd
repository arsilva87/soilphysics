\name{llwrPTF}
\alias{llwrPTF}

\title{ Least Limiting Water Range (LLWR) Using Pedo-Transfer Functions }

\description{
It calculates Least Limiting Water Range (LLWR) using pedo-transfer functions in according to Silva \& Kay (1997) and Silva et al. (2008), for Canadian and Brazilian soils, respectively.
}

\usage{ 
llwrPTF(air, critical.PR, h.FC, h.WP, p.density, Bd, clay.content, org.carbon = NULL)
}

\arguments{
  \item{air}{ the value of the limiting volumetric air content, \eqn{m^3 m^{-3}} }
  \item{critical.PR}{ the value of the critical soil penetration resistance, MPa }
  \item{h.FC}{ the value of matric suction at the field capacity, hPa }
  \item{h.WP}{ the value of matric suction at the wilting point, hPa }
  \item{p.density}{ the value of the soil particle density, \eqn{Mg m^{-3}} }
  \item{Bd}{ a numeric vector containing values of dry bulk density, \eqn{Mg m^{-3}}. Note that Bd can also be a vector of length 1. }
  \item{clay.content}{ a numeric vector containing values of clay content to each bulk density, \eqn{\%} }
  \item{org.carbon}{ a numeric vector containing values of organic carbon to each bulk density, \eqn{\%}, for Canadian soils. Default is 2\eqn{\%}. See details. }
}

\details{
Note that org.carbon is only required for Canadian soil. If it is not passed, LLWR for Canadian soil is calculated with 2\eqn{\%} of organic carbon.
}

\value{
A list of
  \item{LLWR.B}{ LLWR for Brazilian soils }
  \item{LLWR.C}{ LLWR for Canadian soils }
}

\references{
Keller, T; Silva, A.P.; Tormena, C.A.; Giarola, N.B.F., Cavalieri, K.M.V., Stettler, M.; Arvidsson, J. 2015. 
SoilFlex-LLWR: linking a soil compaction model with the least limiting water range concept. \emph{Soil Use and Management}, 31:321-329.

Silva, A.P.; Kay, B.D. 1997. Estimating the least limiting water range of soil from properties and management. 
\emph{Soil Science Society of America Journal}, 61:877-883.

Silva, A.P., Kay, B.D.; Perfect, E. 1994. Characterization of the least limiting water range. 
\emph{Soil Science Society of America Journal}, 61:877-883.

Silva, A.P., Tormena, C.A., Jonez, F.; Imhoff, S. 2008. Pedotransfer functions for the soil water retention and soil resistance 
to penetration curves. \emph{Revista Brasileira de Ciencia do Solo}, 32:1-10.
}

\author{
Renato Paiva de Lima <renato_agro_@hotmail.com>

Anderson Rodrigo da Silva <anderson.agro@hotmail.com>

Alvaro Pires da Silva <apisilva@usp.br>
}

\examples{
# EXEMPLE 1 (for Brazilian Soils) 
llwrPTF(air=0.1,critical.PR=2, h.FC=100, h.WP=15000,p.density=2.65,
        Bd=c(1.2,1.3,1.4,1.5,1.35),clay.content=c(30,30,35,38,40))

# EXEMPLE 2 (for Canadian Soils)
llwrPTF(air=0.1,critical.PR=2, h.FC=100, h.WP=15000,p.density=2.65,
        Bd=c(1.2,1.3,1.4),clay.content=c(30,30,30), org.carbon=c(1.3,1.5,2))

# EXEMPLE 3 (combining it with soil stress)
stress <- stressTraffic(inflation.pressure=200, 
	        recommended.pressure=200, 
	        tyre.diameter=1.8, 
	        tyre.width=0.4, 
	        wheel.load=4000, 
	        conc.factor=c(4,5,5,5,5,5),
                layers=c(0.05,0.1,0.3,0.5,0.7,1), 
	        plot.contact.area = FALSE)

stress.p <- stress$Stress$sigma_mean
layers <- stress$Stress$Layers
n <- length(layers)
def <- soilDeformation(stress = stress.p,
                p.density = rep(2.67,n),
	          iBD = rep(1.55,n), 
                N = rep(1.9392,n), 
                CI = rep(0.06037,n), 
                k = rep(0.00608,n), 
                k2 = rep(0.01916,n),
                m = rep(1.3,n),graph=TRUE,ylim=c(1.4,1.8))

# Grapth LLWR, considering Brazilian soils
plot(x = 1, y = 1, 
     xlim=c(0,0.2),ylim=c(1,0),xaxt = "n",
     ylab = "Soil Depth",xlab ="", type="l", main="")
axis(3)
mtext("LLWR",side=3,line=2.5)

i.LLWR <- llwrPTF(air=0.1,critical.PR=2, h.FC=100, 
                        h.WP=15000,p.density=2.65,
                        Bd=def$iBD,clay.content=rep(20,n))
f.LLWR <- llwrPTF(air=0.1,critical.PR=2, h.FC=100, 
                        h.WP=15000,p.density=2.65,
                        Bd=def$fBD,clay.content=rep(20,n))

points(x=i.LLWR$LLWR.B, y=layers, type="l"); points(x=i.LLWR$LLWR.B, y=layers,pch=15)
points(x=f.LLWR$LLWR.B, y=layers, type="l", col=2); points(x=f.LLWR$LLWR.B, y=layers,pch=15, col=2)

# End (not run)
}
