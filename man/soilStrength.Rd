\name{soilStrength}
\alias{soilStrength}

\title{ Estimation of the Load Bearing Capacity by precompression stress }

\description{
Calculates the soil strength using precompression stress for different clay contents and matric suctions.
}

\usage{ 
soilStrength(clay.content, matric.suction = NULL, water.content = NULL)
}

\arguments{
  \item{clay.content}{ a numeric vector containing the values of clay for each soil layer, \%. 
	Note that it can also be a unique value. }
  \item{matric.suction}{ a numeric vector containing the values of matric suction for each clay content, kPa. }
  \item{water.content}{ a numeric vector containing the values of water content for each clay content, \eqn{m^3 m^{-3}}. 
	Note that water.content must be passed if \code{matric.suction} is not. See details. }
}

\value{
A two-columns data frame: 
  \item{Pc}{ the precompression stress (Severiano et al. 2013)}
  \item{LL.Pc}{ the lower limit of precompression stress in acoording to the Terranimo model criteria (see Stettler et al. 2014). Note: LL.Pc = Pc*0.5}
  \item{UL.Pc}{ the upper limit of precompression stress in acoording to the Terranimo model criteria (see Stettler et al. 2014). Note: UL.Pc = Pc*1.1}
}

\details{
Intervals of soil water content/matric suction to be used as input for estimating soil strength according to Severiano et al. (2013).

\figure{limitsSoilStrength.jpg}{options: scale = 0.5}
}

\references{
Severiano, E.C; Oliveira, G.C.; Dias Junior, M.S.; Curi, N.C.; Costa, K. A.P.; Carducci, C.E. 2013. 
Preconsolidation pressure, soil water retention characteristics, and texture of Latosols in the Brazilian Cerrado. 
\emph{Soil Research}, 51:193-202.

Stettler, M., Keller, T., Weisskopf, P., Lamande, M., Lassen, P., Schjonning, P., 2014. 
Terranimo - a web-based tool for evaluating soil compaction. \emph{Landtechnik}, 69:132-137.
}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
 
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 

Alvaro Pires da Silva <apisilva@usp.br> 
}

\seealso{
\code{\link{stressTraffic}}
}

\examples{
# EXEMPLE 1 (using water content)
soilStrength(clay.content=c(25,28,30,30,30),
             water.content = c(0.26,0.27,0.29,0.32,0.32))


# EXEMPLE 2 (using matric suction)
soilStrength(clay.content=c(25,28,30,30,30),
             matric.suction = c(100,330,1000,3000,5000))


# EXAMPLE 3 (combining it with soil stress)
stress <- stressTraffic(inflation.pressure=200, 
	        recommended.pressure=200, 
	        tyre.diameter=1.8, 
	        tyre.width=0.4, 
	        wheel.load=4000, 
	        conc.factor=c(4,5,5,5,5,5),
                layers=c(0.05,0.1,0.3,0.5,0.7,1), 
	        plot.contact.area = FALSE)

strength <- soilStrength(clay.content=c(25,28,30,30,30,30),
              matric.suction = c(30,100,100,100,200,200))


# Graph
plot(x = 1, y = 1, 
     xlim=c(0,300),ylim=c(1,0),xaxt = "n",
     ylab = "Soil Depth",xlab ="", type="l", main="")
axis(3)
mtext("Vertical Stress",side=3,line=2.5)

stressz <- stress$Stress$Z
layers <- stress$Stress$Layers
points(x=stressz, y=layers, type="l")

# Green zone
x0 <- strength$LL.Pc
x1 <- rep(0,length(layers)) 
y0 <- layers
y1 <- rev(layers)
polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
       col=rgb(red=0, green=1, blue=0, alpha=0.3))

# Yellow zone
x0 <- strength$UL.Pc
x1 <- rev(strength$LL.Pc)
y0 <- layers
y1 <- rev(layers)
polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
       col=rgb(red=1, green=1, blue=0, alpha=0.3))

# Red zone
x0 <- rep(300,length(layers)) 
x1 <- rev(strength$UL.Pc)
y0 <- layers
y1 <- rev(layers)
polygon(x=c(x0,x1), y = c(y0,y1),density = NA, 
       col=rgb(red=1, green=0, blue=0, alpha=0.3))

# End (not run)
}
