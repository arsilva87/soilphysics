\name{stressTraffic}
\alias{stressTraffic}

\title{
Predicting Soil Stress Due to Agricultural Trafficability
}

\description{
Contact area, stress distribuition and stress propagation based on the SoilFlex model 
(Keller 2005; Keller et al. 2007) are calculated. 
}

\usage{
stressTraffic(inflation.pressure, recommended.pressure, tyre.diameter, 
    tyre.width, wheel.load, conc.factor, layers, plot.contact.area = FALSE, ...) 
}

\arguments{
  \item{inflation.pressure}{ tyre inflation pressure, kPa}
  \item{recommended.pressure}{ recommended tyre inflation pressure at given load, kPa}
  \item{tyre.diameter}{ overall diameter of the unloaded tyre, m}
  \item{tyre.width}{ tyre width, m}
  \item{wheel.load}{ wheel load, kg}
  \item{conc.factor}{ concentration factor; a numeric vector ranging from 3 (wet soil) to 6 (dry soil), depending on water content.}
  \item{layers}{ a numeric vector containing values of depth (in meters) for the soil layers. Note that layers can also be a unique value}
  \item{plot.contact.area}{ logical; shall \code{soilTraffic} plot the distribution of stress over the contact area?}
  \item{...}{ further graphical arguments. See \link{par}.}
}

\value{
A list of
  \item{Area}{ Contact area parameters.}
  \item{Loads}{ Estimated wheel loads.}
  \item{Stress}{ Stress propagation into soil.}
  \item{stress.matrix}{ The matrix of applied stress at a specific depth and radial distance from the tyre centre.}
  \item{fZStress}{ The function of stress propagation in z direction (vertical stress).}
  \item{fmeanStress}{ The function of normal mean stress propagation.}
  \item{fStress}{ The function of stress propagation.}
  \item{fXStress}{ The function of stress propagation in x (footprint length or driving) direction.}
  \item{fYStress}{ The function of stress propagation in y (tire width) direction.}
}

\references{
Keller, T. 2005. A model to predict the contact area and the distribution of vertical stress below agricultural tyres 
	from readily-available tyre parameters. \emph{Biosyst. Eng.} 92, 85-96.

Keller, T.; Defossez, P.; Weisskopf, P.; Arvidsson, J.; Richard, G. 2007. SoilFlex: a model for prediction of soil 
	stresses and soil compaction due to agricultural field traffic including a synthesis of analytical approaches. 
	\emph{Soil and Tillage Research} 93, 391-411.
}

\author{
Renato Paiva de Lima <renato_agro_@hotmail.com>

Anderson Rodrigo da Silva <anderson.agro@hotmail.com>

Alvaro Pires da Silva <apisilva@usp.br>
}

\examples{
stress <- stressTraffic(inflation.pressure=200, 
	   recommended.pressure=200, 
	   tyre.diameter=1.8, 
	   tyre.width=0.4, 
	   wheel.load=4000, 
	   conc.factor=c(4,5,5,5,5,5),
           layers=c(0.05,0.1,0.3,0.5,0.7,1), 
	   plot.contact.area = TRUE)

stress

# Building a fancier plot for the contact area
# library(fields)
# image.plot(x = as.numeric(rownames(stress$stress.matrix)), 
#	       y = as.numeric(colnames(stress$stress.matrix)), 
#	       z = stress$stress.matrix,  
#	       xlab="Tyre footprint length (m)", ylab="Tyre width (m)") 
# End (not run)

# Stress Propagation 
# Vertical Stress
stress.v <- stress$Stress$sigma_vertical
layers <- stress$Stress$Layers
plot(x = 1, y = 1, xlim=c(0,300),ylim=c(1,0),xaxt = "n",
     ylab = "Soil Depth",xlab ="", type="l", main="")
axis(3)
mtext("Stress (kPa)",side=3,line=2.5)
lines(x=stress.v, y=layers)

# Mean normal stress
stress.p <- stress$Stress$sigma_mean
lines(x=stress.p, y=layers, lty=2)
legend("bottomright", c("Vertical stress", "Normal mean stress"), lty = 1:2)

# End (not run)
}
