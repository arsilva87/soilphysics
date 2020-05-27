\name{soilDeformation}
\alias{soilDeformation}

\title{ Soil deformation by O'Sullivan and Robertson (1996) }

\description{
It calculates bulk density variation as a function of the applied mean normal stress using critical state theory, by O'Sullivan and Robertson (1996).
}

\usage{
soilDeformation(stress, p.density, iBD, N, CI, k, k2, m, graph = FALSE, ...)
}

\arguments{
  \item{stress}{ a numeric vector containing the values of mean normal stress, kPa; Note that stress can also be a vector of length 1.}
  \item{p.density }{ a numeric vector containing the values of particle density to each stress, \eqn{Mg m^{-3}}.}
  \item{iBD }{ a numeric vector containing the values of initial bulk density to each stress, \eqn{Mg m^{-3}}.}
  \item{N }{ the specific volume at p = 1 kPa, to each stress}
  \item{CI }{ the compression index, to each stress; check details}
  \item{k }{ the recompression index, to each stress; check details}
  \item{k2 }{ the slope of the steeper recompression line to each stress (similar to the k' in O'Sullivan and Robertson (1996) model); check details}
  \item{m }{ the value that separates yield line and VCL to each stress; check details}
  \item{graph}{ logical; shall soilDeformation plot the graph model (only the first parameters set is ploted)?}
  \item{...}{ further graphical arguments. See \link{par}.}
}

\details{
The specific volume (v) is given as \eqn{v=PD/BD}, where PD is particle density and BD is the bulk 
density. Please, check each parameter from O'Sullivan and Robertson (1996) model in the figure below.

\figure{SMP.jpeg}{options: scale = 0.7}
}

\value{
A list of
  \item{iBD }{ initial bulk density, \eqn{Mg m^{-3}}}
  \item{fBD }{ final bulk density, \eqn{Mg m^{-3}}}
  \item{vi}{ initial specific volume}
  \item{vf }{ final specific volume}
  \item{I\eqn{\%} }{ variation of soil bulk density (\eqn{\%}) after the applied stress}
}

\references{
O'Sullivan, M.F.; Robertson, E.A.G. 1996. Critical state parameters from intact samples of two 
agricultural soils. \emph{Soil and Tillage Research}, 39:161-173.

Keller, T.; Defossez, P.; Weisskopf, P.; Arvidsson, J.; Richard, G. 2007. SoilFlex: a model 
for prediction of soil stresses and soil compaction due to agricultural field traffic 
including a synthesis of analytical approaches. \emph{Soil and Tillage Research}, 93:391-411.
}

\author{
Renato Paiva de Lima <renato_agro_@hotmail.com>

Anderson Rodrigo da Silva <anderson.agro@hotmail.com>

Alvaro Pires da Silva <apisilva@usp.br>
}


\examples{
# EXAMPLE 1
soilDeformation(stress = 300,
                p.density = 2.67,
	        iBD = 1.55, 
                N = 1.9392, 
                CI = 0.06037, 
                k = 0.00608, 
                k2 = 0.01916,
                m = 1.3,graph=TRUE,ylim=c(1.4,1.8))

# EXEMPLE 2 (combining it with soil stress)
stress <- stressTraffic(inflation.pressure=200, 
	        recommended.pressure=200, 
	        tyre.diameter=1.8, 
	        tyre.width=0.4, 
	        wheel.load=4000, 
	        conc.factor=c(4,5,5,5,5,5),
                layers=c(0.05,0.1,0.3,0.5,0.7,1), 
	        plot.contact.area = FALSE)

stress.mean <- stress$Stress$sigma_mean
layers <- stress$Stress$Layers
n <- length(layers)

def <- soilDeformation(stress = stress.mean,
                p.density = rep(2.67, n),
	        iBD = rep(1.55,n), 
                N = rep(1.9392,n), 
                CI = rep(0.06037,n), 
                k = rep(0.00608,n), 
                k2 = rep(0.01916,n),
                m = rep(1.3,n),graph=TRUE,ylim=c(1.4,1.8))

# Graph
plot(x = 1, y = 1, 
     xlim=c(1.4,1.7),ylim=c(1,0),xaxt = "n",
     ylab = "Soil Depth",xlab ="", type="l", main="")
axis(3)
mtext("Bulk Density",side=3,line=2.5)

initial.BD <- def$iBD
final.BD <- def$fBD
points(x=initial.BD, y=layers, type="l")
points(x=initial.BD, y=layers,pch=15)
points(x=final.BD, y=layers, type="l", col=2)
points(x=final.BD, y=layers,pch=15, col=2)

# End (not run)
}
