\name{particle.sedimentation}
\alias{particle.sedimentation}

\title{ Sedimentation time of soil particles in aqueous media }

\description{
It calculates the sedimentation time of soil particle in aqueous media using Stokes equation, i.e., 
the time needed for the particles of soil larger than the size attributed as input to sediment in aqueous 
media, usually water.
}

\usage{
particle.sedimentation(d, h=0.2, g=9.81, v=0.001, Pd=2650, Wd=1000) 
}

\arguments{
  \item{d}{ the lower limit of soil particle diameter (micrometers) to sediment withing the calculated time. }
  \item{h}{ the vertical distance (meters) from which the particles fall. Default is 0.2 m. }
  \item{g}{ the acceleration of gravity, in m/s^2. Default is 9.81 m/s^2. }
  \item{v}{ the viscosity of the fluid, in N/s/m^2. Default is 0.001 N/s/m^2, for water at 20 degrees Celsius. }
  \item{Pd}{ the particle density, in kg/m^3. Default is 2650 kg/m^3. }
  \item{Wd}{ the density of the fluid, in kg/m^3. Default is 1000 kg/m^3.}
}

\value{
A \code{data.frame} containing the estimated time for the sedimentation of particles.
}

\references{
Hillel, D. (2003). \emph{Introduction to environmental soil physics}. Elsevier. p.39-51. Doi:10.1016/B978-012348655-4/50004-6
}

\author{ Renato Paiva de Lima <renato_agro_@hotmail.com> }

\examples{
# Example 1
particle.sedimentation(d=2, h=0.2, g=9.81, v=1.002*10^-3, Pd=2650, Wd=1000) 

# Example 2
d <- c(2000, 200, 50, 10, 2, 1)
time <- particle.sedimentation(d=d, h=0.2, g=9.81, v=1.002*10^-3, Pd=2650, Wd=1000) 

plot(x=d, y=time$hours, log = "x", xaxt ="n", 
     ylab = "time of sedimentation (hours)", xlab = "particle diameter (micrometer)")
axis(1,at=d, labels=d)

# End (not run)
}
