\name{voidratio}
\alias{voidratio}

\title{ Void Ratio }

\description{ A function to calculate the soil void ratio. }

\usage{
voidratio(wetsoil, drysoil, diam.cylinder, height.cylinder,
    dens.particle, deformation)
}

\arguments{
  \item{wetsoil}{ the weight of wet soil. }
  \item{drysoil}{ the weight of dry soil. }
  \item{diam.cylinder}{ the diameter of the cylinder. }
  \item{height.cylinder}{ the heigth of the cylinder. }
  \item{dens.particle}{ the particle density. }
  \item{deformation}{ a numeric vector containing soil deformation values. }
}

\value{ A numeric vector with same length of \code{deformation} containig void ratio values. }

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{ \code{\link{sigmaP}} }

\examples{
def <- c(0, 0.0230, 0.0352, 0.0605, 0.1070, 0.1750, 0.2525, 0.3395, 0.4250)
pres <- c(1, 12.5, 25, 50, 100, 200, 400, 800, 1600)
VR <- voidratio(wetsoil = 170.62, drysoil = 134.08, diam.cylinder = 6.95, 
               height.cylinder = 2.5, dens.particle = 2.61, def)
VR

plot(VR ~ pres, type = "b",
	ylab = "Void ratio",
	xlab = "Applied stress (kPa)",
	main = "Compression curve", 
	log = "x")

# End (not run)
}
