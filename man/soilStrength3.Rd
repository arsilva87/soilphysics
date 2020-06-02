\name{soilStrength3}
\alias{soilStrength3}

\title{ Estimation of precompression stress by Saffih-Hdadi et al. (2009)}

\description{
It calculates the precompression stress using the pedo-transfer function from Saffih-Hdadi et al. (2009)
}

\usage{ 
soilStrength3(bulk.density, water.content, texture=c("VeryFine","Fine","MediumFine","Medium","Coarse"))

}

\arguments{
  \item{bulk.density}{ a numeric vector containing the values of soil bulk density, \eqn{Mg m^{-3}}}
  \item{water.content}{ a numeric vector containing the values of gravimetric water content, \eqn{\%}}
  \item{matric.suction}{ the soil texture group. See exemples}

}

\value{
  \item{PC}{the precompression stress}
}

\details{
The function returns 0 for soil properties values beyond the range for which the function was built.
}

\references{
Saffih-Hdadi, K.; Defossez, P.; Richard, G.; Cui, Y. J.; Tang, A. M.; Chaplain, V, 2009.
A method for predicting soil susceptibility to the compaction of surface layers as a function of water content and bulk density 
\emph{Geoderma}, 115:96-103.


}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
Anderson Rodrigo da Silva <anderson.agro@hotmail.com> 
}

\seealso{
\code{\link{stressTraffic}}
}


