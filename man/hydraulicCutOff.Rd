\name{hydraulicCutOff}
\alias{hydraulicCutOff}

\title{The matric potential at the point of hydraulic cut-off obtained from DE (Dexter et al., 2008) and GG (Groenevelt & Grant, 2004) water retention curves.}

\description{
The pore water suction at the point of hydraulic cut-off occurs at the point where the residual 
water content, obtained from Dexter et al. (2008), intercepts with the Groenevelt & Grant (2004) 
retention curve.
}

\usage{
hydraulicCutOff(theta_R, k0, k1, n, x0 = 6.653)
}

\arguments{
  \item{theta_R}{ a parameter that represents the residual water content at the the Dexter's (2008) Water Retention Model.}
  \item{k0}{ a parameter value, extracted from the water retention curve based on the Groenevelt & Grant (2004) formula.}
  \item{k1}{ a parameter value, extracted from the water retention curve based on the Groenevelt & Grant (2004) formula.}
  \item{n}{ a parameter value, extracted from the water retention curve based on the Groenevelt & Grant (2004) formula.}
  \item{x0}{ the value of pF (pore water suction) at which the soil water content becomes zero. The default is 6.653.}
}

\value{
The water suction at the point of hydraulic cut-off.
}

\references{
Dexter, A.R.; Czyz, E.A.; Richard, G.; Reszkowska, A. (2008). A user-friendly water retention function that takes
	account of the textural and structural pore spaces in soil. \emph{Geoderma}, 143:243--253.

Groenevelt, P.H.; Grnat, C.D. (2004). A new model for the soil-water retention curve that solves the problem of 
	residual water contents. \emph{European Journal of Soil Science}, 55:479--485.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{fitsoilwater2}}, \code{\link{fitsoilwater3}}
}

\examples{
# Dexter et al. (2012), Table 4A
hydraulicCutOff(0.1130, 6.877, 0.6508, 1.0453)
hydraulicCutOff(0.1122, 12.048, 0.4338, 2.0658)

# End (not run)
}
