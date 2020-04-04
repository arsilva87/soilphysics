\name{llwr}
\alias{llwr}

\title{Least Limiting Water Range (LLWR)}

\description{Graphical solution for the Least Limiting Water Range
and parameter estimation of the related water retention and penetration resistance curves.
A summary containing standard errors and statistical significance of the parameters is also given.
}

\usage{
llwr(theta, h, Bd, Pr, 
    particle.density, air, 
    critical.PR, h.FC, h.WP, 
    water.model = c("Silva", "Ross"), 
    Pr.model = c("Busscher", "noBd"),
    pars.water = NULL, pars.Pr = NULL, 
    graph = TRUE, graph2 = TRUE, 
    xlab = expression(Bulk~Density~(Mg~m^{-3})), 
    ylab = expression(theta~(m^{3}~m^{-3})), 
    main = "Least Limiting Water Range", ...) 
}

\arguments{
  \item{theta}{a numeric vector containing values of volumetric water content (\eqn{m^3~m^{-3}}).}
  \item{h}{a numeric vector containing values of matric head (cm, Psi, MPa, kPa, ...).}
  \item{Bd}{a numeric vector containing values of dry bulk density (\eqn{Mg~m^{-3}}).
	Note that \code{Bd} can also be a vector of length 1. See details.}
  \item{Pr}{a numeric vector containing values of penetration resistance (MPa, kPa, ...).}
  \item{particle.density}{the value of the soil particle density (\eqn{Mg~m^{-3}}).}
  \item{air}{the value of the limiting volumetric air content (\eqn{m^3~m^{-3}}).}
  \item{critical.PR}{the value of the critical soil penetration resistance.}
  \item{h.FC}{the value of matric head at the field capacity (cm, MPa, kPa, hPa, ...).}
  \item{h.WP}{the value of matric head at the wilting point (cm, MPa, kPa, hPa, ...).}
  \item{water.model}{a character; the model to be used for calculating the soil water content. It must be one of the
	two: \code{"Silva"} (default) or \code{"Ross"}. See details.}
  \item{Pr.model}{a character; the model to be used to predict soil penetration resistance. It must be one of the two: 
	\code{"Busscher"} (default) or \code{"noBd"}. See details.}
  \item{pars.water}{optional; a numeric vector containing the estimates of the three parameters of the soil water retention 
	model employed. If \code{NULL} (default), \code{llwr()} estimates them using a Newton-Raphson algorithm. See details.}
  \item{pars.Pr}{optional; a numeric vector containing estimates of the three parameters of the model proposed by 
	Busscher (1990) for the functional relationship among \code{Pr}, \code{theta} and \code{Bd}. If NULL (default), 
	\code{llwr()} estimates them using a Newton-Raphson algorithm. Moreover, if \code{Pr.model = "noBd"}, then the 
	third value is considered to be null.}
  \item{graph}{logical; if TRUE (default) a graphical solution for the Least Limiting Water Range is plotted.}
  \item{graph2}{logical; if TRUE (default) a line of the Least Limiting Water Range as a function of bulk density is plotted. 
	If \code{graph = FALSE}, then \code{llwr()} will automatically consider \code{graph2 = FALSE} too.}
  \item{xlab}{a title for the x axis; the default is \eqn{Bulk~Density~(Mg~m^{-3})}.}
  \item{ylab}{a title for the y axis; the default is \eqn{\theta~(m^{3}~m^{-3})}.}
  \item{main}{a main title for the graphic; the default is "Least Limiting Water Range"}
  \item{\dots}{further graphical arguments.}
}

\details{
The numeric vectors \code{theta}, \code{h}, \code{Bd} and \code{Pr} are supposed to have the same length,
and their values should have appropriate unit of measurement. For fitting purposes, it is not advisable to use
vectors with less than five values. It is possible to calculate the LLWR for a especific (unique) value of bulk 
density. In This case, \code{Bd} should be a vector of length 1 and, therfore, it is not possible to fit the 
models \code{"Silva"} and \code{"Busscher"}, for water content and penetration resistance, respectively.

The model employed by Silva et al. (1994) for the soil water content (\eqn{\theta}) as a function of the soil bulk density (\eqn{\rho}) 
and the matric head (h) is:

\deqn{\theta = exp(a + b \rho)h^c}

The model proposed by Ross et al. (1991) for the soil water content (\eqn{\theta}) as a function of the matric head (h) is:

\deqn{\theta = a h^c}

The penetration resistance model, as presented by Busscher (1990), is given by

\deqn{Pr = b0 * (\theta^{b1}) * (\rho^{b2})}

If the agrument \code{Bd} receives a single value of bulk density, then \code{llwr()} fits the following simplified model (option \code{noBd}):

\deqn{Pr = b0 * \theta^{b1}}

}

\value{
A list of
  \item{limiting.theta }{a \eqn{n x 4} matrix containing the limiting values of water content for each input 
	value of bulk density at the volumetric air content (\code{thetaA}), penetration resistance (\code{thetaPR}),
	field capacity (\code{thetaFC}) and wilting point (\code{thetaWP}).}
  \item{pars.water }{a "nls" object or a numeric vector containing estimates of the three parameters of the model employed by 
	Silva et al. (1994) for	the functional relationship among \code{theta}, \code{Bd} and \code{h}.}
  \item{r.squared.water }{a "Rsq" object containing the pseudo and the adjusted R-squared for the water model.}
  \item{pars.Pr }{a "nls" object or a numeric vector containing estimates of the three parameters of the penetration resistance model.}
  \item{r.squared.Pr }{a "Rsq" object containing the pseudo and the adjusted R-squared for the penetration resistance model.}
  \item{area }{numeric; the value of the shaded (LLWR) area. Calculated only when Bd is a vector of length > 1.}
  \item{LLWR }{numeric; the value of LLWR (\eqn{m^3~m^{-3}}) corresponding to \code{Bd}. Calculated only when Bd is a single value.}
}

\references{
Busscher, W. J. (1990). Adjustment of flat-tipped penetrometer resistance data to common water content. 
	\emph{Transactions of the ASAE}, 3:519-524.

Leao et al. (2005). An Algorithm for Calculating the Least Limiting Water Range of Soils. 
	\emph{Agronomy Journal}, 97:1210-1215.

Leao et al. (2006). Least limiting water range: A potential indicator of changes in near-surface 
	soil physical quality after the conversion of Brazilian Savanna into pasture. 
	\emph{Soil & Tillage Research}, 88:279-285.

Ross et al. (1991). Equation for extending water-retention curves to dryness.
	\emph{Soil Science Society of America Journal}, 55:923-927. 

Silva et al. (1994). Characterization of the least limiting water range of soils. 
	\emph{Soil Science Society of America Journal}, 58:1775-1781.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{fitbusscher}}
}

\examples{
# Example 1 - part of the data set used by Leao et al. (2005)
data(skp1994)
ex1 <- with(skp1994,
	llwr(theta = W, h = h, Bd = BD, Pr = PR,
		particle.density = 2.65, air = 0.1,
		critical.PR = 2, h.FC = 100, h.WP = 15000))
ex1

# Example 2 - specifying the parameters (Leao et al., 2005)
a <- c(-0.9175, -0.3027, -0.0835) # Silva et al. model of water content
b <- c(0.0827, -1.6087, 3.0570)   # Busscher's model
ex2 <- with(skp1994,
	llwr(theta = W, h = h, Bd = BD, Pr = PR,
		particle.density = 2.65, air = 0.1,
		critical.PR = 2, h.FC = 0.1, h.WP = 1.5,
		pars.water = a, pars.Pr = b))
ex2

# Example 3 - specifying a single value for Bd
ex3 <- with(skp1994,
	llwr(theta = W, h = h, Bd = 1.45, Pr = PR,
		particle.density = 2.65, air = 0.1,
		critical.PR = 2, h.FC = 100, h.WP = 15000))
ex3

# End (not run)
}
