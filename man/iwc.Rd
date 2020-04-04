\name{iwc}
\alias{iwc}

\title{ Integral Water Capacity (IWC) }

\description{
Quantifying the soil water availability for plants through the IWC approach.
The theory was based on the work of Groenevelt et al. (2001), Groenevelt et al. (2004) and 
Asgarzadeh et al. (2014), using the van Genuchten-Mualem Model for estimation of the water 
retention curve and a simple power model for penetration resistance. The salinity effect on 
soil available water is also implemented here, according to Groenevelt et al. (2004). 
}

\usage{
iwc(theta_R, theta_S, alpha, n, a, b, hos = 0, 
    graph = TRUE, 
    xlab = "Matric head (cm)", 
    ylab = expression(cm^-1), 
    xlim1 = NULL,
    xlim2 = NULL, 
    xlim3 = NULL,
    ylim1 = NULL, 
    ylim2 = NULL, 
    ylim3 = NULL,
    col12 = c("black", "blue", "red"),
    col3 = c("orange", "black"),
    lty12 = c(1, 3, 2), 
    lty3 = c(2, 1), ...)
}

\arguments{
  \item{theta_R}{ the residual water content (\eqn{m^3~m{-3}}); a numeric parameter from van Genuchten's model; see details.}
  \item{theta_S}{ the water content at saturation (\eqn{m^3~m{-3}}); a numeric parameter from van Genuchten's model; see details.}
  \item{alpha}{ a scale parameter from van Genuchten's model; see details.}
  \item{n}{ a shape parameter from van Genuchten's model; see details.}
  \item{a}{ a parameter of the soil penetration resistance model; see details.}
  \item{b}{ a parameter of the soil penetration resistance model; see details.}
  \item{hos}{ optional; the value of osmotic head of the saturated soil extract (cm). Used only if one is 
	concerned about the salinity effects on the water available for plants. Default is zero. 
	See Groenevelt et al. (2004) for more details.}
  \item{graph}{logical; if TRUE (default), graphics for both dry and wet range are built.}
  \item{xlab}{ a label for x-axis.}
  \item{ylab}{ a label for y-axis.}
  \item{xlim1, xlim2, xlim3}{ the x limits (x1, x2) of each plot. See \code{\link{plot.default}}.}
  \item{ylim1, ylim2, ylim3}{ the y limits (y1, y2) of each plot. See \code{\link{plot.default}}.}
  \item{col12}{ a vector of length 3 containing the color of each line of the first two plots. See \code{\link{par}}.}
  \item{col3}{ a vector of length 2 containing the color of each line of the third plot. See \code{\link{par}}.}
  \item{lty12}{ a vector of length 3 containing the line types for the first two plots. See \code{\link{par}}.}
  \item{lty3}{ a vector of length 2 containing the line types for the third plot. See \code{\link{par}}.}
  \item{\dots}{ further graphical parameters. See \code{\link{par}}.}
}

\details{
The parameters of the van Genuchten-Mualem Model can be estimated through the function \code{fitsoilwater()}. 
The soil penetration resistance model is given by: \eqn{PR = a*h^b}, where \eqn{h} is the soil water content
and \eqn{a} and \eqn{b} are the fitting parameters.
}

\value{
A table containing each integration of IWC (integral water capacity, in m/m) and EI (integral energy calculation, in J/kg).
}

\references{
Asgarzadeh, H.; Mosaddeghi, M.R.; Nikbakht, A.M. (2014) SAWCal: A user-friendly program for 
	calculating soil available water quantities and physical quality indices. 
	\emph{Computers and Electronics in Agriculture}, 109:86--93.

Groenevelt, P.H.; Grant, C.D.; Semetsa, S. (2001) A new procedure to determine soil water availability.
	\emph{Australian Journal Soil Research}, 39:577--598.

Groenevelt, P.H., Grant, C.D., Murray, R.S. (2004) On water availability in saline soils. 
	\emph{Australian Journal Soil Research}, 42:833--840.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{soilwater}}, \code{\link{fitsoilwater}}, \code{\link{llwr}}
}

\examples{
# example 1 (Fig 1b, Asgarzadeh et al., 2014)
iwc(theta_R = 0.0160, theta_S = 0.4828, alpha = 0.0471, n = 1.2982, 
	a = 0.2038, b = 0.2558, graph = TRUE) 

# example 2 (Table 1, Asgarzadeh et al., 2014)
iwc(theta_R = 0.166, theta_S = 0.569, alpha = 0.029, n = 1.308, 
	a = 0.203, b = 0.256, graph = TRUE) 

# example 3: evaluating the salinity effect
iwc(theta_R = 0.166, theta_S = 0.569, alpha = 0.029, n = 1.308, 
	a = 0.203, b = 0.256, hos = 200, graph = TRUE)

# End (Not run)
}
