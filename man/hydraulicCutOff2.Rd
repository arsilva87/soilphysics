\name{hydraulicCutOff2}
\alias{hydraulicCutOff2}

\title{The matric potential at the point of hydraulic cut-off using the point of maximum curvature of DE (Dexter et al. 2008) water retention curve. 
}

\description{
The pore water suction at the point of hydraulic cut-off occurs at the point where the residual 
water content, obtained from Dexter et al. (2008), intercepts with the Groenevelt & Grant (2004) 
retention curve. This function calculates the Hydraulic Cut-Off using the point of maximum curvature of the 
DE (Dexter et al. 2008) curve.
}

\usage{
hydraulicCutOff2(theta_R, a1, a2, p1, p2, graph = FALSE, ...)
}

\arguments{
  \item{theta_R}{ the residual water content from Dexter's (2008) water retention curve (g/g).}
  \item{a1}{ a water content parameter from Dexter's (2008) water retention curve (g/g).}
  \item{a2}{ a water content parameter from Dexter's (2008) water retention curve (g/g).}
  \item{p1}{ a matric potential parameter from Dexter's (2008) water retention curve (hPa).}
  \item{p2}{ a matric potential parameter from Dexter's (2008) water retention curve (hPa).}
  \item{graph}{ logical; if TRUE a graphical solution with the maximum curvature point is displayed.}
  \item{...}{ further graphical arguments. See \code{\link{par}}}
}

\details{
The arguments are the fitting parameters from Dexter's (2008) water retention curve, which can be fitted using 
\code{\link{fitsoilwater3}}. Further examples of how to use these parameters are given in Dexter et al. (2012). 
}

\value{
A \code{data.frame} containing the values of matric potential (hPa), pF and water content (w) at the hydraulic cut-off (hco) point.
}

\references{
Dexter, A.R.; Czyz, E.A.; Richard, G.; Reszkowska, A. (2008). A user-friendly water retention function that takes
	account of the textural and structural pore spaces in soil. \emph{Geoderma}, 143:243--253.

Dexter, A.R., Czyz, E.A., Richard, G. (2012). Equilibrium, non-equilibrium and residual water: consequences for soil 
	water retention. \emph{Geoderma}, 177:63--71.
}

\author{
Renato Paiva de Lima <renato_agro_@hotmail.com>
}

\seealso{
\code{\link{hydraulicCutOff}}, \code{\link{fitsoilwater3}}
}

\examples{

# Example 1:  soils from Dexter et al. (2012), Table 4

hydraulicCutOff2(theta_R=0.1130,a1=0.0808,a2=0.0576,p1=4043.2,p2=269.1,
                  graph = TRUE, ylim=c(-0.05,0.15)) # Soil 1

hydraulicCutOff2(theta_R=0.0998,a1=0.1456,a2=0.0162,p1=3156.0,p2=71.51,
                  graph = TRUE, ylim=c(-0.20,0.30)) # Soil 4

hydraulicCutOff2(theta_R=0.0709,a1=0.0195,a2=0.1794,p1=4467.5,p2=1395.5,
                  graph = TRUE, ylim=c(-0.20,0.30)) # Soil 7

hydraulicCutOff2(theta_R=0.0359,a1=0.1014,a2=0.0459,p1=1282.4,p2=56.93,
                  graph = TRUE, ylim=c(-0.10,0.20)) # Soil 10

hydraulicCutOff2(theta_R=0.0736,a1=0.0522,a2=0.0321,p1=3516.2,p2=90.54,
                  graph = TRUE, ylim=c(-0.05,0.15)) # Soil 14

# Example 2:

# Fitting the water retention curve through the Dexter's (2008) curve
h <- c(0.001, 50.65, 293.77, 790.14, 992.74, 5065, 10130, 15195)
w <- c(0.5650, 0.4013, 0.2502, 0.2324, 0.2307, 0.1926, 0.1812, 0.1730)
if (interactive()) {
   fitsoilwater3(theta=w, x=h)
}

# Using the fitted parameter
hydraulicCutOff2(theta_R=0.1738,a1=0.07505,a2=0.316,p1=3673,p2=70.38,
                  graph = TRUE, ylim=c(-0.40,0.60))


# End (not run)
}
