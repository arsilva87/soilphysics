\name{llwr_llmpr}
\alias{llwr_llmpr}

\title{ Least Limiting Water (LLWR) and Matric Potential Ranges (LLMPR) }

\description{
A graphical solution and calculation of the least limiting water range and least limiting water matric potential range, including 
the corresponding the water content and water tensions limits. 
}

\usage{ 
llwr_llmpr(thetaR, thetaS, alpha, n, d, e, f = NULL, critical.PR, PD, Bd = NULL, 
         h.FC, h.PWP, air.porosity,
         labels = c("AIR", "FC", "PWP", "PR"), ylab = "",
         graph1 = TRUE, graph2 = FALSE, ...)
}

\arguments{
  \item{thetaR}{ the residual water content, \eqn{m^3 m^{-3}} }
  \item{thetaS}{ the water content at saturation , \eqn{m^3 m^{-3}} }
  \item{alpha}{ the scale parameter of the van Genuchten's model, \eqn{hPa^{-1}} }
  \item{n}{ the shape parameter of the van Genuchten's model }
  \item{d}{ a parameter of Busscher soil penetration resistance model. See details. }
  \item{e}{ a parameter of Busscher soil penetration resistance model. See details. }
  \item{f}{ a parameter of Busscher soil penetration resistance model. See details. }
  \item{critical.PR}{ the limiting value of soil penetration resistance, MPa }
  \item{PD}{ particle density, \eqn{Mg m^{-3}}  }
  \item{Bd}{ the bulk density to be displayed at bottom of the graph (optional), \eqn{Mg m^{-3}} }
  \item{h.FC}{ the value of water tension at field capacity, hPa }
  \item{h.PWP}{ the value of water tension at wilting point, hPa }
  \item{air.porosity}{ the volumetric air-filled porosity }
  \item{labels}{ the labels to h.FC, h.PWP, air.porosity and critical.PR }
  \item{ylab}{ a title for the y-axis }
  \item{graph1}{ logical; if TRUE (default) a graphical solution for the Least Limiting Water Range is displayed }
  \item{graph2}{ logical; if TRUE (default) a graphical solution for the Least Limiting Matric Potential Range is displayed }
  \item{...}{ Further graphical arguments }
}

\details{
The penetration resistance model, as presented by Busscher (1990), is given by PR = d * \eqn{\theta^{e} * BD^{f}}. 
In this model, BD (bulk density) is calculated from thetaS (soil total porosity) and PD (particles density), 
i.e., \eqn{BD = PD * thetaS^{-1}}. If the argument f is not passed, the model becomes \eqn{PR = d * \theta^{e}} .
}

\value{
A list of the LLWR and LLMPR, including the corresponding the water content and water tensions limits. 
}

\references{
Leon, H. N., Almeida, B. G., Almeida, C. D. G. C., Freire, F. J., Souza, E. R., Oliveira, E. C. A., Silva, E. P. 2019. 
	Medium-term influence of conventional tillage on the physical quality of a Typic Fragiudult with hardsetting behavior 
	cultivated with sugarcane under rainfed conditions. \emph{Catena}, 175: 37-46.

Busscher, W. J. 1990. Adjustment of flat-tipped penetrometer resistance data to common water content. 
	\emph{Transactions of the ASAE}, 3: 519-524.

van Genuchten, M. T. 1980. A closed-form equation for predicting the hydraulic conductivity of unsaturated soils 1. 
	\emph{Soil Science Society of America journal}, 44: 892-898.

Silva et al. 1994. Characterization of the least limiting water range of soils. 
	\emph{Soil Science Society of America Journal}, 58: 1775-1781.

Assouline, S., Or, D. 2014. The concept of field capacity revisited: Defining intrinsic static and dynamic criteria for soil internal drainage dynamics. 
	\emph{Water Resources Research}, 50: 4787-4802.

Millington, R. J., Quirk, J. P. 1961. Permeability of porous solids. 
	\emph{Transactions of the Faraday Society}, 57: 1200-1207.

Dexter, A. R., Czyz, E. A., Richard, G. 2012. Equilibrium, non-equilibrium and residual water: consequences for soil water retention. 
	\emph{Geoderma}, 177: 63-71.

Moraes, M. T., Bengough, A. G., Debiasi, H., Franchini, J. C., Levien, R., Schnepf, A., Leitner, D., 2018. Mechanistic framework to link root 
	growth models with weather and soil physical properties, including example applications to soybean growth in Brazil. 
	\emph{Plant and Soil}, 428: 67-92.
}

\author{
Renato Paiva de Lima <renato_agro_@hotmail.com>
}

\examples{
# Parameters from Leon et al. (2018), for usual physical restrictions threshold
  
  llwr_llmpr(thetaR=0.1180, thetaS=0.36, alpha=0.133, n=1.30, 
        d=0.005, e=-2.93, f=3.54, PD=2.65,
        critical.PR=4, h.FC=100, h.PWP=15000, air.porosity=0.1,
        labels=c("AFP", "FC","PWP", "PR"),
        graph1=TRUE,graph2=FALSE, ylab=expression(psi~(hPa)), ylim=c(15000,1))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)

  llwr_llmpr(thetaR=0.1180, thetaS=0.36, alpha=0.133, n=1.30, 
        d=0.005, e=-2.93, f=3.54, PD=2.65,
        critical.PR=4, h.FC=100, h.PWP=15000, air.porosity=0.1,
        graph1=FALSE,graph2=TRUE,
        labels=c("Air-filled porosity", "Field capacity", 
                "Permanent wilting point", "Penetration resistance"),
        ylim=c(0.1,0.30), ylab=expression(theta~(m^3~m^-3)))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)


# Without bulk density effects in Busscher's model (i.e. f=NULL)
 
  llwr_llmpr(thetaR=0.1180, thetaS=0.36, alpha=0.133, n=1.30, 
        d=0.0165, e=-2.93, PD=2.65,
        critical.PR=3, h.FC=100, h.PWP=15000, air.porosity=0.1,
        graph1=TRUE,graph2=FALSE,ylim=c(15000,1),
        ylab=expression(psi~(hPa)))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)

  llwr_llmpr(thetaR=0.1180, thetaS=0.36, alpha=0.133, n=1.30, 
        d=0.0165, e=-2.93, PD=2.65,
        critical.PR=3, h.FC=100, h.PWP=15000,air.porosity=0.1,
        graph1=FALSE,graph2=TRUE,
        ylim=c(0.1,0.30), ylab=expression(theta~(m^3~m^-3)))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)


# Parameters from Leon et al. (2018), calculated physical restrictions threshold

  thetaR <- 0.1180
  thetaS <- 0.36
  alpha <- 0.133
  n <- 1.30 
  clay.content <- 15 # clay content 15 %
  mim.gas.difusion <- 0.005
  root.elongation.rate <- 0.3 # root elogation rate 30%

  FC <- (1/alpha)*((n-1)/n)^((1-2*n)/n) # Assouline and Or (2014)
  PWP <- 10^(3.514 + 0.0250*clay.content) # Dexter et al. (2012)
  AIR.critical <-  (mim.gas.difusion*(thetaS)^2)^(1/(10/3)) # Millington and Quirk (1961)
  PR.critical <- log(root.elongation.rate)/-0.4325 # Moraes et al. (2018)

  llwr_llmpr(thetaR=thetaR, thetaS=thetaS, alpha=alpha, n=n, 
        d=0.005, e=-2.93, f=3.54, PD=2.65,ylim=c(15000,1),
        critical.PR=PR.critical, h.FC=FC, h.PWP=PWP, air.porosity=AIR.critical,
        graph1=TRUE,graph2=FALSE, ylab=expression(psi~(hPa)))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)

  llwr_llmpr(thetaR=thetaR, thetaS=thetaS, alpha=alpha, n=n, 
        d=0.005, e=-2.93, f=3.54, PD=2.65,
        critical.PR=PR.critical, h.FC=FC, h.PWP=PWP, air.porosity=AIR.critical,
        graph1=FALSE,graph2=TRUE,
        ylim=c(0.1,0.30), ylab=expression(theta~(m^3~m^-3)))
  mtext(expression("Bulk density"~(Mg~m^-3)),1,line=2.2, cex=0.8)

# End (not run)
}
