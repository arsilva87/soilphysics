\name{sigmaP}
\alias{sigmaP}

\title{ Preconsolidation Stress }

\description{
A function to determine the preconsolidation stress (\eqn{\sigma_P}). It is a parameter obtained from 
the soil compression curve and has been used as an indicator of soil load-bearing capacity as well as to 
characterize the impacts suffered by the use of machines. 
The function \code{sigmaP()} contains implementations of the main methods for determining the pre-consolidation 
stress, such as the Casagrande method, the method of Pacheco Silva, the regression methods and the method of the 
virgin compression line intercept.
}

\usage{
sigmaP(voidratio, stress, n4VCL = 3,
    method = c("casagrande", "VCLzero", "reg1", "reg2", "reg3", "reg4", "pacheco"),
    mcp = NULL, graph = TRUE, ...)
}

\arguments{
  \item{voidratio}{ a numeric vector containing void ratio (or bulk density) values. }
  \item{stress}{ a numeric vector containing the applied stress sequence.}
  \item{n4VCL}{ the number of points for calculating the slope of the soil Virgin Compression Line (VCL),
                which is obtained by linear regression. }
  \item{method}{ a character indicating which method is to be computed; one of the following:
                 \code{casagrande} (default), \code{VCLzero}, \code{reg1}, \code{reg2}, \code{reg3},
                 \code{reg4} or \code{pacheco}; see Details. }
  \item{mcp}{ the maximum curvature point in log10 scale of \code{stress}; required only if the
   		method \code{casagrande} is used. }
  \item{graph}{ logical; if TRUE (default) the compression curve is plotted. }
  \item{\dots}{ further graphical arguments. }
}

\details{
\code{casagrande} is the method proposed by Casagrande (1936). The preconsolidation stress obtained via \code{VCLzero} corresponds
to the intersection of the soil \emph{Virgin Compression Line} (VCL) with the x-axis at zero applied stress, as described by
Arvidsson & Keller (2004). \code{reg1}, \code{reg2}, \code{reg3} and \code{reg4} are regression methods that obtain the preconsolidation
stress value as the intercept of the VCL and a regression line fitted with the first two, three, four and five points of the curve, respectively,
as described by Dias Junior & Pierce (1995). \code{pacheco} is the method of Pacheco Silva (ABNT, 1990).

You may follow the flowchart below to understand the determination of the preconsolidation stress through \code{sigmaP()}.

\figure{sigmaPfluxo.jpg}{options: scale = 0.7}
}

\value{
A list of
  \item{sigmaP}{ the preconsolidation stress.}
  \item{method}{ the method used as argument.}
  \item{mcp}{ the maximum curvature point in log10 scale of \code{stress}; stored only if the
	method \code{casagrande} is used.}
  \item{CI}{ the compression index.}
  \item{SI}{ the swelling index.}
}

\references{
ABNT - Associacao Brasileira de Normas Tecnicas. (1990). \emph{Ensaio de
    adensamento unidimensional}: NBR 12007. Rio de Janeiro. 13p.

Arvidsson, J.; Keller, T. (2004). Soil precompression stress
    I. A survey of Swedish arable soils. \emph{Soil & Tillage Research}, 77:85-95.

Bowles, J. A. (1986). \emph{Engineering Properties of Soils and their
    Measurements}, 3rd edition. McGraw-Hill Book Company, Inc. NY, 218pp.

Casagrande, A. (1936). \emph{The determination of the pre-consolidation
   load and its practical significance}. In: Proceedings of the
   International Conference on Soil Mech. and Found. Eng.
   (ICSMFE), Cambridge, MA, 22-26 June 1936, vol. 3. Harvard
   University, Cambridge, MA, USA, pp. 60-64.

Dias Junior, M. S.; Pierce, F. J. (1995). A simple procedure for
   estimating preconsolidation pressure from soil compression curves.
   \emph{Soil Technology}, 8:139-151.
}

\author{ Anderson Rodrigo da Silva <anderson.agro@hotmail.com> }

\seealso{
\code{\link{voidratio}}, \code{\link{maxcurv}}, \code{\link{fitlbc}}
}

\examples{
pres <- c(1, 12.5, 25, 50, 100, 200, 400, 800, 1600)
VR <- c(0.846, 0.829, 0.820, 0.802, 0.767, 0.717, 0.660, 0.595, 0.532)

plot(VR ~ log10(pres), type = "b") # find the 'mcp'
sigmaP(VR, pres, method = "casagrande", mcp = 1.6, n4VCL = 2)

# fitting the VCL
sigmaP(VR, pres, method = "casagrande", mcp = 1.6, n4VCL = 3)

# self-calculation of "mcp" argument for Casagrande method
sigmaP(VR, pres, method = "casagrande", n4VCL = 3)

# Pacheco method
sigmaP(VR, pres, method = "pacheco")

# Regression method
sigmaP(VR, pres, method = "reg3")

# End (not run)
}
