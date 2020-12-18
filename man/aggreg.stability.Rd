\name{aggreg.stability}
\alias{aggreg.stability}

\title{ Soil Aggregate-Size Distribution }

\description{
It calculates the mean weight diameter (MWD), the geometric mean diameter (GMD) and the soil aggregates size 
distribution per class based on the mass of the aggregates retained in each sieve from a total soil mass used 
for the soil aggregate stability test. 
}

\usage{ 
aggreg.stability(sample.id = NA, dm.classes, aggre.mass)
}

\arguments{
  \item{sample.id}{ optional; a character vector containing the sample names. }
  \item{dm.classes}{ a numeric vector containing the aggregates classes, in mm. }
  \item{aggre.mass}{ a \code{data.frame} consisting of columns with soil aggregates mass (g) of each one 
	of the corresponding \code{dm.classes}. }
}

\value{
A \code{data.frame} containing valor of MWD, GMD, total soil mass (\code{total.mass}) used in the aggregate stability 
test and the percentage of soil aggregate size distribution per class.
}

\details{
The user must arrange a \code{data.frame} with lines representing the samples and the columns representing the mass 
of the aggregates retained in each one of the meshes (corresponding to each size class) in the aggregate stability test.

\figure{aggregequations.jpg}{options: scale = 0.5}
}

\references{
W. Kemper, W. Chepil. (1965). Size distribution of aggregates. C. Black (Ed.). \emph{Methods of Soil Analysis}, 
	American Society Agronomy, Madison. pp. 499-510.

Yoder, R. A. (1936). A direct method of aggregate analysis of soils and a study of the physical nature of erosion losses. 
	\emph{Journal of the American Society of Agronomy}, 28:337-351.
}

\author{ 
Renato Paiva de Lima <renato_agro_@hotmail.com>
}

\examples{
data(SoilAggregate)
classes <- c(3, 1.5, 0.75, 0.375, 0.178, 0.053)
aggreg.stability(sample.id = SoilAggregate[ ,1], 
   dm.classes = classes, aggre.mass = SoilAggregate[ ,-1])

# End (not run)
}