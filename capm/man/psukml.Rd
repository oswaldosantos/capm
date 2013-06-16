\name{psukml}
\alias{psukml}
\title{Creates a folder with *.kml files of a subset of polygons form a polygon shapefile}
\usage{
  psukml(mp = NULL, psu = NULL, id = NULL)
}
\arguments{
  \item{mp}{polygon shapefile layer name.}

  \item{psu}{the values to be matched.}

  \item{id}{column of the *.dbf file with the values to be
  matched against.}
}
\value{
  A folder with *.kml files of the subsetted polygons.
}
\description{
  Subset polygons acording to the matches between a vector
  and a specified column from a
  \link[sp]{SpatialPolygonsDataFrame}.
}
\examples{
# Load data with the polygon identifiers.
data(psu.ssu)

# Take a sample of 20 psu with probability
# proportional to size with replacement.
(selected.psu = ppssr(psu.ssu, 20, write = FALSE))

# Load the polyogn shapefile.
mplyer <- system.file('shp/santos.shp', package="capm")

# Create *kml files of 10 polygons.
psukml(mplyer, selected.psu[, 1], 1)
}
\seealso{
  \code{\link{readShapeSpatial}}
}

