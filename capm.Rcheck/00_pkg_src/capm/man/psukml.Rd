\name{psukml}
\alias{psukml}
\title{Creates a folder with *.kml files of a subset of polygons form a polygon shapefile}
\usage{
  psukml(mp = NULL, psu = NULL, id = NULL)
}
\arguments{
  \item{mp}{polygon shapefile layer name ommitting the
  extensions *.shp, *.shx and *dbf, which are added in the
  function.}

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

# Load the polyogn shapefile.
mplyer <- system.file('shp/santos.shp', package="capm")

# Create *kml files of 10 polygons.
psukml(mplyer, psu.ssu[seq(1, 100, 10), 1], id = 1)
}
\seealso{
  \code{\link{readShapeSpatial}}
}

