\name{MapkmlPSU}
\alias{MapkmlPSU}
\title{Creates *.kml files of a subset of polygons from a polygon shapefile}
\usage{
  MapkmlPSU(mp = NULL, psu = NULL, id = NULL)
}
\arguments{
  \item{mp}{polygon shapefile layer name.}

  \item{psu}{the values to be matched.}

  \item{id}{column of the *.dbf file with the values to be
  matched against.}
}
\value{
  *.kml files of the subsetted polygons.
}
\description{
  Subset polygons acording to the matches between a vector
  and a specified column from a
  \link[sp]{SpatialPolygonsDataFrame}.
}
\details{
  If there is *.kml files in the working directory, the new
  files created will overwrite it in case of name matching.
}
\examples{
# Load data with the polygon identifiers.
data(psu.ssu)

# Take a sample of 20 PSU with probability
# proportional to size with replacement.
(selected.psu <- SamplePPS(psu.ssu, 20, write = FALSE))

# Load the polyogn shapefile.
mplyer <- system.file('shp/santos.shp', package="capm")

# Create *kml files of 10 polygons.
MapkmlPSU(mplyer, selected.psu[, 1], 1)
}
\seealso{
  \code{\link{readShapeSpatial}}
}

