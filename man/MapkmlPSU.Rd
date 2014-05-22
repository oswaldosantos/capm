\name{MapkmlPSU}
\alias{MapkmlPSU}
\title{Creates *.kml files of a subset of polygons from a polygon shapefile}
\usage{
MapkmlPSU(shape = NULL, psu = NULL, id = NULL, path = ".")
}
\arguments{
  \item{shape}{string with the name of a polygon shapefile
  or an object of \code{\link{class}}
  \link[sp]{SpatialPolygonsDataFrame} (see examples).}

  \item{psu}{the values to be matched.}

  \item{id}{column of the *.dbf file with the values to be
  matched against.}

  \item{path}{string indicating the path to the folder
  containing the shapfile. If the shapefile is in the
  working directory or if \code{shape} argument is a
  shapefile, \code{path} can be ignored.}
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
If there are *.kml files in the working directory, the new
created files will overwrite it in case of name matching.

\code{shape} must receive a shapefile with appropriate
coordinate reference system, otherwise, \code{MapkmlPSU}
report an error.
}
\examples{
# Load data with the polygon identifiers.
data(psu.ssu)

# Take a sample of 10 PSU with probability
# proportional to size with replacement.
(selected.psu <- SamplePPS(psu.ssu, 10, write = FALSE))

## Define shape from shapefile.
shp.path <- system.file('extdata', package="capm")
# The code above used a shapefile avaliable in the
# capm package.
# You might want to write a code like:
# shp.path  <- 'path_to_the_folder_with_the_shapefile'

# Create *kml files of 10 polygons.
# Uncomment the following line to create kml files:
# MapkmlPSU('santos', selected.psu[, 1], 1, shp.path)

## Define the shape argument as an object x of class SpatialPolygonsDataFrame.
# MapkmlPSU(x, selected.psu[, 1], 1)
}
\references{
\url{http://oswaldosantos.github.io/capm}
}
\seealso{
\code{\link{readShapeSpatial}}
}

