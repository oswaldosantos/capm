#' Creates a folder with *.kml files of a subset of polygons form a polygon shapefile
#' @description Subset polygons acording to the matches between a vector and a specified column from a \link[sp]{SpatialPolygonsDataFrame}.
#' @param mp polygon shapefile layer name.
#' @param psu the values to be matched.
#' @param id column of the *.dbf file with the values to be matched against.
#' @return A folder with *.kml files of the subsetted polygons.
#' @seealso \code{\link{readShapeSpatial}}
#' @export
#' @examples
#' # Load data with the polygon identifiers. 
#' data(psu.ssu)
#' 
#' # Take a sample of 20 psu with probability 
#' # proportional to size with replacement.
#' (selected.psu = ppssr(psu.ssu, 20, write = FALSE))
#' 
#' # Load the polyogn shapefile.
#' mplyer <- system.file('shp/santos.shp', package="capm")
#' 
#' # Create *kml files of 10 polygons.
#' psukml(mplyer, selected.psu[, 1], 1)
#' 
psukml <- function(mp = NULL, psu = NULL, id = NULL) {
  tmp <- readShapePoly(mp)
  proj4string(tmp) <- CRS('+proj=longlat +ellps=WGS84')
  tmp2 = NULL
  for(i in 1:length(psu)) {
    tmp1 <- tmp[which(as.character(tmp@data[ , id]) == psu[i]), ]
    writeOGR(tmp1, dsn = paste(eval(psu[i]), '.kml', sep =''), 
             layer = 'selected_psu', driver = 'KML')
    tmp2[i] <- which(as.character(tmp@data[ , id]) == psu[i])
  }
  tmp2 <- tmp[tmp2, ]
  writeOGR(tmp2, dsn = 'all_psu.kml', 
           layer = 'selected_psu', driver = 'KML')
  return(cat('\n', 'The maps are in the directory:', '\n\n', getwd()))
}