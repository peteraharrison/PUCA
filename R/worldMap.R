#' The map of the world
#' 
#' Internal function to load the world map shapefile
#' 
#' @docType data
#' 
#' @usage data(worldMap)
#' 
#' @format A shapefile object of class \code{SpatialPolygonsDataFrame}
#' 
#' @source Map originally downloaded using \code{raster::getData( countries')}. See \code{?raster::getData} for details
#' 
#' @example 
#' data(worldMap)
#' donttest{plot(worldMap)}
"worldMap"