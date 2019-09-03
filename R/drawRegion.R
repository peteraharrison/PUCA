#' Draw a region around a point
#' 
#' @description Funtionally similar to drawBuffer but instead of subsampling the polygon, returns the drawn polygon.
#' 
#' @param x The longitude coordinate for the centre of the region.
#' @param y The latitude coordinate for the centre of the region.
#' @param r The radius (in kilometers) of the region.
#' @param numVert The number of vertices to create the region.
#' 
#' @return An object of class 'SpatialPolygon' 
#' 
#' @author P. A. Harrison
#' 
#' @seealso \code{\link{seedSource}}, \code{\link[plotrix]{draw.circle}}, \code{\link[sp]{SpatialPolygons}}, \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{spsample}}
#' 
#' @examples 
#' #load libraries
#' library(raster)
#' library(sp)
#' 
#' # run code
#' my_region <- PUCA::drawRegion(x = -42.008, y = 147.4686, r = 50)
#' projection(my_region) <- CRS("+proj=longlat +datum=WGS84")
#' plot(my_region)
#' 
#' @export
#' @import sp raster
#' 
drawRegion <- function (x, y, r, numVert = 1000){
  r <- r / 100 #r*0.008333333 #convert to dd = ca 1km
  #-- Get values to draw a circle
  angle.inc <- 2 * pi / numVert
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  for (circle in 1:length(r)){
    xv <- cos(angles) * r[circle] + x
    yv <- sin(angles) * r[circle] + y
  }
  tmpCirc = data.frame(x = xv, y = yv)
  tmpCirc = rbind(tmpCirc, tmpCirc[1,])
  
  #-- Convert buffer to spatial
  sp_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(tmpCirc)), ID = 1)))    
  
  #-- Return buffer
  invisible(sp_poly)
}
