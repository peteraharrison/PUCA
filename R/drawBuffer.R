#' Draw a buffer around a point
#' 
#' @description A helper function for function \code{seedSource}. This function first creates a a polygon of class 'SpatialPolygonsDataFrame' and subsamples the polygon to create a uniform point patten of class 'SpatialPoints' which is retunred to an object.
#' 
#' @param x The longitude coordinate for the centre of the buffer.
#' @param y The latitude coordinate for the centre of the buffer.
#' @param r The radius (in kilometers) of the buffer.
#' @param numVert The number of vertices to create the buffer.
#' @param n The number of uniform points to create within the buffer
#' @param mask Optinal mask shapefile (.shp) used to clip the buffer.
#' 
#' @return An object of class 'SpatialPoints' 
#' 
#' @author P. A. Harrison
#' 
#' @seealso \code{\link{seedSource}}, \code{\link[plotrix]{draw.circle}}, \code{\link[sp]{SpatialPolygons}}, \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{spsample}}
#' 
#' @examples 
#' #' #load libraries
#' library(raster)
#' library(sp)
#' 
#' tmp <- PUCA::drawBuffer(x = 147.4686, y = -42.008, r = 5)
#' plot(tmp)
#' 
#' @export
#' @import sp raster
#' 
drawBuffer = function (x, y, r, numVert = 1000, n = 20, mask = NULL){
  r = r/100 #r*0.008333333 #convert to dd
  #-- Get values to draw a circle
  angle.inc <- 2 * pi/numVert
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  for (circle in 1:length(r)){
    xv <- cos(angles) * r[circle] + x
    yv <- sin(angles) * r[circle] + y
  }
  tmpCirc = data.frame(x = xv, y = yv)
  tmpCirc = rbind(tmpCirc, tmpCirc[1,])
  
  #-- Convert buffer to spatial
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(tmpCirc)), ID=1)))
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  projection(sp_poly_df) <- CRS("+proj=longlat +datum=WGS84")
  if(!is.null(mask)) {sp_poly_df <- crop(sp_poly_df, mask)}
  
  #-- Create points in buffer
  psamp = spsample(sp_poly_df,n = n, type="regular")    
  
  #-- Return points within the buffer
  invisible(psamp)
}