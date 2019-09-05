#' Find what species occur within a polygon
#' 
#' @description A wrapper function that calls \code{ALA4R} to searche the Atlas of Living Australia for species occurnances within a polygon of size \code{radius}.
#' 
#' @param x A longitude coordinate
#' @param y A latitude coordinate
#' @param r The radius (in kilometers) of the polgon
#' @param numVert The number of vertices to create the polygon
#' 
#' @details 
#' This feature is currently only available for searches within Australia. Later versions will implement a call to the GBIF API server. Use \code{getSpecies2} to download occurance records for a species outside of Australia. 
#' 
#' This function relies on the wapper function \code{specieslist} of package \code{ALA4R}, and we greatly acknowledge the creator(s) of this package.
#'
#' @return A \code{data.frame} containing the kingdom the species belongs to, the species name, their common name (if known), and the number of occurnance records
#'
#' @author P. A. Harrison
#' 
#' @examples
#' # Not run
#' #library(sp)
#' #library(raster)
#' 
#' #tmp <- PUCA::speciesInPolygon(x = 147.4686, y = -42.008, r = 5)
#' #head(tmp)
#'
#' @export
#' @import sp raster
#' 
speciesInPolygon <- function (x, y, r, numVert = 1000){
  ## buffer drawing based on plotrix::draw.circle()
  r <- r/100 # r*0.008333333
  #-- Get values to draw a circle
  angle.inc <- 2 * pi/numVert
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  for(circle in 1:length(r)){
    xv <- cos(angles) * r[circle] + x
    yv <- sin(angles) * r[circle] + y
  }
  tmpCirc <- data.frame(x = xv, y = yv)
  tmpCirc <- rbind(tmpCirc, tmpCirc[1,])
  
  #-- Convert buffer to spatial
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(tmpCirc)), ID = 1)))
  projection(sp_poly) <- CRS("+proj=longlat +datum=WGS84")
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data = data.frame(ID = 1))
  e <- extent(sp_poly)
  wkt <- paste("POLYGON((", paste(e@xmin, e@ymin, collapse = " "), ",",
               paste(e@xmax, e@ymin, collapse = " "), ",",
               paste(e@xmax, e@ymax, collapse = " "), ",",
               
               paste(e@xmin, e@ymax, collapse = " "), ",",
               paste(e@xmin, e@ymin, collapse = " "),"))", sep = "")

  #lonlat <- sp_poly@polygons[[1]]@Polygons[[1]]@coords
  #temp = chull(lonlat)
  #lonlat = lonlat[c(temp,temp[1]),] 
  #wkt = paste("POLYGON((",paste(apply(lonlat,1,function(z)paste(z,collapse=" ")),collapse=","),"))",sep="")
  #wkt = rgeos::writeWKT(sp_poly)
  spList <- ALA4R::specieslist(wkt = wkt, fq = "rank:species")
  spList <- data.frame(kingdom = spList$kingdom, 
                       species = spList$speciesName,
                       commonName = spList$commonName,
                       occurrenceCount = spList$occurrenceCount)
  spList <- spList[with(spList, order(kingdom, species)), ]
  invisible(spList)
}