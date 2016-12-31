#' Straight-line distance between two sets of coordinates
#' 
#' @description Calculates the geographic distance (in kilometers) between a reference point and a set of coordinates using the Haversine formula (great-circle distance).
#' 
#' @param ref The reference Latitude and Longitude coordinates used as the 'starting point' for the geographic distance measure.
#' @param coords The Latitude and Longitude coordinates representing the 'end point' for the geographic distance measure. 
#' 
#' @section Warning:
#' It is assumed the coordinates will have column names matching "Latitude" and "Longitude". If these column names are not found the function will fail. Use \code{colnames(x) <- c("Latitude", "Longitude")} to change the column names, where \code{x} is the data frame
#' 
#' @details More than one value can be supplied to the \code{coords} data frame. See example below.
#' 
#' @return A numeric value in kilometers between the reference point and each point in \code{coord}
#' 
#' @author P. A. Harrison
#' 
#' @examples
#' # set up the 'ref' data frame
#' tmp0 <- data.frame(Longitude = 147.4686, Latitude = -42.008)
#' 
#' # set up the 'coord' data frame
#' tmp1 <- data.frame(Longitude = 147.1541, Latitude = -41.8339)
#' 
#' # run the function
#' PUCA::geoDist(tmp0, tmp1)
#' #   geoDist
#' # 1   32.47
#' 
#' #not ran
#' #set up red and coord in same data frame
#' tmp <- rbind(tmp0, tmp1)
#' 
#' #run the function
#' #PUCA::geoDist(tmp[1,], tmp[2,])
#' #   geoDist
#' # 1   32.47
#' 
#' # set up the 'coord' data frame with more than one point
#' tmp1 <- data.frame(Longitude = c(147.1541, 147.457063), Latitude = c(-41.8339, -41.980778))
#' 
#' # run the function
#' geoDist(tmp0, tmp1)
#' 
#' #   geoDist
#' # 1   32.47
#' # 2    3.18
#' 
#' @export
#' 
geoDist = function (ref,coords){
  rad <- pi/180
  R <- 6378.145
  #-- Reference coordinant
  ref$lat <- ref$Latitude * rad
  ref$lon <- ref$Longitude * rad
  
  #-- Coordinates to compare to
  coords$lat <- coords$Latitude * rad
  coords$lon <- coords$Longitude * rad
  
  #-- Calculate differences
  dist = list()
  for(i in 1:nrow(coords)){
    dlon <- coords$lon[i] - ref$lon
    dlat <- coords$lat[i] - ref$lat
    a <- (sin(dlat/2))^2 + cos(ref$lat) * cos(coords$lat[i]) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    dist[[i]] <- R * c  ####
  }
  dist = do.call(rbind.data.frame, dist)[1]
  colnames(dist)<-"geoDist"
  return(round(dist,2))
}