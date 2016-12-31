#' Georeference a set of coordinates from \code{seedSource}
#' 
#' @description A helper function to georeference a set of coordinates from the output of \code{seedSource} using \code{geoCode}.
#' 
#' @param data The output data frame of \code{seedSource}
#' @param rank The column name used to rank the data frame
#' @param n The number of points to georeference
#' @param printToScreen Logic whether to print results to screen. Default is \code{TRUE}
#' 
#' @details The output data frame is first ranked in ascending order according to column name (i.e. "\code{climDist}"). The data frame is then reduced to \code{n} rows and georeferenced.
#' 
#' @return A data frame showing the coordinates, geographic distance (kilometers), climate distance, and georeferenced suburb ranked by \code{rank}. 
#' 
#' @author P. A. Harrison
#' 
#' @note See \code{geoCode} for further information on the limitation of points that can be georeferenced at any one time.
#' 
#' @seealso \code{\link{seedSource}}, \code{\link{geoCode}} 
#' 
#' @export
#' 
georefPoints <- function(data, rank, n, printToScreen = T){
  #-- Rank data
  tmpD <- data[order(data[, rank], decreasing = F), ]
  
  #-- Georefernce the distribution points
  tmpR <- tmpD[1:n, ]
  geoL = list()
  for(g in 1:nrow(tmpR)){
    tmpGeoRef = geoCode(paste(tmpR$Latitude[g], tmpR$Longitude[g], sep=","), type = 2)
    tmpLoc = rbind(strsplit(tmpGeoRef, " ",)[[1]])
    geoL[[g]] = if(length(tmpLoc) == 4){paste(tmpLoc[,1], sep="")} else{paste(tmpLoc[,1], tmpLoc[,2], sep = " ")}
  }
  geoRefs = do.call(rbind.data.frame, geoL)
  colnames(geoRefs) <- "location"
  tmpOut <- cbind(tmpR, geoRefs)
  row.names(tmpOut) <- 1:n
  if(printToScreen){print(tmpOut)}
  invisible(tmpOut)
}