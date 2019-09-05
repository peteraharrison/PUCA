#' Georeference a set of coordinates from \code{seedSource}
#' 
#' @description A helper function to georeference the top 20 coordinates from the output of \code{seedSource} using \code{geoCode}.
#' 
#' @param data The output data frame of \code{seedSource}
#' @param rank The column name used to rank the data frame
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
georefPoints <- function(data, rank, printToScreen = T){
  #-- Rank data
  tmpD <- data[order(data[, rank], decreasing = F), ]
  
  #-- Georefernce the distribution points
  tmpR <- tmpD[1:20, ]
  geoL <- list()
  for(g in 1:nrow(tmpR)){
    tmpGeoRef <- geoCode(address = paste(tmpR$Latitude[g], tmpR$Longitude[g], sep=","))
    geoL[[g]] <- tmpGeoRef
    # tmpLoc <- rbind(strsplit(tmpGeoRef, " ")[[1]])
    # geoL[[g]] <- if(length(tmpLoc) == 4){paste0(tmpLoc[, 1])} else{paste(tmpLoc[,1], tmpLoc[,2], sep = " ")}
  }
  # geoL1 <- lapply(geoL, "[[", function(x){strsplit(x = x, split = ",")[[1]][2]})
  
  geoRefs <- do.call(rbind.data.frame, geoL)
  colnames(geoRefs) <- "location"
  geoRefs$locations <- apply(geoRefs, 1, function(x){strsplit(x = x, split = ",")[[1]][2]})
  tmpOut <- data.frame(tmpR, Location = geoRefs$locations, row.names = NULL)
  # row.names(tmpOut) <- 1:n
  if(printToScreen){print(tmpOut)}
  invisible(tmpOut)
}
