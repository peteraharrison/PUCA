#' Wrapper function to plot the distribution of a species
#' 
#' @description This is a wrapper function that is called by \code{getSpecies2}.
#' 
#' @param region The region or country to plot.
#' @param extent The latitude and longitude extent of the plot region.
#' @param distribution The latitude and longitude distribution of a species
#' @param species The scientific or common name of a species
#' @param ... Additional arguments passed to \code{plot}
#' 
#' @author P. A. Harrison
#' 
#' @seealso \code{\link{getSpecies2}}, \code{\link[graphics]{plot}}
#' 
#' @examples 
#' # Not run 
#' 
#' library(ALA4R)
#' library(rgbif)
#' library(raster)
#' library(sp)
#' 
#' # Download the distribution for Eucalyptus ovata
#' # from the ALA server
#' 

#' #tmp <- PUCA::getSpecies2(species = "Eucalyptus ovata", client = "ALA",
#' #                         plot = FALSE, region = "Australia")
#' 
#' # Convert the latitude/longitude coodinates for a species
#' # to a 'SpatialPoints' class
#' #coordinates(tmp) <- Longitude + Latitude
#' #projection(tmp) <- CRS("+proj=longlat +datum=WGS84")
#' 
#' # Plot the distribution of your species
#' #PUCA::plotMyRegionegion(region = "Australia", distribution = tmp, 
#' #                  species = "Eucalyptus ovata")
#' 
#' @export                                   
#'
plotMyRegion <- function(region = NULL, extent = NULL, 
                        distribution = NULL, species = species, ...){
  if(inherits(region, "character")){
    #windows(width = 50, height = 50, title = species)
    plot(worldMap[which(worldMap@data$admin == region),], ...)
    if(inherits(distribution, "SpatialPoints")){points(distribution, pch = 21, 
                                                       col = "black", bg = "red", cex = 0.6)}
    title(substitute(expr = paste("Species name: ", italic(i)), 
                     env = list(i = species)))
  }
  if(length(extent) == 4){
    #windows(width = 50, height = 50, title = species)
    plot(worldMap, xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]), ...)
    if(inherits(distribution, "SpatialPoints")){points(distribution, pch = 21, 
                                                       col = "black", bg = "red", cex = 0.6)}
    title(expression(paste(italic(species))))
  }
}