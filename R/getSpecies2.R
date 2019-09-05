#' A wrapper function to download and plot a species distribution
#' 
#' @description A helper function for \code{speciesInPolygon}.
#' 
#' @param species The scientific or common name of a species. 
#' @param client The API server which to quiry to download distribution records for the selected species. See Details for available clients.
#' @param plot Logic whether to plot the distribution of the selected species
#' @param region The geographic region where the species occurs - typically the country, i.e. Australia.
#' 
#' @details This function relies on the \code{ALA4R} and \code{rgbif} wrapper functions to quarier the API servers to download the distribution records for a specified species. The function first determines whether the species of interest occurs in Australia or outside of Australia. If the species occurs in Australia, then \code{client = "ALA"} and the Atlas of Living Australia (ALA) API server is quiried. If the species occurs outside of Australia, then \code{client = "GBIF"} and the Global Biodiversty Inforamtion Facility (GBIF) API server is quired. If \code{plot = T}, a new plotting window will open and the distirbution of the specified species will be plotted. 
#' 
#' @section Warning:
#' The code calls \code{window()} when \code{plot = T}. On Windows operating systmes, this option is relatively stable. However, on Mac operating systmes, \code{window()} can be quite buggy and can cause R to crash.
#' 
#' @return Returns a \code{data.frame} containing three columns: \code{species}, \code{Latitude}, \code{Longitude}.
#' 
#' @references 
#' ALA spatail portal (\url{http://spatial.ala.org.au/#})
#' 
#' \code{ALA4R} package (\url{https://github.com/AtlasOfLivingAustralia/ALA4R})
#' 
#' GBIF (\url{http://www.gbif.org/})
#' 
#' \code{rgbif} package (\url{https://github.com/ropensci/rgbif})
#' 
#' @author P. A. Harrison
#' 
#' @note 
#' The \code{ALA4R} and \code{rgbif} packages are only available from GitHub and are not yet on CRAN. To download these packages, use Hadley Wickham's \code{devtools} package. For example:
#'
#' \code{devtools::install_github("AtlasOfLivingAustralia/ALA4R")} for ALA4R
#'
#' \code{devtools::install_github("ropensci/rgbif")} for rgbif
#' 
#' @seealso \code{\link{speciesInPolygon}}, \code{\link{plotMyRegion}}
#' 
#' @examples
#' # not run
#' 
#' #' library(ALA4R)
#' library(rgbif)
#' library(raster)
#' library(sp)
#' 
#' # Download and plot the distribution for Eucalyptus ovata
#' # from the ALA server
#' 
#' #tmp <- PUCA::getSpecies2(species = "Eucalyptus ovata", client = "ALA",
#' #                         plot = TRUE, region = "Australia")
#' 
#' #head(tmp)
#' #            species Latitude Longitude
#' # 1 Eucalyptus ovata    -37.3     148.6
#' # 2 Eucalyptus ovata    -37.3     148.7
#' # 3 Eucalyptus ovata    -37.5     149.0
#' # 4 Eucalyptus ovata    -37.5     149.1
#' # 5 Eucalyptus ovata    -37.5     149.0
#' # 6 Eucalyptus ovata    -37.5     149.0
#' 
#' @export
#' @import sp raster ALA4R rgbif
#' 
getSpecies2 <- function(species, client, plot = TRUE, region){  
  if(client == "ALA"){
    #library(ALA4R)
    cat(paste("Downloading ", species, " data now...", sep = ""))
    speciesDat <- as.data.frame(ALA4R::occurrences(taxon = species, download_reason_id = 10)$data)
    speciesDat <- speciesDat[, names(speciesDat) %in% c("species", "latitude", "longitude")]
    colnames(speciesDat) <- c("species", "Latitude", "Longitude")
    speciesDat <- speciesDat[grep(pattern = species, x = speciesDat$species), ] #old PAH: which(speciesDat$species == species)
    speciesDat <- na.omit(speciesDat)
    cat("DONE!")
    
    #plot
    if(plot){
      data("worldMap")
      cat(paste("\nPlotting ", species, " data in new window...", sep = ""))
      speciesDatPlot <- speciesDat
      sp::coordinates(speciesDatPlot) <- ~Longitude+Latitude
      raster::projection(speciesDatPlot) <- sp::CRS(raster::projection(worldMap))
      plotMyRegion(region = region, distribution = speciesDatPlot, species = speciesDat$species[1])
      cat("DONE!\n\n")
    }
  }
  
  if(client == "GBIF"){
    #library(rgbif)
    cat(paste("Downloading ", species, " data now...", sep = ""))
    counts <- rgbif::occ_search(scientificName = species)$meta$count
    speciesDat <- rgbif::occ_search(scientificName = species, limit = counts)
    speciesDat <- as.data.frame(speciesDat['data'])
    speciesDat <- speciesDat[, names(speciesDat) %in% c('data.scientificName', "data.decimalLatitude", "data.decimalLongitude")]
    colnames(speciesDat) <- c("species", "Latitude", "Longitude")
    speciesDat <- speciesDat[grep(pattern = species, x = speciesDat$species), ]
    speciesDat <- na.omit(speciesDat)
    
    #plot
    if(plot){
      data("worldMap")
      cat(paste("\nPlotting ", species, " data in new window...", sep = ""))
      speciesDatPlot <- speciesDat
      sp::coordinates(speciesDatPlot) <- ~Longitude+Latitude
      raster::projection(speciesDatPlot) <- sp::CRS(raster::projection(worldMap))
      par(mar = c(1,1,1,1))
      plotMyRegion(region = region, distribution = speciesDatPlot, species = speciesDat$species[1])
    }
  }
  
  speciesDat <- speciesDat[, names(speciesDat) %in% c("species", "Latitude", "Longitude")]
  invisible(speciesDat)
}