% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMyRegion.R
\name{plotMyRegion}
\alias{plotMyRegion}
\title{Wrapper function to plot the distribution of a species}
\usage{
plotMyRegion(region = NULL, extent = NULL, distribution = NULL,
  species = species, ...)
}
\arguments{
\item{region}{The region or country to plot.}

\item{extent}{The latitude and longitude extent of the plot region.}

\item{distribution}{The latitude and longitude distribution of a species}

\item{species}{The scientific or common name of a species}

\item{...}{Additional arguments passed to \code{plot}}
}
\description{
This is a wrapper function that is called by \code{getSpecies2}.
}
\examples{
# Not run 

library(ALA4R)
library(rgbif)
library(raster)
library(sp)

# Download the distribution for Eucalyptus ovata
# from the ALA server

#tmp <- PUCA::getSpecies2(species = "Eucalyptus ovata", client = "ALA",
#                         plot = FALSE, region = "Australia")

# Convert the latitude/longitude coodinates for a species
# to a 'SpatialPoints' class
#coordinates(tmp) <- Longitude + Latitude
#projection(tmp) <- CRS("+proj=longlat +datum=WGS84")

# Plot the distribution of your species
#PUCA::plotMyRegionegion(region = "Australia", distribution = tmp, 
#                  species = "Eucalyptus ovata")

}
\author{
P. A. Harrison
}
\seealso{
\code{\link{getSpecies2}}, \code{\link[graphics]{plot}}
}

