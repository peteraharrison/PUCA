% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawRegion.R
\name{drawRegion}
\alias{drawRegion}
\title{Draw a region around a point}
\usage{
drawRegion(x, y, r, numVert = 1000)
}
\arguments{
\item{x}{The longitude coordinate for the centre of the region.}

\item{y}{The latitude coordinate for the centre of the region.}

\item{r}{The radius (in kilometers) of the region.}

\item{numVert}{The number of vertices to create the region.}
}
\value{
An object of class 'SpatialPolygon'
}
\description{
Funtionally similar to drawBuffer but instead of subsampling the polygon, returns the drawn polygon.
}
\examples{
#load libraries
library(raster)
library(sp)

# run code
my_region <- PUCA::drawRegion(x = -42.008, y = 147.4686, r = 50)
projection(my_region) <- CRS("+proj=longlat +datum=WGS84")
plot(my_region)

}
\author{
P. A. Harrison
}
\seealso{
\code{\link{seedSource}}, \code{\link[plotrix]{draw.circle}}, \code{\link[sp]{SpatialPolygons}}, \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{spsample}}
}

