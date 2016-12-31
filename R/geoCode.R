#'Georeference a set of coordinates
#'
#'@description geoCode is a helper function for \code{georefPoints} and quries the Google Map API server to return the street address for a set of coordinates.
#'
#'@param address The latitude and longitude coordinates for a point of interest seperated by a comma, as class 'character'
#'@param print Logical argument whether to print the results to the consol
#'@param type The type of address to return. See Details for arguments.
#'
#'@details The function \code{geoCode} returns various annotations for a set of coordinates:
#'
#'type = 1 : returns the full street address
#'
#'type = 2 : returns the suburb with no street identifier
#'
#'type = 3 : returns the state and postcode (zipcode)
#'
#'type = 4 : return the state and country
#'
#'@section Warning:
#'The Google Map API has a maximum threshold limit on the number of points that can be georeferenced at any one time. Preliminary testing has found >1000 points in one request can result in the code stopping and returning an error message. There is also a threshold on the number of requests to the API server in a single day, and may result in the code stopping with an error message. If this occurs the threshold is reset in 24 hours. See \url{https://developers.google.com/maps/} for more details.
#'
#'@return A list of georeferenced locations
#'
#'@author P. A. Harrison
#'
#'@seealso \code{\link{georefPoints}}
#'
#'@examples 
#'# Load libraries
#'library(RJSONIO)
#'
#'# Create data frame
#'tmp <- data.frame(id = "test", longitude = 147.4686, latitude = -42.008)
#'
# Run function
#'
#' @export
#' @importFrom RJSONIO fromJSON
#'
geoCode <- function(address, print = F, type = 1){
  baseURL <- "http://maps.google.com/maps/api/geocode/json?sensor=false&"
  conURL <- paste(baseURL,'latlng=',URLencode(address), sep="") 
  con <- url(conURL)  
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con) 
  status <- data.json["status"]
  if(toupper(status) == "OK"){
    out = sapply(data.json$results,function(x) {list(address=x$formatted_address)})[type]#or chnage to [2] for more privacy
    out = paste(out,sep="")
  }else {out = paste("Georeferencing unknown")}
  if(print) print(out)
  invisible(out)
} 