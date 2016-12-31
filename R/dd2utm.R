#' Decimal degrees to Universal Transverse Mercator
#' 
#' @description A function to convert decimal degrees into universal transverse mercator coordinate system. This is an R implementation of the formulas provided by Prof. Steven Dutch.
#' 
#' @param id The column number containing the identifier for each row. This 'identifier' may be the name of a population/provenance or could be a sequence of numbers
#' @param long The column number containing the logitude decimal degree coordinates
#' @param lat The column number containing the latitude decimal degree coordinates
#' @param data The data frame containing the latitude/longitude coodinates. This can be of either class 'data.frame' or 'matrix'
#' 
#' @return 
#' An object of class 'data.frame' containing four columns: 
#' \describe{
#' \item{id}{The identifier from the original data file}
#' \item{north}{The north UTM coordinate}
#' \item{east}{The east UTM coordinate}
#' \item{zone}{The zone which the latitudes originate. Zone ranges from 1 (180 to 147 W) to 60}}
#' 
#' @author P.A. Harrison
#' 
#' @references 
#' CFF Karney (2010) Transverse Mercator with an accuracy of a few nanometers, http://arxiv.org/abs/1002.1417v3
#' 
#' \url{http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.HTM}
#' 
#' @examples 
#' # Create a data frame
#' tmp <- data.frame(id = "test", longitude = 147.4686, latitude = -42.008)
#' 
#' # Run function
#' PUCA::dd2utm(id = 1, long = 2, lat = 3, data = tmp)
#' 
#' # Output
#' #     id   north     east zone
#' # 1 test 5349229 538803.5   55
#' 
#' @export
#' 
dd2utm <- function (id, long, lat,data){
  #setup data
  attach(data, warn.conflicts = F)
  id2=id
  lonlat = matrix(nrow = nrow(data), ncol=3)
  lonlat[,1] = data[,id]
  lonlat[,2] = data[,long]
  lonlat[,3] = data[,lat]
  colnames(lonlat) = c("id", "long", "lat")
  lonlat = as.data.frame(lonlat)
  rm(long, lat, id)
  attach(lonlat, warn.conflicts = F)
  #constants
  eq.rad = 6378137
  pol.rad = 6356752.3142
  flat = 0.00335281066474748
  inv.flat = 298.257223563
  mean.rad = (eq.rad*pol.rad)^(1/2)
  scale.fact = 0.9996
  e = sqrt(1-(pol.rad/eq.rad)^2)
  e2 = e*e/(1-e*e)
  n = (eq.rad-pol.rad)/(eq.rad+pol.rad)
  #Meridional Arc constants
  a0 = eq.rad*(1-n+(5*n*(n/4))*(1-n) + (81*n^4/64)*(1-n))
  b0 = (3*eq.rad*n/2)*(1-n-(7*n*(n/8))*(1-n)+(55*(n^4/64)))
  c0 = (15*eq.rad*n*(n/16))*(1-n+(3*n*(n/4))*(1-n))
  d0 = (35*eq.rad*(n^3/48))*(1-n+11*n*(n/16))
  e0 = (315*eq.rad*(n^4/51))*(1-n)
  #cal constant
  sin1 = pi/(180*3600)
  #main calculation  
  mat = matrix(nrow = nrow(lonlat), ncol = 4)
  colnames(mat) = c("id","north", "east", "zone")
  for(i in 1:nrow(lonlat)){
    long.zone = as.integer(31+(long[i]/6))
    long.zoneCM = 6*long.zone-183
    delta.longRad = (long[i]-long.zoneCM)*3.141593/180
    lat.rad = lat[i]*pi/180
    long.rad = long[i]*pi/180
    rho = eq.rad*(1-e*e)/((1-(e*sin(lat.rad))^2)^(3/2))
    nu = eq.rad/((1-(e*sin(lat.rad))^2)^(1/2))
    mer.arc = a0*lat.rad-b0*sin(2*lat.rad)+c0*sin(4*lat.rad)-
      d0*sin(6*lat.rad + e0*sin(8*lat.rad))
    ki = mer.arc*scale.fact
    kii = nu*sin(lat.rad)*cos(lat.rad)/2
    kiii =((nu*sin(lat.rad)*cos(lat.rad)^2)*
             (5-tan(lat.rad)^2 +9*e2*cos(lat.rad)^2)+4*e2^2*cos(lat.rad)^4)* scale.fact
    kiv = nu*cos(lat.rad)*scale.fact
    kv = cos(lat.rad)^3*(nu/6)*(1-tan(lat.rad)^2+e2*cos(lat.rad)^2)*scale.fact
    a6 = ((delta.longRad)^6*nu*sin(lat.rad)^5/720)*
      (61-58*tan(lat.rad)^2+tan(lat.rad)^4 + 270*e2*cos(lat.rad)^2-330*e2*sin(lat.rad)^2)*scale.fact
    rawNorth = (ki+kii*delta.longRad*delta.longRad+kiii*delta.longRad^4)
    north = ifelse (rawNorth < 0, 10000000+rawNorth, rawNorth)
    east = 500000+((kiv*delta.longRad)+(kv*delta.longRad^3))
    zone = long.zone
    mat[i,1] <- id[i]
    mat[i,2] <- north
    mat[i,3] <- east
    mat[i,4] <- zone
  }
  mat = as.data.frame(mat)
  mat = cbind(data[,id2], mat[,c(2:4)])
  colnames(mat)<- c("id","north", "east", "zone")
  detach(lonlat)
  detach(data)
  return(mat)
}