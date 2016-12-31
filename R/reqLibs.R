#' Download/load libraries
#' 
#' @description \code{reqLibs} loads a list of libraries by searching the \code{R} packages directory and calling the \code{library} function. If a library is missing from the \code{R} packages directory, the function will download the library from CRAN along with all dependancies.
#' 
#' @param package A list of packages to load/download
#' 
#' @author P. A. Harrison
#' 
#' @seealso \code{\link[base]{library}}
#' 
#' @examples 
#' # load the following libraries
#' libs <- c("ggplot2", "raster")
#' PUCA::reqLibs(libs)
#'
#' @export
#'
reqLibs <- function(package){
  installedLibs<-function(package){
    missing <- package[!(package %in% installed.packages()[,"Package"])]
    return(missing)
  }
  t=installedLibs(package)
  if(length(t)>0){
    defaultCRANmirror = "http://cran.ms.unimelb.edu.au/"
    new.packages <- t
    install.packages(new.packages, dependencies = TRUE)
  } 
  suppressMessages((suppressWarnings(sapply(package, library, character.only=T))))
  t=installedLibs(package)
  if(length(t)>0){return (cat("Error: libraries not loaded. Manually load libraries"))} else if(length(t)==0){cat("Libraries loaded")
  }
}