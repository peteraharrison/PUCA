.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to PUCA version 1.0!\n\nThis is a beta version, so please report errors to <P.A.Harrison@utas.edu.au>.\n\n")
  library(ALA4R)
  library(FactoMineR)
  library(mefa)
  library(raster)
  library(rgbif)
  library(sp)
  library(tcltk)
  library(RJSONIO)
  library(maptools)
  library(tcltk2)
}