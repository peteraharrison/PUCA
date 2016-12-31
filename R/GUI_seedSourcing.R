#' Graphical user interface for \code{seedSource}
#' 
#' @description A graphical user interface (GUI) implementation of the \code{seedSource} function.
#' 
#' @details See the vignette for details on how to use the GUI. Additonally, see \code{seedSource} for futher details.
#' 
#' @author P. A. Harrison
#' 
#' @seealso \code{\link{seedSource}}
#' 
#' @examples
#' # Run the GUI
#' #PUCA::seedSource_GUI()
#' 
#' @export
#' @import tcltk
#' 
seedSource_GUI <- function(){

  ## Define fonts for GUI
  fontHeading <- tkfont.create(family="times",size=20,weight="bold",slant="italic")
  fontHeading1<-tkfont.create(family="times",size=14,weight="bold")
  fontHeading2<-tkfont.create(family="times",size=12,weight="bold")
  fontNormal<-tkfont.create(family="times",size=12)
  fontNormalSmall<-tkfont.create(family="times",size=9)
  
  fontHeading1a<-tkfont.create(family="times",size=14,weight="bold", underline=1)
  fontHeading2a<-tkfont.create(family="times",size=12,weight="normal", underline=0)
  
  fontSpecies<-tkfont.create(family="times",size=12,weight="normal", underline=0,slant="italic")
  x = c('fontHeading', 'fontHeading1', 'fontHeading2', 'fontNormal', 'fontNormalSmall',
        'fontHeading1a', 'fontHeading2a')
  lapply(x, function(x){assign(x = x, value = get(x), envir = .GlobalEnv)})
  
  ## get directory for logos
  logoDir <- paste(path.package("PUCA"), '/extdata/', sep = "")
 
  start1 <- tktoplevel(bg='white')
  assign(x = 'start1', value = start1, envir = .GlobalEnv)
  z.GUI_start(logoDir = logoDir, start1 = start1)
  
  #cat("\014") #clear console
  
  cat("
      ==========================================================

      Welcome to Provenancing Using Climate Analogues (PUCA)
      (Version 1.0) (c) 2016 PA Harrison

      Launching the Graphical User Interface

      Please read the Terms and Conditions for use of this
      program. If you agree to the Terms and Conditions please 
      click 'I agree'.

      ==========================================================
      ")
}




