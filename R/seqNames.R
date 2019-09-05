#' Create a sequence of names
#' 
#' @description \code{seqNames} creates a sequence of names that has length \code{quantity}
#' 
#' @param text A text variable that is repeated \code{quantity} times
#' @param quantity The maximum numerical value for the sequence
#' 
#' @details This code was adpated from Zurr et al (2013).
#' 
#' @references Zurr AF, Hilbe JM, Leno EA (2013) A beginner's guide to GLM and GLMM with R. Highland Statistics Ltd (Newburgh, UK).
#' 
#' @author P. A. Harrison
#' 
#' @examples
#' PUCA::seqNames('bio', 19)
#' 
#' # Returns
#' # "bio1"  "bio2"  "bio3"  "bio4"  "bio5"  "bio6"  "bio7"  "bio8"  "bio9"
#' # "bio10" "bio11" "bio12" "bio13" "bio14" "bio15" "bio16" "bio17" 
#' # "bio18" "bio19"
#' 
#' @export
#' 
seqNames <- function(text, quantity){
  if(!is.numeric(quantity)){stop("'quantity' must be a numerical value > 0")}
  if(quantity < 1){stop("'quantity' must be a numerical value > 0")}
  as.character(sapply(text, function(x){paste0(x, 1:round(quantity, 0))}))
}
