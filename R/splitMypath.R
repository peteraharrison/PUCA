#' Split the path of a directory
#' 
#' @description A helper function that splits the path of a direcory based on either the '\\\' or '/'.
#' 
#' @param Dir The path name of a directory
#' 
#' @return A list of folder names, with the lowest nested folder occuring first,
#' and returns the reverse order of the directory such that the lowest level is first.
#' 
#' @author P. A. Harrison
#' 
#' @examples
#' # Example using '\'
#' tmp <- "C:\\Test\\Test1\\test2\\test3"
#' PUCA::splitMyDir(Dir = tmp)
#' # [1] "test3" "test2" "Test1" "Test"  "C:"   
#' 
#' # Example using '/'
#' tmp <- "C:/Test/Test1/test2/test3"
#' PUCA::splitMyDir(Dir = tmp)
#' # [1] "test3" "test2" "Test1" "Test"  "C:" 
#' 
#' @export
#' 
splitMyDir <- function(Dir) {
  rev(setdiff(strsplit(Dir,"/|\\\\")[[1]], ""))
} 