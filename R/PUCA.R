#' @name PUCA-package
#' 
#' @aliases PUCA-package PUCA
#' 
#' @docType package
#' 
#' @title Provenancing Using Climate Analogues
#' 
#' @description An R package to identify provenances occupying projected analogous climates using contemporary and future climate projections.
#' 
#' @details Global climates are changing, and recent ecological restoration literature is pushing for a paradigm shift from traditional local provenancing to new restoration/reforestation provenancing strategies. The code implemented in this package employs the theory behind the climate-adjusted provenancing strategy of Prober \emph{et al.} 2015 by identifying areas of a species distribution which currently occupy analogous climates projected for the future. 
#'
#' The core function to implement the climate-adjusted provenancing strategy is \code{seedSource}, which can also be implemented via the graphical user interface (GUI) by calling \code{seedSourcing_GUI}. These functions rely on the climate and distribution data being named correctly prior to been read into \code{R}, and it is highly recommended the user first read the example vignette.
#' 
#' \packageDESCRIPTION{PUCA}
#' \packageIndices{PUCA}
#' 
#' @author P. A. Harrison
#' 
#' Maintainer: P. A. Harriosn <P.A.Harrison@utas.edu.au>
#' 
#' @references
#' Harrison PA, Vaillancourt RE, Harris RMB, Potts BM (2017) Integrating climate change and habitat fragmentation to identify candidate seed sources for ecological restoration.\emph{Restoration Ecology}, 24, 524-531.
#'
#' Prober, SM, Byrne, M, McLean, EH, Steane, DA, \emph{et al.} (2015) Climate-adjusted provenancing: A strategy for climate-resilient ecological restoration. \emph{Frontiers in Ecology and Evolution}, 3, 1-5.