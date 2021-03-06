#' Climatically matched sites to the Ross revegetation site
#' 
#' A list containing the sites to collect seed from for the Ross revegetation site based on contemporary climate (1976 - 2005) and the multi-model mean of six global circulation models representing three future time periods (2020s, 2050s, and 2080s).
#' 
#' @format A list containing 24 data frames. Each data frame has six variables:
#' \describe{
#' \item{id}{An identifier for the species and time period - see Details}
#' \item{Longitude}{The longitude coordinate of a distribution record}
#' \item{Latitude}{The latitude coordinate of a distribution record}
#' \item{collect}{A binary value wehter to collect the point or not based on climDist and the prior threshold. If collect = 1 then the point is below or equal to the prior threshold rule}
#' \item{climDist}{The Euclidean distance between a distribution point and the revegetation site in the multidimensional space defined by the retained Principal Components}
#' \item{geoDist}{The geographic distance (in kilometers) between a distribution point and the revegetation site}
#' }
#' 
#' @usage rossCollect
#' 
#' @details Each data frame within the list contains a unique \code{id} column. This identifier starts with '\code{output_}' which indicates the data is the output of the \code{seedSource} function. After this is the species names, which follow:
#' \describe{
#' \item{_amy_}{\emph{Eucalyptus amygdalina}}
#' \item{_pauci_}{\emph{Eucalyptus pauciflora}}
#' \item{_ten_}{\emph{Eucalyptus tenuiramis}}
#' \item{_ova_}{\emph{Eucalyptus ovata}}
#' \item{_rod_}{\emph{Eucalyptus rodwayi}}
#' \item{_vim_}{\emph{Eucalyptus viminalis}}
#' }
#' The last part of the name is the model and time slice used:
#' \describe{
#' \item{revegCurrent}{The contemporary climate}
#' \item{revegFuture_MMM_2020}{The mulit-model mean for the 2020s}
#' \item{revegFuture_MMM_2050}{The mulit-model mean for the 2050s}
#' \item{revegFuture_MMM_2080}{The mulit-model mean for the 2080s}
#' }
#' 
#' @source See \code{citation("PUCA")}
#' 
"rossCollect"