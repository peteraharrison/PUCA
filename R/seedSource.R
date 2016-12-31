#' Find provenances using climate analoges
#' 
#' @description The core function of the \code{PUCA} package to implement the climate-adjusted provenancing strategy of Prober \emph{et al.} 2015.
#' 
#' @param poi A \code{data.frame} or \code{matrix} of Latitude and Longitude coordinates for the revegetation site or point of interest (POI)
#' @param species A list of species names that exactly match those in the global enivronement
#' @param region A defined region around the ecological resotration site. See Details for additional information
#' @param currClim A 'RasterStack' of contemporary climate layers. See Details for additional information
#' @param futureClim A 'RasterStack' of projected future climate layers. See Details for additional information
#' @param threshold A numeric value to transform the climate distance into a binary value
#' @param radius The radius of the buffer that is drawn around the revegetation site using \code{drawBuffer}
#' @param returnResults The number of top ranking distribution points to print to the console based on the climate distance between the revegetaion site and each distribution point
#' @param verbose Logic whether to print a log to the screen
#' 
#' @details The function first creates a uniform 'SpatialPoints' grid across the defined \code{region}, matching the resolution of the climate layers (i.e. if \code{res(currClim)[1]} equals 0.01 then the grid points are 0.01 apart). The contemporary climate is then extracted and passed to the  Principal Components Analysis (PCA), which is implemented using the \code{PCA} function of package \code{FactoMineR}. It is this PC climate space that is used to define the Euclidean distance between the revegetation site and each species distribution point. \strong{Thus, the definition of \code{region} is important and requires due consideration}. A region may de defined by (i) a specific distance from the revegetation site (see \code{drawRegion}), (ii) using the eco-regions of the world (i.e. http://wwf.panda.org/about_our_earth/ecoregions/ecoregion_list/), (iii) or bio-regions of a country (i.e. Australias IBRA bioregions -https://www.environment.gov.au/land/nrs/science/ibra/australias-bioregions-maps).
#'
#' The RasterStacks contain the contemporary and future climate projections. The only requirement for these RasterStacks is the number of climate variables for the projected future climate RasterStack must match the same number of climate variables in the contemporary climate RasterStack. For example, if the contemporary climate RasterStack contained 19 climate varaibles, then the future climate RasterStack \strong{must} contain projected values for the same 19 varaibles.
#' 
#' By default, if more than one global circulation model (i.e. future projection) is provided to the function, it will automatically create an ensemble of these models. The ensembled layers will thus be a multi-model mean by averaging each cell projection.
#'
#' @return 
#' There are numerious objects that are assigned to the \code{.GlobalEnv}, but the most important are:
#'
#' 1. The Principal Components Analysis (PCA) for the region, species, and revegetation site. The PCA is assigned for each species and begins with '\code{PCA_}'. Calling \code{ls(pattern = "PCA_+", envir = .GlobalEnv)} will list the various PCA objects.
#' 
#' 2. The output of the \code{seedSource} function. The output is uniquely assigned using the follow convention: '\code{output_species}', species name, whether its for the contemporary (\code{revegCurrent}) or future (\code{revegFuture}). For the future predictions there will be an additional two prefixs specifying the global circulation model (or multi-model mean (MMM) used and the time slice. Calling \code{ls(pattern = "output_+", envir = .GlobalEnv)} will list the various output objects.
#' 
#' @references 
#' Harrison PA, Vaillancourt RE, Harris RMB, Potts BM (in press) Predicting seed sources for future climates.\emph{Journal of Applied Ecology}.
#' 
#' Prober SM, Byrne M, McLean EH, Steane DA, \emph{et al.} (2015) Climate-adjusted provenancing: A strategy for climate-resilient ecological restoration. \emph{Frontiers in Ecology and Evolution}, 3, 1-5.
#' 
#' @author P. A. Harrison, B. M. Potts
#' 
#' @seealso \code{\link{drawRegion}}, \code{\link{drawBuffer}}, \code{\link[FactoMineR]{PCA}}, \code{\link{seedSource_GUI}}
#' 
#' @examples 
#' # See vignette for a detailed example
#' 
#' @export
#' @import raster sp FactoMineR tcltk mefa
#' @importFrom tcltk2 tk2text tk2scrollbar 
#' 
seedSource = function(poi, species, region, currClim, futureClim, threshold,
                      radius, returnResults, verbose){
  ## Set up log window
  ##===================
  if(verbose){
    log <- tktoplevel()
    tkfocus(log)  
    tkwm.geometry(log, "+200+0") # position in upper left corner of screen
    tkwm.minsize(log, 550,500)
    tkwm.title(log,"Log - seed sourcing function")
    
    log$env$scr <- tk2scrollbar(log, orient = "vertical",
                                command = function(...) tkyview(log$env$txt, ...))
    log$env$txt <- tk2text(log, bg = "white",
                           font = "courier", width = 60, height = 10,
                           yscrollcommand = function(...) tkset(log$env$scr, ...))
    tkgrid(log$env$txt, log$env$scr, sticky = "nsew")
    tkgrid.rowconfigure(log, log$env$txt, weight = 1)
    tkgrid.columnconfigure(log, log$env$txt, weight = 1)
    
    tkinsert(log$env$txt, "end", paste("Program started on ", format(Sys.time(), "%a %b %d %X %Y"), "\n\n", sep = ""))
    tkfocus(log$env$txt)
  }
  
  ## Pre-function checks
  ##=====================
  
  #-- a. revegetation site coordinates
  if(inherits(poi, "numeric") & length(poi <2)){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste0("\nError: Incorrect latitude/longitude coordinates entered!\n\n", sep = ""))
      tkfocus(log$env$txt)}
    stop("Incorrect latitude/longitude coordinates entered!")}
  if(inherits(poi, c("matrix", "data.frame")) & ncol(poi) < 2){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Error: Incorrect latitude/longitude columns entered!\n\n", sep = ""))
      tkfocus(log$env$txt)}
    stop("Incorrect latitude/longitude columns entered!")}
  
  #-- b. region
  if(!any(inherits(region, "numeric") | inherits(region, "SpatialPolygonsDataFrame"))){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Error: Restoration region has not be defined!\nExpected region to be class 'numeric' or class 'SpatialPolygonsDataFrame'\n\n", sep = ""))
      tkfocus(log$env$txt)}
    stop("Restoration region has not be defined! Expected region to be class 'numeric' or class 'SpatialPolygonsDataFrame'")}
  
  #-- c. climate layers
  if(length(currClim) < 1){ #ls(pattern = "climate_+", envir = .GlobalEnv)
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Error: No climate variables found!\n\n", sep = ""))
      tkfocus(log$env$txt)}
    stop("No climate variables found!")}
  
  #-- d. if n futureClim > 1 convert to multi-model mean
  future_mod <- unique(do.call(rbind.data.frame, strsplit(names(futureClim), "_", fixed = T))[,1])
  if(length(future_mod) > 1){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("More than one GCM selected. Using the multi-model mean. \n\n", sep = ""))
      tkfocus(log$env$txt)}
    
    newS <- stack()
    yrs <- unique(do.call(rbind.data.frame, strsplit(names(futureClim), "_", fixed = T))[,2])
    for(y in yrs){
      tmpbs <- subset(x = futureClim, which(grepl(y , tolower(names(futureClim)))))
      vars <- unique(do.call(rbind.data.frame, strsplit(names(tmpbs), "_", fixed = T))[,3])
      for(v in vars){
        tmpbs2 <- subset(x = tmpbs, which(grepl(paste(v,"$", sep="") , tolower(names(tmpbs)))))
        tmpbs2 <- calc(tmpbs2, fun = sum)/length(future_mod)
        names(tmpbs2) <- paste("MMM", y, v, sep = "_")
        newS <- addLayer(newS, tmpbs2)
      }
    }
    futureClim <- newS
  }
  
  ## 1. Make grid over region
  ##==========================
  
  #-- Sample a shapefile
  if(inherits(region, "SpatialPolygonsDataFrame")){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Reading restoration area shapefile and sampling revegetation region...\n\n", sep = ""))
      tkfocus(log$env$txt)}
    
    #-- Read in shapefile from directory and project
    #region <- readShapeSpatial(region)
    #projection(region) <- CRS("+proj=longlat +datum=WGS84")
    
    #-- sample region
    regionPoints = spsample(region, n = 1000, cellsize = res(currClim)[1], type = "regular")
    regionPoints_df <- data.frame(Longitude = regionPoints@coords[,1],
                                  Latitude = regionPoints@coords[,2])
  }
  
  #-- Sample a buffer
  if(inherits(region, "numeric")){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Creating restoration region and samping revegetation site...\n\n", sep = ""))
      tkfocus(log$env$txt)}
    
    #if(nrow(poi) > 1 & any(geoDist(poi[1,], poi[2:nrow(poi),]) < region)){}
    
    # could add in the ability to clip polygon around a specified altitude
    #-- Create buffer
    region <- drawRegion(x = poi$Longitude, y = poi$Latitude, r = region)
    projection(region) <- CRS("+proj=longlat +datum=WGS84")
    
    #-- sample region
    regionPoints = spsample(region, n = 1000, cellsize = res(currClim)[1], type = "regular")
    regionPoints_df <- data.frame(Longitude = regionPoints@coords[,1],
                                  Latitude = regionPoints@coords[,2])
  }
  
  
  ## 2. Make buffer and grid around revegetation site
  ##==================================================
  
  revegPoints <- drawBuffer(x = poi$Longitude, y = poi$Latitude, r = radius, 
                            n = 20, mask = region) #cellsize = res(climate_current)[1]
  revegPointsDD <- revegPoints@coords
  colnames(revegPointsDD) <- c("Longitude", "Latitude")
  if(verbose){
    tkfocus(log)
    tkinsert(log$env$txt, "end", paste("Extracting climate data for region and revegetation sites...\n\n", sep = ""))
    tkfocus(log$env$txt)}
  
  
  ## 3. Extract contemporary climate for region and revegetaion gird
  ##=================================================================
  
  currClimPoints <- extract(x = currClim, y = regionPoints, df = T)
  currClimPoints$ID <- rep("region", nrow(currClimPoints))
  
  currClimRevegPoints <- extract(x = currClim, y = revegPoints, df = T)
  currClimRevegPoints$ID <- rep("revegCurrent", nrow(currClimRevegPoints))
  
  ## 4. Extract future climate for revegetation grid
  ##=================================================
  
  ln <- names(futureClim)
  unique_ln <- unique(do.call(rbind.data.frame, strsplit(ln, "_", fixed = T))[,2]) #use to get gcm years
  tb <- list()
  for(i in unique_ln){
    unique_mod <- unique(do.call(rbind.data.frame, strsplit(ln, "_", fixed = T))[,1])
    for(y in unique_mod){
      tmpbs <- subset(x = futureClim, which(grepl(tolower(paste(y, i, sep = '_')) , tolower(names(futureClim)))))
      names(tmpbs) <- unique(do.call(rbind.data.frame, strsplit(names(tmpbs), "_", fixed = T))[,3])
      name <- paste("revegFuture", y, i, sep = "_") #strsplit(i, "\\.")
      tb[[name]] <- extract(x = tmpbs, y = revegPoints)
    }
  }
  
  futureClimRevegPoints <- data.frame(ID = do.call(rbind.data.frame, strsplit(row.names(do.call(rbind.data.frame, tb)), "\\."))[,1],
                                      do.call(rbind.data.frame, tb),
                                      row.names = NULL)
  
  ## 5. Run core of function
  ##=========================
  
  set.seed(1807)
  for(s in species){
    if(verbose){
      tkfocus(log) 
      tkinsert(log$env$txt, "end", paste("Calculating distances between revegetation site and ", s, "\n\n", sep = ""))
      tkfocus(log$env$txt)}
    
    #-- Get species data and check correct number of columns supplied
    spDat <- get(s)
    colnames(spDat)[1] <- ifelse(colnames(spDat)[1] == "Species", "species", colnames(spDat)[1])
    #spDat$species <- rep(paste("species", unique(spDat$species), sep = "_"), nrow(spDat))
    if(ncol(spDat) <3){stop("Unexpected number of columns - expected three columns 'species', 'Longitude', and 'Latitude'")}
    if(ncol(spDat[,names(spDat) %in% c("species", tolower("species"), "Latitude", "Longitude")]) < 3) stop("Unexpected column names. Please check names match the following: species, Latitude, Longitude.")
    
    #-- Extract climate data for species
    spDatxy <- spDat
    coordinates(spDatxy) <- ~Longitude+Latitude
    currClimSpecies <- extract(x = currClim, y = spDatxy, df = T)
    #colnames(currClimSpecies)[which(colnames(currClimSpecies) == "ID")] <- "species"
    currClimSpecies$ID <- rep(unique(spDat$species), nrow(currClimSpecies))
    #currClimSpecies <- cbind(spDat[, 2:3], currClimSpecies)
    
    #-- Combine site data and species data with  region data    
    pcaDat <- do.call(rbind.data.frame, list(currClimRevegPoints, 
                                             futureClimRevegPoints, 
                                             currClimSpecies, 
                                             currClimPoints))
    #pcaDat <- na.omit(pcaDat)
    currLength <- range(grep("revegCurrent", pcaDat$ID))
    futureLength <- range(grep("revegFuture", pcaDat$ID))
    spLength <- range(grep(unique(spDat$species), pcaDat$ID))
    regionLength <- range(grep("region", pcaDat$ID))
    
    ## Run PCA and assign to gloabl environment
    if(verbose){
      tkfocus(log)
      tkinsert(log$env$txt, "end", paste("...running PCA", sep = ""))
      tkfocus(log$env$txt)}
        
    pcaDset <- pcaDat[, !colnames(pcaDat) %in% "ID"]
    pca1 = PCA(pcaDset, graph = F, ind.sup = 1:spLength[2]) 
    assign(x = paste("PCA_model", s, sep = "_"), value = pca1, envir = .GlobalEnv)
    
    ## Extract data from PCA output
    regionPC <- data.frame(ID = rep("region", nrow(pca1$ind$coord)), pca1$ind$coord[, 1:length(which(pca1$eig[1] >= 1))])
    colnames(regionPC)[2:ncol(regionPC)] <- seqNames("PC", length(which(pca1$eig[1] >= 1)))
    
    revegCurrentPC <- data.frame(ID = currClimRevegPoints$ID, revegPointsDD,
                                 pca1$ind.sup$coord[currLength[1]:currLength[2], 1:length(which(pca1$eig[1] >= 1))])
    colnames(revegCurrentPC)[4:ncol(revegCurrentPC)] <- seqNames("PC", length(which(pca1$eig[1] >= 1)))
    
    revegFuturePC <- data.frame(ID = futureClimRevegPoints$ID, revegPointsDD,
                                pca1$ind.sup$coord[futureLength[1]:futureLength[2], 1:length(which(pca1$eig[1] >= 1))])
    colnames(revegFuturePC)[4:ncol(revegFuturePC)] <- seqNames("PC", length(which(pca1$eig[1] >= 1)))
    
    spCurrentPC <- data.frame(ID = currClimSpecies$ID, spDat[, 2:3],
                              pca1$ind.sup$coord[spLength[1]:spLength[2], 1:length(which(pca1$eig[1] >= 1))])
    colnames(spCurrentPC)[4:ncol(spCurrentPC)] <- seqNames("PC", length(which(pca1$eig[1] >= 1)))
    
    revegPC <- rbind(revegCurrentPC, revegFuturePC)
    
    assign("speciesDset",spCurrentPC, envir = .GlobalEnv) # <--- change here
    
    
    if(verbose){
      tkfocus(log)
      tkinsert(log$env$txt, "end", paste("   Done!", "\n\n", sep = ""))
      tkfocus(log$env$txt)}
    
    
    ## 6. Climatically match records with site
    ##=========================================
    
    ## Core of function
        
    #-- loop 1 = get the unique time slice
    for(x in unique(revegPC$ID)){
      if(verbose){
        tkfocus(log) 
        tkinsert(log$env$txt, "end", paste("...Calculating distance for ", x, sep = ""))
        tkfocus(log$env$txt)}
      #-- get time slice data
      sliceDat <- revegPC[which(revegPC$ID == x),]
      
      #-- get lists to write output to
      tmpCollect <- list()
      tmpEucDist <- list()
      tmpGeoDist <- list()
      
      #-- loop 2 = get row data for each species record
      for(y in 1:nrow(spCurrentPC)){         
        #-- loop 3 = match each point in the time slice with the species
        for(z in 1:nrow(sliceDat)){
          spP <- spCurrentPC[y, grep("PC", colnames(spCurrentPC))]
          splatlon <- spCurrentPC[y, colnames(spCurrentPC) %in% c("Latitude", "Longitude")]
                    
          poiP <- sliceDat[z, grep("PC", colnames(sliceDat))]
          poiPlatlon <- sliceDat[z, colnames(sliceDat) %in% c("Latitude", "Longitude")]
          
          eucDist <- as.numeric(dist(rbind(poiP, spP), method = "euclidean"))
          gDist = as.numeric(geoDist(ref = poiPlatlon, coords = splatlon))
          
          if(eucDist[1] <= threshold) {tmpCollect[["collect"]][y] <- 1
                                       tmpEucDist[["climDist"]][y] <- eucDist
                                       tmpGeoDist[["geoDist"]][y] <- gDist; break}
          if(z == nrow(sliceDat) & eucDist[1] > threshold){tmpCollect[["collect"]][y] <- 0
                                                           tmpEucDist[["climDist"]][y] <- eucDist
                                                           tmpGeoDist[["geoDist"]][y] <- gDist; break}
          
          #tmpCollect[["collect"]][y] <- ifelse(eucDist[1] <= threshold, 1, 0)
          #tmpEucDist[["climDist"]][y] <- eucDist
          #tmpGeoDist[["geoDist"]][y] <- gDist
        } # end of loop 3
      } # end of loop 2
      tmp1 = data.frame(Longitude = spCurrentPC$Longitude, Latitude = spCurrentPC$Latitude,
                        collect = tmpCollect, ClimDist = tmpEucDist, GeoDist = tmpGeoDist, 
                        row.names=NULL)
      assign(x = paste("output", s, x, sep = "_"), value = tmp1, envir = .GlobalEnv)
      
      
      ## 7. Print results to console
      ##=============================
      
      cat("\n==================================================================\n\n")
      cat(paste("POI: ",poi[, "Longitude"], ", " ,poi[, 'Latitude']," (",geoCode(paste(poi[, "Latitude"], poi[, "Longitude"], sep = ",")), ")",sep=""))
      cat("\n")
      cat(paste("Species: ", as.character(spDat[1, "species"]), sep = " " ))
      cat("\n")
      cat(paste("Time slice: ", x, sep = " " ))
      cat("\n")
      cat(paste("Dataset written to global environment as: ", paste("output", s, x, sep = "_") ,"\n\n", sep = ""))
      
      consoleOutput <-  georefPoints(data = tmp1, rank = "climDist", n = returnResults, printToScreen = F)
      print(consoleOutput, digits = 6)
      
      if(verbose){
        tkfocus(log)
        tkinsert(log$env$txt, "end", paste("   Done!", "\n\n", sep = ""))
        tkfocus(log$env$txt)}
    } # end of loop 1 
  } # end of species loop
  
  if(verbose){
    tkfocus(log) 
    tkinsert(log$env$txt, "end", paste("Program finished ", format(Sys.time(), "%a %b %d %X %Y"), "\n\n", sep = ""))
    tkfocus(log$env$txt)}
}
