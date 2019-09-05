#' @importFrom maptools readShapeSpatial
#' @import raster
#' 
z.GUI_findProv <- function(){
  ## set up main screen of GUI
  ##===========================
  
  prov <- tktoplevel(bg='white')
  tkfocus(prov)  
  tkwm.geometry(prov, "+200+0") # position in upper left corner of screen
  tkwm.minsize(prov, 550,550)
  tkwm.title(prov,"Find provenances")
  tclRequire("BWidget")
  
  #-- Create tabs
  nb <- tk2notebook(prov, tabs = c("Info", "Main", "Parameters"))
  tkpack(nb, fill = "both", expand = TRUE)
  
  ## Populate tab1 - Information tab
  ##=================================
  
  tb1 <- tk2notetab(nb, "Info")
  tb1_lab <- tk2label(tb1, text = "\n\nFind provenances - information", font=fontHeading1,width=0)
  tkpack(tb1_lab, side = "top")
  infoText <- paste(
    "\n  There are two tabs above:
    
    (1) The 'Main' tab contains all the data that was imported using the
    'Get data' program. If any data is missing then re-run the data importer. The only 
    two bits of information that needs to be set in this window is the latitude/longitude 
    coordinates of the revegetation site(s) and an output directory for the results.
    
    (2) The 'Parameters' tab contains the various modelling parameters that can be set. 
    The values shown are the default." )
  tb1_infoText <- tk2label(tb1, text = infoText, font = fontHeading2a)
  tkpack(tb1_infoText, side = "left", expand = TRUE, anchor = 'w', padx = 10)
  
  
  ## Populate tab2 - get names of imported data
  ##============================================
  
  tb2 <- tk2notetab(nb, "Main")
  tb2_lab <- tk2label(tb2, text = "\nFind provenances - main window\n", font=fontHeading1,width=0)
  tkgrid(tb2_lab, sticky = "w")
  
  #-- Get coordinates of revegetation site
  defaultDistanceLat = tclVar()
  defaultDistanceLon = tclVar()
  siteEnteredLat = tkentry(tb2, width="25", textvariable=defaultDistanceLat, bg='white', width=25)
  siteEnteredLon = tkentry(tb2, width="25", textvariable=defaultDistanceLon, bg='white', width=25)
  distLat = tklabel(tb2, text = "Enter latitude of a revegetation site", font = fontHeading2a)
  distLon = tklabel(tb2, text = "Enter longitude of a revegetation site", font = fontHeading2a)
  tkgrid(distLat,  sticky = "w")#, padx = 5)
  tkgrid(siteEnteredLat,  sticky = "w", padx = 10)
  tkgrid(distLon, sticky = "w")#, padx = 5)
  tkgrid(siteEnteredLon, sticky = "w", padx = 10)
  #tkgrid(tklabel(tb2, text = "", font = fontHeading2a))
  
  #-- Get species
  myL <- list()
  myL2 <- list()
  for(s in ls(pattern = "*species_+", envir = .GlobalEnv)){
    myL[[s]] <- as.character(get(s, envir = .GlobalEnv)[1,"species"])
    myL2[[s]] <- s
  }
  myL <- do.call(rbind.data.frame, myL)
  if(nrow(myL) < 1){selectionList <- paste("No species selected. Please select a species using \n(1) Find species in my area, or \n(2) Get data.")
  } else{colnames(myL) <- "species"; selectionList <- c(as.character(myL$species))}
  speciesLab = tklabel(tb2, text = "\nSelect which species to use. (Select multiple by holding Ctrl)", font = fontHeading2a)
  tkgrid.configure(speciesLab, sticky = "w")
  speciesList2 <- tk2listbox(tb2, height = 5, width = 60, selectmode = "extended")
  tkselection.set(speciesList2, 0)#1
  tkgrid(speciesList2, padx = 10, pady = c(5, 10), sticky = 'W', column = 0)
  for(s in selectionList){tkinsert(speciesList2, "end", s)}
  
  #-- Get current layers
  if(length(ls(pattern = "climate_current", envir = .GlobalEnv)) < 1){layerList <- paste("No climate layers found. \nPlease use Get data to either download or import contemporary climate data.")
  } else{layerList <- names(get(ls(pattern = "climate_current", envir = .GlobalEnv)))}
  
  layerLab = tklabel(tb2, text = "Contemporary layers", font = fontHeading2a)
  tkgrid.configure(layerLab, sticky = "w")
  layerList2 <- tk2listbox(tb2, height = 5, width = 60, selectmode = "browse")
  tkselection.set(layerList2, 0)#1
  tkgrid(layerList2, padx = 10, pady = c(5, 10), sticky = 'W', column = 0)
  for(l in layerList){tkinsert(layerList2, "end", l)}
  
  #-- Get future layers
  mods <- unique(do.call(rbind.data.frame, strsplit(names(climate_future), "_"))[,1])
  
  if(length(ls(pattern = "climate_future", envir = .GlobalEnv)) < 1){futureList <- paste("No future climate layers found. \nPlease use Get data to either download or import future climate data.")
  } else{if(length(ls(pattern = "climate_future", envir = .GlobalEnv)) > 0){futureList <- as.character(mods)}}
  
  futureLab = tklabel(tb2, text = "Select which future projection layers to use. (Select multiple by holding Ctrl)", font = fontHeading2a)
  tkgrid.configure(futureLab, sticky = "w")
  futureList2 <- tk2listbox(tb2, height = 5, width = 60, selectmode = "extended")
  tkselection.set(futureList2, 0)#1
  tkgrid(futureList2, padx = 10, pady = c(5, 10), sticky = 'W', column = 0)
  for(l in futureList){tkinsert(futureList2, "end", l)}
  
  tkgrid(tklabel(tb2, text = "", font = fontHeading2a))
  
  
  ## Populate tab3 - set parameters
  ##================================
  
  tb3 <- tk2notetab(nb, "Parameters")
  tb3_lab <- tk2label(tb3, text = "\nFind provenances - parameters\n", font=fontHeading1,width=0)
  tkgrid(tb3_lab, sticky = "w")
  
  #- Set regional size
  defaultRegional = tclVar(30)
  regionEntered = tkentry(tb3, width="25", textvariable=defaultRegional, bg='white', width=62)
  tkgrid(tklabel(tb3,text="\nSelect directory with regional map or enter in a buffer distance (km)",
                 font=fontNormal,width=0), sticky = 'W')
  tkgrid(regionEntered, sticky = "w", padx = 5)
  
  onBrowse2 <- function() {
    fileFilters <- matrix(c("Shapefile", ".shp"), 1, 2, byrow = T)
    files <- tk_choose.files(filters = fileFilters, multi = F)
    ext <- strsplit(as.character(splitMyDir(files)[[1]]), "\\.")[[1]][2]
    if(any(ext != "shp")){stop("File type not supported. Plase use files with extension .shp.")}
    if(any(ext == "shp")){regionChoice <- readShapeSpatial(files)
                          projection(regionChoice) <- CRS("+proj=longlat +datum=WGS84")
                          defaultRegional <- files
                          tkinsert(regionEntered, "end", files)}
    assign(x = "regionChoice", value = regionChoice, envir = .GlobalEnv)
  }
  #tkgrid(tklabel(tb3,text= " ",font=fontNormal,width=0), sticky = 'W')
  tkgrid(tk2button(tb3, text = "Browse", command = onBrowse2, width=62), sticky = 'W')
  #tkgrid(tklabel(tb3,text="\n",font=fontNormal,width=0), sticky = 'W')
  
  #- Buffer size
  bufferSize = tclVar(5)
  bufferEntered = tkentry(tb3, width="25", textvariable=bufferSize, bg='white', width=62)
  tkgrid(tklabel(tb3,text="\nEnter the buffer size around revegetation site (km)",
                 font=fontNormal,width=0), sticky = 'W')
  tkgrid(bufferEntered, sticky = "w", padx = 5)
  
  #- Binary transformation threshold
  threshold = tclVar(2)
  thresholdEntered = tkentry(tb3, width="25", textvariable=threshold, bg='white', width=62)
  tkgrid(tklabel(tb3,text="\nEnter the binary transformation threshold",
                 font=fontNormal,width=0), sticky = 'W')
  tkgrid(thresholdEntered, sticky = "w", padx = 5)
  
  #-- Return to screen top x results
  topResults = tclVar(20)
  topResultsEntered = tkentry(tb3, width="25", textvariable=topResults, bg='white', width=62)
  tkgrid(tklabel(tb3,text="\nEnter the number of top results to print to screen for each time slice",
                 font=fontNormal,width=0), sticky = 'W')
  tkgrid(topResultsEntered, sticky = "w", padx = 5)
  
  #- Verbose
  verboseOut <- c('TRUE', "FALSE")
  verboseLab = tklabel(tb3, text = "Show progress?", font = fontHeading2a)
  verboseSelect <- tk2combobox(tb3, values = as.tclObj(verboseOut))
  verboseDefault <- tclVar('TRUE')
  tkconfigure(verboseSelect, textvariable = verboseDefault)
  tkgrid(verboseLab, sticky = "w", padx = 0, pady = c(5, 0))
  tkgrid(verboseSelect, sticky = "w", padx = 5)#, pady = c(0, 0))
  tkgrid.configure(verboseLab, verboseSelect, sticky = "w", column = 0)
  
  
  ## Run Find provenances
  ##======================
  
  onRun <- function(){
    
    lats <- tclvalue(defaultDistanceLat)
    lons <- tclvalue(defaultDistanceLon)
    
    tmpLats <- strsplit(lats, split = ",")
    lats <- lapply(tmpLats, as.numeric)
    tmpLons <- strsplit(lons, split = ",")
    lons <- lapply(tmpLons, as.numeric)
    
    coords <- data.frame(Latitude = lats,
                        Longitude = lons,
                        row.names = NULL)
    colnames(coords) <- c("Latitude", "Longitude")
    assign('coords', coords, envir = .GlobalEnv)
    
    selectedSpecies <- as.numeric(tkcurselection(speciesList2))+1
    speciesToUse <- row.names(myL)[c(selectedSpecies)] #row.names(myL[which(myL[,c(selectedSpecies)] == myL[c(selectedSpecies),])])
    assign('speciesToUse', speciesToUse, envir = .GlobalEnv)
    
    futureMods <- as.numeric(tkcurselection(futureList2))+1
    futureMods <- mods[futureMods]
    assign('futureMods', futureMods, envir = .GlobalEnv)
    
    region <- as.character(tclvalue(defaultRegional))
    region <- splitMyDir(region)
    if(length(region) == 1) {regionChoice <- as.numeric(region)}
    if(length(region) > 1) {regionChoice <- regionChoice}
    assign('regionChoice', regionChoice, envir = .GlobalEnv)
    
    bufferRadius <- as.numeric(tclvalue(bufferSize))
    assign('bufferRadius', bufferRadius, envir = .GlobalEnv)
    
    binThreshold <- as.numeric(tclvalue(threshold))
    assign('binThreshold', binThreshold, envir = .GlobalEnv)
    
    topResults <- as.numeric(tclvalue(topResults))
    assign('topResults', topResults, envir = .GlobalEnv)
    
    verboseLog <- as.character(tclvalue(verboseDefault)[1])
    verboseLog <- ifelse(verboseLog == "TRUE", TRUE, FALSE)
    assign('verboseLog', verboseLog, envir = .GlobalEnv)
    
    futureClim <- stack()
    for(m in mods){
      tmpr <- subset(climate_future, c(grep(m, names(climate_future))))
      futureClim <- addLayer(futureClim, tmpr)
    }
    
    seedSource(poi = coords, 
               species = speciesToUse, 
               region = regionChoice, 
               currClim = climate_current,
               futureClim = futureClim,
               threshold = binThreshold, 
               radius = bufferRadius, 
               returnResults = topResults,
               verbose = verboseLog)
    
  }
  tkgrid(tk2button(tb2, text = "Run", command = onRun, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  tkgrid(tklabel(tb2, text = "", font = fontHeading2a))
  
}