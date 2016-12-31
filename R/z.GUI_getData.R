#' @import tcltk 
#' @importFrom tcltk2 tk2combobox tk2button tk2notebook tk2notetab tk2label tk2listbox
#' 
z.GUI_getData <- function(){
  ## set up main screen of GUI
  ##===========================
  
  gd<-tktoplevel(bg='white')
  tkfocus(gd)  
  tkwm.geometry(gd, "+200+0") # position in upper left corner of screen
  tkwm.minsize(gd, 550,500)
  tkwm.title(gd,"Import data")
  tclRequire("BWidget")
  
  #-- Create tabs
  nb <- tk2notebook(gd, tabs = c("Info", "I have my own data", "I need to download data"))
  tkpack(nb, fill = "both", expand = TRUE)
  
  ## Populate tab1 - Information tab
  ##=================================
  
  tb1 <- tk2notetab(nb, "Info")
  tb1_lab <- tk2label(tb1, text = "\n\nImport data", font=fontHeading1,width=0)
  tkpack(tb1_lab, side = "top")
  infoText <- paste(
    "\nThere are two ways data can be imported:\n
    1. Import your own data. This can be done using the 'I have my own data' tab above
    and requires the user to supply the directory for each requested file. It is important that
    ALL climate layers have the same name. Currently, there is only support for layers 
    with extensions .asc or .tif or .bil.\n
    2. Download data. This option requires internet access and can be done using the 
    'I need to downlaod data tab'.
    \nBoth of these options can be used, for example, to import your own distribution 
    data of a species but download climate layers. There is also an option to download 
    these files to a directory, however, they can also be downloaded to a temp folder 
    which will be deleted when R exits.")
  tb1_infoText <- tk2label(tb1, text = infoText, font = fontHeading2a)
  tkpack(tb1_infoText, side = "left", expand = TRUE, anchor = 'w', padx = 10)
  
  
  ## Populate tab2 - Import user data
  ##==================================
  
  tb2 <- tk2notetab(nb, "I have my own data")
  tb2_lab <- tk2label(tb2, text = "\nImport your own data", font=fontHeading1,width=0)
  tkgrid(tb2_lab, sticky = "w")
  
  #-- Get species directory
  tkgrid(tklabel(tb2,text="\nSelect directory with species data.",
                 font=fontNormal,width=0), sticky = 'W')
  spDir <- tk2listbox(tb2, height = 0, width = 60, selectmode = "extended")
  tkselection.set(spDir, 2)
  tkgrid(spDir, padx = 5, sticky = 'W') #pady = c(5, 10), 
  
  onBrowse1 <- function() {
    fileFilters <- matrix(c("CSV (Comma delimited)", ".csv"), 1, 2, byrow = T)
    files <- tk_choose.files(filters = fileFilters)
    for (file in files){
      tkinsert(spDir, "end", file)
      speciesData <- read.csv(file)
      speciesName <- strsplit(as.character(splitMyDir(file)[[1]]), "\\.")[[1]][1]
      assign(x = paste("species", speciesName, sep = "_"), 
             value = speciesData, envir = .GlobalEnv)
    }
    tkselection.set(spDir, 2)
    tkgrid(spDir, padx = 10, pady = c(5, 10))
  }
  tkgrid(spDir, tk2button(gd, text = "Browse", command = onBrowse1, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  
  #-- Get directory for contemporary climate data
  tkgrid(tklabel(tb2,text="\nSelect directory with contemporary climate data.",
                 font=fontNormal,width=0), sticky = 'W')
  contempClimDir <- tk2listbox(tb2, height = 0, width = 60, selectmode = "browse")
  tkselection.set(contempClimDir, 2)
  tkgrid(contempClimDir, padx = 5, sticky = 'W') #pady = c(5, 10), 
  
  onBrowse2 <- function() {
    #fileFilters <- matrix(c("ASCII", ".asc", "GeoTIFF", ".tif", "BIL", ".bil"), 3, 2, byrow = T)
    files <- tk_choose.dir()
    b = list()
    for (file in files){
      tkinsert(contempClimDir, "end", file)
      name = as.character(splitMyDir(file)[[1]])
      name = gsub(pattern = "\\s", replacement = "", x = name) #removes spaces from name
      bioDir = file
      b[[name]] <- list.files(path = bioDir, pattern = "\\.asc$", full.names=T)
      ext <- strsplit(as.character(splitMyDir(b[[1]][1])[[1]]), "\\.")[[1]][2]
      if(!any(ext != "asc" | ext != "tif" | ext != "bil")){stop("File type not supported. Plase use files with extension .asc, .tif, or .bil. Alternatively see raster::writeRaster.")}
    }
    tkselection.set(contempClimDir, 2)
    tkgrid(contempClimDir, padx = 10, pady = c(5, 10))
    bs <- stack(b[[1]])
    name <- seqNames("bio", nlayers(bs))
    names(bs) <- name
    projection(bs) <- CRS("+proj=longlat +datum=WGS84")
    assign(x = "climate_current", value = bs, envir = .GlobalEnv)
  }
  tkgrid(contempClimDir, tk2button(gd, text = "Browse", command = onBrowse2, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  
  #-- Get directory for future climate data
  tkgrid(tklabel(tb2,text="\nSelect directory(s) with future climate data.",
                 font=fontNormal,width=0), sticky = 'W')
  futureClimDir <- tk2listbox(tb2, height = 0, width = 60, selectmode = "extended")
  tkselection.set(futureClimDir, 2)
  tkgrid(futureClimDir, padx = 5, sticky = 'W') #pady = c(5, 10), 
  
  onBrowse3 <- function() {
    files <- tk_choose.dir()
    b = list()
    for (file in files){
      tkinsert(futureClimDir, "end", file)
      name = as.character(splitMyDir(file)[[1]])
      name = gsub(pattern = "\\s", replacement = "", x = name) #removes spaces from name
      bioDir = file
      b[[name]] <- list.files(path = bioDir, pattern = "\\.asc$", full.names=T)
      ext <- strsplit(as.character(splitMyDir(b[[1]][1])[[1]]), "\\.")[[1]][2]
      if(!any(ext != "asc" | ext != "tif" | ext != "bil")){stop("File type not supported. Plase use files with extension .asc, .tif, or .bil. Alternatively see raster::writeRaster.")}
    }
    tkselection.set(futureClimDir, 2)
    tkgrid(futureClimDir, padx = 10, pady = c(5, 10))
    if(exists("climate_future", envir = .GlobalEnv)){bs <- climate_future} else{bs <- stack()}
    for(r in 1:length(b)){
      bsT <- stack(b[[r]])
      name <- seqNames(paste(names(b[r]), "_bio"), nlayers(bsT))
      names(bsT) <- name
      bs <- addLayer(x = bs, bsT)
    }
    projection(bs) <- CRS("+proj=longlat +datum=WGS84")
    #assign(x = "dir_future", value = files, envir = .GlobalEnv)
    assign(x = "climate_future", value = bs, envir = .GlobalEnv)
  }
  tkgrid(futureClimDir, tk2button(gd, text = "Browse", command = onBrowse3, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  
  #-- Get directory for vegetation map
#   tkgrid(tklabel(tb2,text="\nSelect directory with vegetation map. (Optional)",
#                  font=fontNormal,width=0), sticky = 'W')
#   vegMap <- tk2listbox(tb2, height = 0, width = 60, selectmode = "browse")
#   tkselection.set(vegMap, 2)
#   tkgrid(vegMap, padx = 5, sticky = 'W') #pady = c(5, 10), 
#   
#   onBrowse4 <- function() {
#     fileFilters <- matrix(c("ASCII", ".asc", "GeoTIFF", ".tif", "BIL", ".bil",
#                             "Shapefile", ".shp"), 4, 2, byrow = T)
#     files <- tk_choose.files(filters = fileFilters, multi = F)
#     for (file in files){
#       tkinsert(vegMap, "end", file)
#     }
#     tkselection.set(vegMap, 2)
#     tkgrid(vegMap, padx = 10, pady = c(5, 10))
#     ext <- strsplit(as.character(splitMyDir(files)[[1]]), "\\.")[[1]][2]
#     if(!any(ext != "asc" | ext != "tif" | ext != "bil" | ext != "shp")){stop("File type not supported. Plase use files with extension .asc, .tif, .bil, or .shp.")}
#     if(any(ext != "asc" | ext != "tif" | ext != "bil")){vegetationMap <- raster(files)}
#     if(any(ext != "shp")){vegetationMap <- readShapeSpatial(files)}
#     assign(x = "vegetation_map", value = vegetationMap, envir = .GlobalEnv)
#   }
#   tkgrid(vegMap, tk2button(gd, text = "Browse", command = onBrowse4, width=62))#width = -6, sticky = "w", pady = c(2, 2)
#   tkgrid(tklabel(tb2,text="\n",font=fontNormal,width=0), sticky = 'W')
  
  finished <- function() {tkdestroy(gd); tkraise(start1)}
  tkgrid(tk2button(tb2, text = "Finished", width = 62, command = finished))
  
  ## Populate tab3 - Download data
  ##===============================
  
  tb3 <- tk2notetab(nb, "I need to download data")
  tb3_lab <- tk2label(tb3, text = "Download data", font=fontHeading1,width=0)
  tkgrid(tb3_lab, sticky = "w")
  
  #-- coordinates of revegetation site
  defaultDistanceLat = tclVar()
  defaultDistanceLon = tclVar()
  siteEnteredLat = tkentry(tb3, width="25", textvariable=defaultDistanceLat, bg='white', width=25)
  siteEnteredLon = tkentry(tb3, width="25", textvariable=defaultDistanceLon, bg='white', width=25)
  distLat = tklabel(tb3, text = "Enter latitude of a revegetation site", font = fontHeading2a)
  distLon = tklabel(tb3, text = "Enter longitude of a revegetation site", font = fontHeading2a)
  tkgrid(distLat,  sticky = "w")#, padx = 10)
  tkgrid(siteEnteredLat,  sticky = "w", padx = 10)
  tkgrid(distLon, sticky = "w")#, padx = 10)
  tkgrid(siteEnteredLon, sticky = "w", padx = 10)
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  
  #-- Species
  if(length(ls(pattern = "species_+", envir = .GlobalEnv)) < 1){
    tkgrid(tklabel(tb3,text="Find a species",
                   font=fontNormal,width=0), sticky = 'W')
    getSpecies <- function() {
      spInPoly()
    }
    tkgrid(tk2button(tb3, text = "Find species", command = getSpecies, width=62))#width = -6, sticky = "w", pady = c(2, 2)
    #tkgrid(tklabel(tb3,text="\n",font=fontNormal,width=0), sticky = 'W')
  }
  
  
  #myL <- list()
  #for(s in ls(pattern = "species_+", envir = .GlobalEnv)){
  #  myL[[s]] <- get(s, envir = .GlobalEnv)[1,"species"]
  #}
  #myL <- do.call(rbind.data.frame, myL)
  #colnames(myL) <- "species"
  #selectionList <- c(as.character(myL[,1]), as.character(paste("Find new species")))
  
  #comboSelect <- tk2combobox(tb3, values = as.tclObj(selectionList))
  #selectLab = tklabel(tb3, text = "Select type of speices to return ", font = fontHeading2a)
  #tkgrid.configure(selectLab, sticky = "w")
  #tkgrid(comboSelect, sticky = "w", padx = 10, pady = c(5, 10))#, padx = 10, pady = c(0, 15))
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  #selectDefault <- tclVar()
  #tkconfigure(comboSelect, textvariable = selectDefault)
  
  
  #-- Select climate data
  availableGCM <- data.frame(gcm = c('ACCESS1-0*', 'BCC-CSM1-1', 'CCSM4', 'CESM1-CAM5-1-FV2', 'CNRM-CM5*',
                                     'GFDL-CM3', 'GFDL-ESM2G', 'GISS-E2-R', 'HadGEM2-AO', 'HadGEM2-CC', 
                                     'HadGEM2-ES', 'INMCM4', 'IPSL-CM5A-LR', 'MIROC-ESM-CHEM*',
                                     'MIROC-ESM*', 'MIROC5*', 'MPI-ESM-LR', 'MRI-CGCM3', 'NorESM1-M'),
                             code = c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG',
                                      'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  )
  
  gcmList <- c(as.character(availableGCM$gcm))
  gcmLab = tklabel(tb3, text = "Select which Global Circulation Models to download.", font = fontHeading2a)
  tkgrid.configure(gcmLab, sticky = "w")
  gcmList2 <- tk2listbox(tb3, height = 5, width = 60, selectmode = "extended")
  tkselection.set(gcmList2, 0)#1
  tkgrid(gcmList2, padx = 10, pady = c(5, 10), sticky = 'W', column = 0)
  for (g in gcmList){tkinsert(gcmList2, "end", g)}
  
  #-- climate data parameters
  #---- resoultion
  resLab = tklabel(tb3, text = "Select resolution of climate layers.", font = fontHeading2a)
  #tkgrid.configure(resLab, sticky = "w")
  res <- c("2.5 arc min", "5 arc min", "10 arc min")#"0.5 arc sec",
  resSelect <- tk2combobox(tb3, values = as.tclObj(res))
  #tkgrid(resSelect, sticky = "w", padx = 10, pady = c(5, 0))#, padx = 10, pady = c(0, 15))
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  resDefault <- tclVar()
  tkconfigure(resSelect, textvariable = resDefault)
  
  tkgrid(resLab, sticky = "w", padx = 0, pady = c(5, 0))
  tkgrid(resSelect, sticky = "w", padx = 10, pady = c(5, 0))
  tkgrid.configure(resLab, resSelect, sticky = "w", column = 0)
  
  #---- variables
  varLab = tklabel(tb3, text = "Select climate layer variables.", font = fontHeading2a)
  #tkgrid.configure(varLab, sticky = "w")
  vars <- c("Min temperature", "Max temperature", "Precipitation", "Bioclim")
  varSelect <- tk2combobox(tb3, values = as.tclObj(vars))
  #tkgrid(varSelect, sticky = "w", padx = 10, pady = c(5, 0))#, padx = 10, pady = c(0, 15))
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  varDefault <- tclVar()
  tkconfigure(varSelect, textvariable = varDefault)
  
  tkgrid(varLab, sticky = "w", padx = 0, pady = c(5, 0))
  tkgrid(varSelect, sticky = "w", padx = 10, pady = c(5, 0))
  tkgrid.configure(varLab, varSelect, sticky = "w", column = 0)
  
  #---- years
  availableYears <- c('contemporary', "2050", "2070")
  yearList <- c(as.character(availableYears))
  yearLab = tklabel(tb3, text = "Select the projected time period.", font = fontHeading2a)
  tkgrid.configure(yearLab, sticky = "w")
  yearList2 <- tk2listbox(tb3, height = 1, width = 60, selectmode = "extended")
  tkselection.set(yearList2, 0)#1
  tkgrid(yearList2, padx = 10, pady = c(5, 10), sticky = 'W', column = 0)
  for (g in yearList){tkinsert(yearList2, "end", g)}
  
  
  #yearLab = tklabel(tb3, text = "Select the projected time period.", font = fontHeading2a)
  #years <- c("1950-2000", "2041-2060", "2061-2080")
  #yearSelect <- tk2combobox(tb3, values = as.tclObj(years))
  #yearDefault <- tclVar()
  #tkconfigure(yearSelect, textvariable = yearDefault)
  #tkgrid(yearLab, sticky = "w", padx = 0, pady = c(5, 0))
  #tkgrid(yearSelect, sticky = "w", padx = 10, pady = c(5, 0))
  #tkgrid.configure(yearLab, yearSelect, sticky = "w", column = 0)
  
  #---- emission
  rcpLab = tklabel(tb3, text = "Select the emission scenario (RCP).", font = fontHeading2a)
  #tkgrid.configure(rcpLab, sticky = "w")
  rcps <- c("45", "85")
  rcpSelect <- tk2combobox(tb3, values = as.tclObj(rcps))
  #tkgrid(rcpSelect, sticky = "w", padx = 10, pady = c(5, 0))#, padx = 10, pady = c(0, 15))
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  rcpDefault <- tclVar()
  tkconfigure(rcpSelect, textvariable = rcpDefault)
  
  tkgrid(rcpLab, sticky = "w", padx = 0, pady = c(5, 0))
  tkgrid(rcpSelect, sticky = "w", padx = 10, pady = c(0, 0))
  tkgrid.configure(rcpLab, rcpSelect, sticky = "w", column = 0)
  #tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  
  #-- Directory to save data (optional) - will need to create directors etc
  tkgrid(tklabel(tb3,text="Select directory to save files. (Optional)",
                 font=fontNormal,width=0), sticky = 'W')
  outDire <- tk2listbox(tb3, height = 0, width = 60, selectmode = "browse")
  tkselection.set(outDire, 2)
  tkgrid(outDire, padx = 5, sticky = 'W') #pady = c(5, 10), 
  
  onBrowse5 <- function() {
    files <- tk_choose.dir()
    for (file in files){
      tkinsert(outDire, "end", file)
    }
    tkselection.set(outDire, 2)
    tkgrid(outDire)#, padx = 10, pady = c(5, 10))
    assign(x = "output_directory", value = outDire, envir = .GlobalEnv)
  }
  tkgrid(outDire, tk2button(tb3, text = "Browse", command = onBrowse5, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  tkgrid(tklabel(tb3, text = "", font = fontHeading2a))
  
  onGet <- function(){
    #- get lat/lon
    x = data.frame(lat = as.numeric(tclvalue(defaultDistanceLat)), 
                   long = as.numeric(tclvalue(defaultDistanceLon)))  
    #- get climate data
    selectedGCM <- as.numeric(tkcurselection(gcmList2))+1
    gcms <- availableGCM[selectedGCM,]
    #- get parameters
    resolution <- as.character(tclvalue(resDefault)[1])
    variable <- as.character(tclvalue(varDefault)[1])
    years <- as.numeric(tkcurselection(yearList2))+1
    years <- availableYears[years]
    rcp <- as.integer(tclvalue(rcpDefault)[1])
    
    #Downlaod climate data
    if(length(ls(pattern = 'output_directory', envir = .GlobalEnv)) < 1){
      #- run raster::getData
      toDownload <- T
      dirDownload <- Sys.getenv("TMP")
      gcmCodes <- gcms$code
      resolution <- strsplit(resolution, "\\s")[[1]][1]
      variable <- ifelse(variable == "Min temperature", 'tmin', 
                         ifelse(variable == "Max temperature", 'tmax', 
                                ifelse(variable == "Precipitation", 'prec', 'bio')))
      
      for(y in years){
        y2 <- ifelse(y == "contemporary", "contemporary", 
                        ifelse(y == "2050", 50, 70))  
        
        if(y2 == "contemporary"){
          dir.create(paste(dirDownload, "\\current", sep = ""))
          contClim <- getData(name = 'worldclim', 
                              download = toDownload,
                              path = paste(dirDownload, "\\current\\", sep = ""),
                              var = variable,
                              res = resolution)
          assign('climate_current', contClim, envir = .GlobalEnv)
          
        }
        if(y2 != "contemporary"){
          for(m in gcmCodes){
            dir.create(paste(dirDownload, "\\", m, "_", y, sep = ""))
            futClim <- getData(name = 'CMIP5', 
                               download = toDownload,
                               path = paste(dirDownload, "\\", m, "_", y, "\\", sep = ""),
                               var = variable,
                               res = resolution,
                               rcp = rcp,
                               model = m,
                               year = y2)
            names(futClim) <- seqNames(paste(m, y2, "bio", sep = "_"), 19)
            assign(paste("climate_future", m, y2, sep = "_"), futClim, envir = .GlobalEnv)
          }
        }
      }
      climate_future <- stack(lapply(ls(pattern = "climate_future+", envir = .GlobalEnv), 
                                     FUN = function(x){get(x, envir = .GlobalEnv)}))
      assign("climate_future", climate_future, envir = .GlobalEnv)
    }
    if(length(ls(pattern = 'output_directory', envir = .GlobalEnv)) > 0){
      #- run raster::getData
      toDownload <- T
      gcmCodes <- gcms$code
      resolution <- strsplit(resolution, "\\s")[[1]][1]
      variable <- ifelse(variable == "Min temperature", 'tmin', 
                         ifelse(variable == "Max temperature", 'tmax', 
                                ifelse(variable == "Precipitation", 'prec', 'bio')))
      years <- ifelse(years == "contemporary", "contemporary", 
                      ifelse(years == "2050", 50, 70))
            
      for(y in years){
        if(years == "contemporary"){
          dir.create(paste(output_directory, "\\current", sep = ""))
          contClim <- getData(name = 'worldclim', 
                              download = toDownload,
                              path = paste(output_directory, "\\current", sep = ""),
                              var = variable,
                              res = resolution)
          names(contClim) <- seqNames(bio, 19)
          assign(paste("climate_current", y, m, sep = "_"), futClim, envir = .GlobalEnv)
        }
        if(years != "contemporary"){
          for(m in gcmCodes){
            dir.create(paste(output_directory, "\\future\\",m, "_", y,sep = ""))
            futClim <- getData(name = 'CMIP5', 
                               download = toDownload,
                               path = paste(output_directory, "\\future\\",m, "_", y, sep = ""),
                               var = variable,
                               res = resolution,
                               rcp = rcp,
                               model = m,
                               year = y)
            names(futClim) <- seqNames(bio, 19)
            assign(paste("climate_future", y, m, sep = "_"), futClim, envir = .GlobalEnv)
          }
        }
      }
    }
    tkdestroy(gd); tkraise(start1)
  
  }
  tkgrid(tk2button(tb3, text = "Download data", command = onGet, width=62))#width = -6, sticky = "w", pady = c(2, 2)
  #tkgrid(tklabel(tb3,text="\n",font=fontNormal,width=0), sticky = 'W')
  
}