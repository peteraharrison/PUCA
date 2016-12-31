z.GUI_spInPoly <- function(){
  #-- Create GUI window
  findspecies<-tktoplevel(bg='white')
  tkfocus(findspecies)  
  tkwm.geometry(findspecies, "+200+0") # position in upper left corner of screen
  tkwm.minsize(findspecies, 550,500)
  tkwm.title(findspecies,"Find species in my area")
  tclRequire("BWidget")
  
  #-- add info symbol about function
  helpFunction <-function(){
    tkmessageBox(message=paste("",sep=''),icon='question')
  }
  #questionButton=tkbutton(pis,text=' ? ',command=helpFunction,bg='lightblue',width=4,font=fontHeading2)
  
  #-- Write text to window and add info button
  tkgrid(tklabel(findspecies,text="    ",bg='white'),sticky="w")
  text1 = tklabel(findspecies,text="Find species in my area",font=fontHeading1,width=0,bg='white')
  tkgrid(text1, sticky="w") #questionButton, ,columnspan=4
  tkgrid(tklabel(findspecies,text="    ",bg='white'),sticky="w")
  
  #-- Enter coordinates for search (i.e. revegetation site)
  defaultDistanceLat = tclVar(-42.008)
  defaultDistanceLon = tclVar(147.4686)
  siteEnteredLat = tkentry(findspecies, width="25", textvariable=defaultDistanceLat, bg='white', width=25)
  siteEnteredLon = tkentry(findspecies, width="25", textvariable=defaultDistanceLon, bg='white', width=25)
  distLat = tklabel(findspecies, text = "Enter latitude of a revegetation site", bg='white', font = fontHeading2a)
  distLon = tklabel(findspecies, text = "Enter longitude of a revegetation site", bg='white', font = fontHeading2a)
  tkgrid(distLat)
  tkgrid(siteEnteredLat, padx = 10, pady = c(0, 15))
  tkgrid.configure(distLat, sticky = "w")
  tkgrid.configure(siteEnteredLat, sticky = "w")
  tkgrid(distLon)
  tkgrid(siteEnteredLon, padx = 10, pady = c(0, 15))
  tkgrid.configure(distLon, sticky = "w")
  tkgrid.configure(siteEnteredLon, sticky = "w")
  
  #-- Enter search radius
  searchRadius = tclVar(5)
  radiusEntered = tkentry(findspecies, width="5", textvariable=searchRadius, bg='white', width=45)
  radiusLab = tklabel(findspecies, text = "Enter a search radius (km)", bg='white', font = fontHeading2a)
  tkgrid(radiusLab)
  tkgrid(radiusEntered, padx = 10, pady = c(0, 15))
  tkgrid.configure(radiusLab, sticky = "w")
  tkgrid.configure(radiusEntered, sticky = "w")
  
  #-- Select whether to search or plants or animals or both
  selectionList <- c("Plant", "Animal", "Both")
  comboSelect <- tk2combobox(findspecies, values = selectionList)
  #tk2list.set(comboSelect, selectionList)
  selectLab = tklabel(findspecies, text = "Select type of speices to return ", bg='white', font = fontHeading2a)
  tkgrid.configure(selectLab, sticky = "w")
  tkgrid(comboSelect, sticky = "w", padx = 10, pady = c(0, 15))
  tkgrid(tklabel(findspecies, text = "", bg='white', font = fontHeading2a))
  selectDefault <- tclVar()
  tkconfigure(comboSelect, textvariable = selectDefault)
  #comboSelect <- tclvalue(selectDefault)
  
  #-- run program to find species
  onRun <- function() {
    lats = as.numeric(tclvalue(defaultDistanceLat))
    lons = as.numeric(tclvalue(defaultDistanceLon))
    radi = as.numeric(tclvalue(searchRadius))
    comboSelect = tclvalue(selectDefault)[1]
    #print(lats)
    #print(lons)
    #print(radi)
    #print(tclvalue(selectDefault)[1])
    
    spL <- NULL
    
    out <- speciesInPolygon(x = lons, y = lats, r = radi)
    if(comboSelect == "Plant"){
      out <- out[which(out$kingdom == "Plantae"), c("species", "occurrenceCount")]
      out <- out[order(-out$occurrenceCount),]
      spL <- out$species
    }
    if(comboSelect == "Animal"){
      out <- out[which(out$kingdom == "ANIMALIA"), c("species", "occurrenceCount")]
      out <- out[order(-out$occurrenceCount),]
      spL <- out$species
    }
    if(comboSelect == "Both"){
      out <- out[order(-out$occurrenceCount),]
      spL <- out$species
    }
    
    #delList <- length(list)
    #while(delList < 1) {for(i in rev(delList)){tkdelete(list, i)} # delete list)
    #                    delList <- length(list)}
    
    for(sp in spL){tkinsert(list, "end", sp)} #re-populate list
    tkselection.set(list, 0)#2
    tkgrid(list, padx = 10, pady = c(5, 10))
    assign(x = "spL", value = spL, envir = .GlobalEnv)
  }
  #onClear <- function() {
  #delList <- 0:length(list)
  #for(i in rev(delList)){tkdelete(list, i)} # delete list
  # }
  
  tkgrid(tk2button(findspecies, text = "Find species", width = -6, command = onRun, width=10),
         sticky = "we")
  tkgrid(tklabel(findspecies, text = "", bg='white', font = fontHeading2a))
  #tkgrid(tk2button(findspecies, text = "Clear species", width = -6, command = onClear, width=10),
  #sticky = "we")
  #tkgrid(tklabel(findspecies, text = "", bg='white', font = fontHeading2a))
  
  #-- ouput window to select species and plot
  list <- tk2listbox(findspecies, height = 10, width = 90, selectmode = "extended")
  tkselection.set(list, 0)#1
  tkgrid(list, padx = 10, pady = c(5, 10), sticky = 'W')
  #tkinsert(list, "end", "Species found...")
  
  #-- plot species (optional)
  plot.but <- function(){
    selectedSpecies <- as.numeric(tkcurselection(list))+1
    spChoice <- as.character(spL[selectedSpecies])
    x = data.frame(Latitude = as.numeric(tclvalue(defaultDistanceLat)), 
                   Longitude = as.numeric(tclvalue(defaultDistanceLon)))
    coordinates(x) <- ~Longitude+Latitude
    projection(x) <- CRS("+proj=longlat +datum=WGS84")
    region <- as.character(over(x, worldMap)$admin)
    if(region == "Australia"){spDistrib <- getSpecies2(species = spChoice, client = "ALA", region = region)}
    if(region != "Australia"){spDistrib <- getSpecies2(species = spChoice, client = "GBIF", region = region)}    
  }
  
  #-- save species lat/lon (optional)
  select.but <- function(){
    selectedSpecies <- as.numeric(tkcurselection(list))+1
    spChoice <- as.character(spL[selectedSpecies])
    x = data.frame(Latitude = as.numeric(tclvalue(defaultDistanceLat)), 
                   Longitude = as.numeric(tclvalue(defaultDistanceLon)))
    coordinates(x) <- ~Longitude+Latitude
    projection(x) <- CRS("+proj=longlat +datum=WGS84")
    region <- as.character(over(x, worldMap)$admin)
    if(region == "Australia"){spDistrib <- getSpecies2(species = spChoice, client = "ALA", region = region, plot = F)}
    if(region != "Australia"){spDistrib <- getSpecies2(species = spChoice, client = "GBIF", region = region, plot = F)}    
    assign(x = paste("species",strsplit(spChoice, " ")[[1]][1], strsplit(spChoice, " ")[[1]][2], sep = "_"), 
           value = spDistrib, envir = .GlobalEnv)
    finished()
  }
  
  finished <- function() {tkdestroy(findspecies); tkraise(start1); rm(spL, envir = .GlobalEnv)}
  tkgrid(tk2button(findspecies, text = "Plot distribution", width = -6, command = plot.but, width=10),
         sticky = "we", pady = c(2, 2))
  tkgrid(tk2button(findspecies, text = "Select species and return", width = -6, command = select.but, width=10),
         sticky = "we", pady = c(2, 2))
  tkgrid(tk2button(findspecies, text = "Quit", width = -6, command = finished, width=10),
         sticky = "we", pady = c(2, 2))
}