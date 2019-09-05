z.GUI_start <- function(logoDir, start1){
  
  ## Set up GUI layout
    
  #-- Set geometry and title of GUI
  tkwm.geometry(start1, "+200+200") # position in upper left corner of screen
  tkwm.title(start1,"PUCA: Version 1.0")
  
  #-- Load logos and add to GUI
  dataDir <- logoDir
  arcLogo<-paste(dataDir,'arc.gif', sep=""); utasLogo<-paste(dataDir,'utas.gif', sep=""); gaLogo<-paste(dataDir,'ga.gif', sep="")
  
  logo1 <- tkimage.create('photo', file=arcLogo,width=0, height=0)  
  logo2 <- tkimage.create('photo', file=utasLogo,width=0, height=0)
  logo3 <- tkimage.create('photo', file=gaLogo,width=0, height=0)
  
  blk0<-tklabel(start1, text = "       ",bg='white');blk1<-tklabel(start1, text = "       ",bg='white')
  blk2<-tklabel(start1, text = "       ",bg='white');blk3<-tklabel(start1, text = "       ",bg='white')
  left<-tklabel(start1, image = logo1,bg='white');centre<-tklabel(start1, image = logo2,bg='white'); right<-tklabel(start1, image = logo3,bg='white')
  
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(blk0,left,blk1,centre,blk2,right,blk3)
  tkgrid.configure(left,sticky="e"); tkgrid.configure(right,sticky="w")
  
  #-- Set up buttons for GUI
  
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="Provenancing using climate analogues",font=fontHeading,width=0,bg='white'),columnspan=20)
  tkgrid(tklabel(start1,text="A program to identify native seed sources growing in projected future climates.",font=fontHeading2,width=0,bg='white'),columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="    ",bg='white'))
  
  aboutMessage <- function(){
    tkmessageBox(message="Provenancing using climate analogues: a program to assist in seed sourcing for the future.\n\nThis program was written by Peter A. Harrison as part of a project funded by the Australian Research Council through a linkage grant between the University of Tasmania and Greening Australia (LP120200380).\n\nPlease refer to help manual for additional information or see citation('PUCA'). Alternatively contact author: <P.A.Harrison@uats.edu.au>\n\nVersion 1.0 (February, 2016)", 
                 title="About", icon='info',type = "ok")
  }
  
  cleanUP <- function(){
    rm(list = ls(pattern = "font+", envir = .GlobalEnv), envir = .GlobalEnv)
    rm('start1', envir = .GlobalEnv)
  }
  
  fuct1Button<-tcltk::tkbutton(start1,text="Find species in my area",command=z.GUI_spInPoly,width=20,font=fontHeading2,bg='light grey')
  fuct2Button<-tcltk::tkbutton(start1,text="Get data",command=z.GUI_getData,width=20,font=fontHeading2,bg='lightgrey')
  plotButton<-tcltk::tkbutton(start1,text="Find provenances",command=z.GUI_findProv,width=20,font=fontHeading2,bg='light grey')
  aboutButton<-tcltk::tkbutton(start1,text="About",command=aboutMessage,width=20,font=fontHeading2,bg='light grey')  
  exitButton<-tcltk::tkbutton(start1,text="Exit",command=function() {tkdestroy(start1); cleanUP()},
                       width=20,font=fontHeading2,bg='light grey')
  
  tkgrid(fuct1Button,columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(fuct2Button,columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(plotButton,columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(aboutButton,columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(exitButton,columnspan=20)
  tkgrid(tklabel(start1,text="    ",bg='white'))
  tkgrid(tklabel(start1,text="    ",bg='white'))
  
  #-- Add a menu bar
  topMenu <- tcltk::tkmenu(start1)
  tkconfigure(start1, menu = topMenu) 
  fileMenu <- tcltk::tkmenu(topMenu, tearoff = FALSE)
  tcltk::tkadd(fileMenu, "command", label="Find species in my area", command= z.GUI_spInPoly)
  tcltk::tkadd(fileMenu, "command", label="Get data", command= z.GUI_getData)
  tcltk::tkadd(fileMenu, "command", label="Find provenances", command= z.GUI_findProv)
  tcltk::tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(start1))
  tcltk::tkadd(topMenu, "cascade", label = "Menu", menu = fileMenu)
  
  helpMenu <- tcltk::tkmenu(topMenu, tearoff = FALSE)
  tcltk::tkadd(helpMenu, "command", label="About",command = aboutMessage)
  tcltk::tkadd(helpMenu, "command", label="Reference manual",command = function(){})
  tcltk::tkadd(topMenu, "cascade", label = "Help", menu = helpMenu)
  
  #-- Add disclaimer message
  z.GUI_disclaimerMessage()
  
  #-- Set the focus to this window
  tcltk::tkraise(start1)
  tcltk::tkfocus(start1)
}