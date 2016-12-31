
<!-- README.md is generated from README.Rmd. Please edit that file -->
General introduction
--------------------

This is a beta version of the Provenancing Using Climate Analogues (PUCA) that integrates the principals of the climate-adjusted provenancing strategy with concepts from population genetics to guide seed sourcing for ecological restoration and reforestation. A detailed description of this package is published in Restoration Ecology (doi: 10.1111/rec.12488), and outlines the generalized proceedure of PUCA along with providing two examples demonstating the use of PUCA. This package can be used as a research tool to guide provenance choice to test the assumptions of PUCA and verious provenancing techniques, in addition to being operational-ready for restoration ecologists to guide provenance choice for current and future climate scenarios. Please cite this package following `citation("PUCA")`.

*As this is a beta version, there will inevitably be a few minor errors. While these errors will not affect the underlying procedure of PUCA, if errors are detected, please report them to the author at <P.A.Harrison@utas.edu.au>. This package has been tested on both a Windows and Mac operating system.*

Download instructions
---------------------

The PUCA package makes use of several R packages, including FactoMineR (Le, Josse & Husson 2008), maptools (Bivand and Lewin-Koh 2015), mefa (Solymos 2009), raster (Hijmans 2015), RJSONIO (Lang 2014), sp (Pebesma and Bivand 2005; Bivand, Pebesma & Gomez-Rubio 2013), tcltk (R Core Team 2014), and tcltk2 (Grosjean 2015). These packages (and associated dependencies) are automatically downloaded when PUCA is first installed. However, prior to PUCA being installed, there are three separate packages that must be present. These include devtools (Wickham and Chang 2015), ALA4R (Raymond, VanDerWal & Belbin 2015), and rgbif (Chamberlain *et al.* 2015).

The first package to install is devtools. This package facilities the download of ALA4R, rgbif, and PUCA from the GitHub repository using the install\_github function. The following code chunk will install the devtools package, all of its dependencies and then load it into the global workspace. The subsequent code will then download ALA4R, rgbif and PUCA from GitHub, along with their dependent packages. The last line calls the PUCA library. (Note: this process requires an internet connection and may take a few minutes to run as there are a number of dependent packages that will be downloaded (if not already in the R library folder)).

``` r
install.packages("devtools", dependencies = TRUE)
library(devtools)
devtools::install_github("AtlasOfLivingAustralia/ALA4R", dependencies = TRUE)
devtools::install_github("ropensci/rgbif", dependencies = TRUE)
devtools::install_github("peteraharrison/PUCA", dependencies = TRUE)
library(PUCA)
```

Main components of PUCA
-----------------------

The core function of PUCA is `seedSource` which is also avilable as a GUI using the function `GUI_seedSourcing`. It is recommended that most users use the `GUI_seedSourcing` function as it provides a step-by-step guide to importing the required data (or download from external repositories) and setting various parameters. Detail of how to use the GUI are given in Supplementary Material S1 of Restoration Ecology manuscript (doi: 10.1111/rec.12488). Please also see `vignette("How-to-create-fragmentation-layers", package = "PUCA")` for a tutorial on how to create a fragmentation layer using MODIS satellite data.

Future releases
---------------

The idea of PUCA is to be as user-friendly as possible. Any helpful feedback on improving PUCA will be greatfully recieved. If there are any additional features required, please email <P.A.Harrison@utas.edu.au>. Version 2 of PUCA will be released in the middle of 2017 and will include:

1.  Weighting of environmental variables using *a priori* knowledge
2.  Improved visualisation of output

References
----------

Bivand R, Lewin-Koh N (2015) maptools: Tools for reading and handling spatial objects. R package version 0.8-37. Available at <https://CRAN.R-project.org/package=maptools>

Bivand RS, Pebesma E, Gomez-Rubio V (2013) Applied spatial data analysis with R. Second edn. New York: Springer

Chamberlain C, Ram K, Barve V, Mcglinn D (2015) rgbif: interface to the Global 'Biodiversity' Information Facility 'API'. R package version 0.9.0.9964. Available at <https://github.com/ropensci/rgbif>

Lang DT (2014) RJSONIO: serialize R objects to JSON, object notation. R package version 1.3-0. Available at <https://CRAN.R-project.org/package=RJSONIO>

Grosjean PH (2015). SciViews: A GUI API for R. UMONS, Mons,. URL <http://www.sciviews.org/SciViews-R>

Hijmans RJ (2015) raster: Geographic data analysis and modeling. R package version 2.5-2. Available at <https://CRAN.R-project.org/package=raster>

Le S, Josse J, Husson F (2008) FactoMineR: An R package for multivariate analysis. Journal of Statistical Software 25:1-18

Pebesma EJ, Bivand RS (2005) Classes and methods for spatial data in R. R News 5

R Core Team (2014) R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.

Raymond B, VanDerWal J, Belbin L (2015) ALA4R: Atlas of Living Australia (ALA) data and resources in R. R package version 1.23. Available at <https://github.com/AtlasOfLivingAustralia>

Solymos P (2009) Processing Ecological Data in R with the mefa Package. Journal of Statistical Software 29:1-28

Wickham H, Chang W (2015) devtools: tools to make developing R Packages easier. R package version 1.9.1. Available at <https://CRAN.R-project.org/package=devtools>
