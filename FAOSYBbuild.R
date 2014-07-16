########################################################################
## Simple script for building the package
########################################################################


## The documentation of roxygen can be find at:
## https://github.com/hadley/devtools/wiki/docs-function

library(roxygen2)
library(sp)
library(maptools)

## Remove the folder if it exists
if(file.exists("./FAOSYB"))
    unlink("FAOSYB", recursive = TRUE)

## Build the package
package.skeleton("FAOSYB", code_files = paste("./Codes/",
                           dir("./Codes/", pattern = "\\.R$"), sep = ""))

## Shapefile for map
## ---------------------------------------------------------------------

## Need to fix the non-ASCII strings
shpLocation = "./GAUL_SYB_2013/GAUL0_new3_Rob_NoAntca_simple"
GAULspatialPolygon = readShapePoly(shpLocation,
    proj4string = CRS(projargs = "+proj=robin +ellps=WGS84"))
save(GAULspatialPolygon, file = "GAULspatialPolygon.RData")

## Include the data
dir.create("FAOSYB/data")
file.copy(from = "./GAULspatialPolygon.RData",
          to = "./FAOSYB/data/", overwrite = TRUE)
file.copy(from = "./DESCRIPTION", to = "FAOSYB/",
          overwrite = TRUE)

## Use roxygen to build the documentation
roxygenize("FAOSYB")

## Include vignette
dir.create("./FAOSYB/vignettes/")
file.copy(from = "./Documentation/FAOSYB.pdf",
          to = "./FAOSYB/vignettes/", overwrite = TRUE)

## dir.create("./FAOSYB/inst/doc/")
## file.copy(from = "./Documentation/FAOSYB.pdf",
##           to = "./FAOSYB/inst/doc/", overwrite = TRUE)

## Create the vignette hack
## cat("%\\VignetteIndexEntry{Graphic guide}\n\\documentclass{article}\n\\begin{document}\n\\end{document}", file = "./FAOSYB/inst/doc/FAOSYB.Rnw")


## Build and check the package (Windows)
system("R CMD INSTALL --build FAOSYB")
system("R CMD build FAOSYB")
## system("Rcmd check FAOSYB")
system("R CMD check --as-cran FAOSYB")



