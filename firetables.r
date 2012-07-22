
setwd('c:/users/vppatil/desktop/yrb_gis/')
dir()[grep('fire',dir())]
filename=dir()[grep('fire',dir())][10]

library(rgdal)
library(RODBC)
library(shapefiles)
library(sp)
library(raster)
library(proj4)
library(maptools)

fires<-read.dbf(filename)$dbf
fire.table<-table(fires$Lake_ID,fires$FireYear)

fires.2010<-read.dbf(filename)$dbf

fire2010.table<-table(fires.2010$Lake_ID,fires.2010$FireYear)