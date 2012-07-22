#lidar workflow

get points shapefile
isolate points from lakes within lidar image (clip based on lidar image shapefile
create lines in r between all start and end points
make line file
extract raster elevations at fixed points along lines, or for all pixels intersecting lines if possible
plot between start and end
calculate average elevation change, some kind of slope index
compare somehow with veg transition information

xyFromCell to get coordinates from specified raster cells- get the cells that intersect with the points, then take coords

from raster package

once you have the line, use cellFromLine to extract all cells that intersect each line- returns a list.

#zone coords code is in qualvegmapcompare, but should be merged over into 3. then delete dupe code.

library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)

transect.ends<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/transectbounds')@data[,1:5]

dd=data.frame(Id = transect.ends$TransectID,X = transect.ends$VegEasting,Y = transect.ends$VegNorthin)
ddTable = transect.ends	
names(ddTable)[1]<-'Id'

test<-convert.to.shapefile(dd,ddTable,'Id',3)
#not working right.


transects = unique(transect.ends$TransectID)
lines.df<-data.frame(TransectID=NULL,start.east=NULL,start.north=NULL,stop.east=NULL,stop.north=NULL)

for(i in 1:length(transects))
{
	start.east<-transect.ends$VegEasting[transect.ends$TransectID == transects[i] & transect.ends$position=='Start']
	start.north<-transect.ends$VegNorthin[transect.ends$TransectID == transects[i] & transect.ends$position=='Start']
	
	stop.east<-transect.ends$VegEasting[transect.ends$TransectID == transects[i] & transect.ends$position=='End']
	stop.north<-transect.ends$VegNorthin[transect.ends$TransectID == transects[i] & transect.ends$position=='End']

	temp<-data.frame(TransectID=transects[i],start.east,start.north,stop.east,stop.north)
	lines.df<-rbind(lines.df,temp)
}

?reshape
	