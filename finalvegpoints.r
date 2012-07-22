#note that the end transect coordinates merging is a work in progress, and hasn't been integrated into either the yrb2011 or yrb databases yet. See the coordsmerge.r file for more details


library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)

setwd('c:/users/vppatil/desktop/yrb_gis')

lakes<-readShapePoly('YRB_Sample_Date_Polygons')

yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
transects<-sqlQuery(yrb,"select TransectID, Azimuth,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects where transectid in (select distinct transectid from tblvegtransectmetadata) and vegendeasting >0 and vegendnorthing > 0")


	#check the column headings depending on data source
	coords<-transects[,3:6]
	start.east<-coords[,1]
	start.north<-coords[,2]
	stop.east<-coords[,3]
	stop.north<-coords[,4]
	
simple.zoom<-function(plot.object,addon = NULL)
{
	reps = 0
	while(reps <1)
	{
		cat("click the bottom left and upper right corners of the zoom area")
		loci<-locator(2)
		
		xmin<-loci$x[1]
		ymin<-loci$y[1]
		xmax<-loci$x[2]
		ymax<-loci$y[2]
		if(ymax<ymin)
			reps = 1
		else if(xmax<xmin & !is.null(addon))
			{	plot(plot.object)
				plot(addon,add=T,col='blue')}
		else if(!is.null(addon))
			{	plot(plot.object,xlim=c(xmin,xmax),ylim=c(ymin,ymax))
				plot(addon,add=T,col='blue')}

	}
}
#for identifying lakes in your plot

fill<-function(shape,lakeid)
	plot(shape[shape$LakeID %in% lakeid,],add=T,col='red')

#convert functions
deg2rad <- function(deg) return(deg*pi/180)
flip<-function(theta)
	{
		flip<-(theta+180)
		flip<-ifelse(flip>360,360-flip,flip)
		return(flip)
	}

dX<-function(theta)
	return(-1*sin(deg2rad(theta))*100)
dY<-function(theta)
	return(-1*cos(deg2rad(theta))*100)


#wgs84 projection string
proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#restack
transect.stack<-reshape(transects,varying = list(c('VegStartEasting','VegEndEasting'),c('VegStartNorthing','VegEndNorthing')),direction = 'long',v.names = c('VegEasting','VegNorthing'))
transect.stack$position = 'End'
transect.stack$position[1:dim(transects)[1]] = 'Start'

dd<-data.frame(Id = paste(transect.stack$TransectID,transect.stack$position,sep='_'),Y=transect.stack$VegEasting,X=transect.stack$VegNorthing)
ddTable = transect.stack
ddTable$Id<-paste(transect.stack$TransectID,transect.stack$position,sep='_')

transectbounds.shp<-convert.to.shapefile(dd,ddTable,'Id',1)#1 means point shapefile
write.shapefile(transectbounds.shp, "c:/users/vppatil/desktop/yrb_gis/transectbounds", arcgis=T)
lakepts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/transectbounds',proj4string=CRS(proj4.string))

sqlFetch(yrb,'tblLakeBaseInfo')

##so the initial layer is made

#now try to put habs on top of it,
#collect all habs and their proportions if possible for each transect.
#if you can combine all into a single shapefile with points for each, that would be ideal.

#you could make a separate shapefile with percent cover averaged for each point.

#calc functions
euclid=function(n1,e1,n2,e2)
{
	dist=sqrt((n2-n1)^2 + (e2-e1)^2)
	return(dist)
}

zone.coords<-function(df)
{
	df$dist<-euclid(df$start.north,df$start.east,df$stop.north,df$stop.east)
	df$dY<-df$stop.north-df$start.north
	df$dX<-df$stop.east-df$start.east
	zone1.Y<-df$start.north+df$dY*(zone.mid[1]/df$dist)
	zone2.Y<-df$start.north+df$dY*(zone.mid[2]/df$dist)
	zone3.Y<-df$start.north+df$dY*(zone.mid[3]/df$dist)
	zone1.X<-df$start.east+df$dX*(zone.mid[1]/df$dist)
	zone2.X<-df$start.east+df$dX*(zone.mid[2]/df$dist)
	zone3.X<-df$start.east+df$dX*(zone.mid[3]/df$dist)
	zone1<-data.frame(TransectID=df$TransectID,zone=1,X=zone1.X,Y=zone1.Y)
	zone2<-data.frame(TransectID=df$TransectID,zone=2,X=zone2.X,Y=zone2.Y)
	zone3<-data.frame(TransectID=df$TransectID,zone=3,X=zone3.X,Y=zone3.Y)
	zones<-rbind(zone1,zone2,zone3)
	df<-merge(df,zones,by='TransectID')
	return(df)
}

pythagoras<-function(a,b)
{
	return(sqrt(a^2+b^2))
}

names(transects)[3:6]=c('start.east','start.north','stop.east','stop.north')

pct.cov<-sqlQuery(yrb,"select TransectID,HabitatType,Start,End from tblVegTransectPctCover where transectid in (select distinct transectid from tblTransects) and start is not null and end is not null and start < end")
hab.zone.class<-data.frame()
rows<-length(row.names(pct.cov))
zones<-data.frame(zone=c(1,2,3),begin=c(0,33,67),end=c(33,67,100))

for (r in 1:rows)
{
	pct.cov.row<-pct.cov[r,]
	
	for (z in 1:3)
	{
		sIr <- (pct.cov.row$Start >= zones$begin[z] & pct.cov.row$Start < zones$end[z])
		eIr <- (pct.cov.row$End >= zones$begin[z] & pct.cov.row$End < zones$end[z])
		bound<-pct.cov.row$Start < zones$begin[z] & pct.cov.row$End >= zones$end[z]
			
		if(sIr & (eIr == FALSE)) # only start in range
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=zones$end[z]-pct.cov.row$Start)
			
		}else if (eIr & (sIr == FALSE)) # only end in range
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=pct.cov.row$End-zones$begin[z])
			
		} else if (sIr & eIr) #whole habitat type in range
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=pct.cov.row$End-pct.cov.row$Start)
			
		} else if (bound)
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=33)
			
		} else 
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=0)
			
		}
		
			check = hab.zone.class$widthInZone[hab.zone.class$TransectID == temp$TransectID & hab.zone.class$HabitatType == temp$HabitatType & hab.zone.class$zone == temp$zone]
			if(length(check) > 0) #if there is already a number, add to it
			{
				hab.zone.class$widthInZone[hab.zone.class$TransectID == temp$TransectID & hab.zone.class$HabitatType == temp$HabitatType & hab.zone.class$zone == temp$zone] <-temp$widthInZone + check
			} else
			{
				hab.zone.class<-rbind(hab.zone.class,temp)
			}
	}
}	

#about 20 more in hab zone class because they don't have end points	
hab.zone.class<-subset(hab.zone.class,hab.zone.class$TransectID %in% unique(transects$TransectID))	
hab.zone.class$widthInZone <-ifelse(hab.zone.class$widthInZone > 33,33,hab.zone.class$widthInZone)

#get indices for all maximums
	#do things the hard way
	transect.names<-unique(hab.zone.class$TransectID) #fix code stemming from this name change
	zones<-1:3
	hab.zone.class$maxWidth<-0

for( z  in zones)
{
	for(t in 1:length(transects))
	{
		#find max width corresponding to each transect and zone
		max.width<-max(hab.zone.class$widthInZone[hab.zone.class$zone==z & hab.zone.class$TransectID == transect.names[t]])
		hab.zone.class$maxWidth[hab.zone.class$zone==z & hab.zone.class$TransectID == transect.names[t] & hab.zone.class$widthInZone==max.width]=1
	}
}

zone.mid<-c(33/2,33+(67-33)/2,67+(100-67)/2)	
transects=zone.coords(transects)
	
#remove the few fucked up ones so far.
#will need to get this number down

transects=subset(transects,dist<200 & dist > 50)
hab.zone.class<-subset(hab.zone.class,widthInZone > 0,select=c( "zone","TransectID"  ,"HabitatType" ,"widthInZone"   ))

hab.zone.class$zoneid<-paste(hab.zone.class$TransectID,hab.zone.class$zone,sep = '_')
transects$zoneid<-paste(transects$TransectID,transects$zone,sep='_')

hab.zone.class<-hab.zone.class[,!(names(hab.zone.class) %in% c('TransectID','zone')),] 
transects=transects[,!(names(transects) %in% c('TransectID','zone')),] 

#use spl to get names back

 
transect.habs<-merge(hab.zone.class,transects,by='zoneid')
transect.habs$TransectID<-sapply(transect.habs$zoneid,spl,1,4)
transect.habs$zone<-sapply(transect.habs$zoneid,spl,5,5)
transect.habs2<-transect.habs

transect.names<-unique(transect.habs$TransectID) #fix code stemming from this name change

transect.habs$maxWidth<-0
zones=1:3

for( z  in zones)
{
	for(t in 1:length(transect.names))
	{
		#find max width corresponding to each transect and zone
		max.width<-max(transect.habs$widthInZone[transect.habs$zone==z & transect.habs$TransectID == transect.names[t]])
		transect.habs$maxWidth[transect.habs$zone==z & transect.habs$TransectID == transect.names[t] & transect.habs$widthInZone==max.width]=1
	}
}

#trackduplicates
a<-cbind(tapply(transect.habs$maxWidth,transect.habs$zoneid,sum))
a<-data.frame(reps = a[a>1],zoneid = row.names(a)[a>1])

dups<-a$transect

transect.habs[transect.habs$zoneid %in% as.character(a$zoneid),]
#decide what to do with them.
#where is that 3 coming from- how is that possible-because it was in the emergent zone.

transect.habs$zoneProportion = transect.habs$widthInZone/33
transect.habs<-subset(transect.habs,maxWidth ==1)

d.rm<-!(duplicated(transect.habs$zoneid))
thabs.nodups<-transect.habs[d.rm,]

##############################################################Make the shapefile#######################################	

#wgs84 projection string
proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#restack
transect.habs2<-transect.habs
transect.habs<-thabs.nodups

dd<-data.frame(Id = transect.habs$zoneid,X=transect.habs$X,Y=transect.habs$Y)
ddTable = transect.habs
names(ddTable)[1]= 'Id'

transecthabs.shp<-convert.to.shapefile(dd,ddTable,'Id',1)#1 means point shapefile
write.shapefile(transecthabs.shp, "c:/users/vppatil/desktop/yrb_gis/transecthabs", arcgis=T)
habitatPoints<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/transecthabs',proj4string=CRS(proj4.string))
simple.zoom(habitatPoints,lakes)

	
