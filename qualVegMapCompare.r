#note that the end transect coordinates merging is a work in progress, and hasn't been integrated into either the yrb2011 or yrb databases yet. See the coordsmerge.r file for more details


library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)

setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
yrb11<-odbcConnectAccess2007('2011YRBiodiversity.accdb')
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

sqlQuery(yrb,"select distinct(TransectID) from tblVegTransectSppList where TransectID like '%1_42_2'")
sqlQuery(yrb11,"select distinct(Bearing) from tblVegTransectSppList where LakeID = '1_42_2'")

old.t<-sqlQuery(yrb,"select TransectID, Azimuth,LakeEdgeEasting,LakeEdgeNorthing,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects")
meta<-sqlFetch(yrb,'tblVegTransectMetadata')
meta.names<-names(meta)
pct.cov<-sqlFetch(yrb,'tblVegTransectPctCover')
#pct.cov<-sqlFetch(yrb,'tblVegTransectPctCover')
 #pct.cov<-pct.cov[grep('0_32_1',pct.cov$TransectID),]

 #may want to keep the next part in- or remove aquatic zones
 pct.cov<-pct.cov[is.na(pct.cov$Start)==FALSE,]

#transects<-sqlFetch(yrb,'tblTransects')
#transects<-sqlFetch(yrb11,'tblTransects')

#if working off coords merge from coordsmerge.r, then do:
#transects=t

#get meta part
#need TID, Observer1
lastyr<-readShapePoints('c:/users/vppatil/desktop/YRB_GIS/2010_GRTS_Sampling_Lakes_final')
thisyr<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/GRTS_2011_lakeareas')
jul09<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/July2009mosaic_wgs84')



#option for when data are taken straight from the database
complete<-old.t[is.na(old.t$VegEndEasting)==FALSE,]

#option from coords merge
#complete = t[is.na(t$vegStopEast) == FALSE,]

	#check the column headings depending on data source
	coords<-complete[,c(2:5)]
	start.east<-coords[,1]
	start.north<-coords[,2]
	stop.east<-coords[,3]
	stop.north<-coords[,4]
	
simple.zoom<-function(plot.object)
{
	cat("click the bottom left and upper right corners of the zoom area")
	b.left<-locator(1)
	u.right<-locator(1)
	xmin<-b.left$x
	ymin<-b.left$y
	xmax<-u.right$x
	ymax<-u.right$y
	if(xmax<xmin|ymax<ymin)
		plot(plot.object)
	else
		plot(plot.object,xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
#for identifying lakes in your plot
circle.lake<-function(shape,lakeID)
{
	dat<-shape@data
	x<-dat$UTMX[dat$Strat_Samp == lakeID]
	y<-dat$UTMY[dat$Strat_Samp== lakeID]
	points(x,y,cex=4,col='red',lty=2)
}

#also TID, Azimuth, TransectID, Azimuth,LakeEdgeEasting,LakeEdgeNorthing,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing


#don't remember why to use this bit. should have commented it better the first time.
query<-"select TransectID, Azimuth,LakeEdgeEasting,LakeEdgeNorthing,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects where TransectID in (select TransectID from tblVegTransectMetadata where Observer1 = 'Vijay Patil') order by TransectID"

vij.lakes<-sqlQuery(yrb11,"select LakeID from tblTransects where TransectID in (select distinct(TransectID) from tblVegTransectMetadata where Observer1 = 'Vijay Patil')")

sqlQuery(yrb11,"select * from tblVegTransectMetadata where TransectID like '%1_57_1'")

vij.transects<-sqlQuery(yrb11,query)
#none complete

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
x1<-419843
y1<-7337045	

theta<-340
x<-dX(theta)
y<-dY(theta)

test.x<-x1+x
test.y<-y1+y

test.x2<-x1-x
test.y2<-y1-y


#loop to extract habitat types by zones for all transects

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
			hab.zone.class<-rbind(hab.zone.class,temp)
		}else if (eIr & (sIr == FALSE)) # only end in range
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=pct.cov.row$End-zones$begin[z])
			hab.zone.class<-rbind(hab.zone.class,temp)
		} else if (sIr & eIr) #whole habitat type in range
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=pct.cov.row$End-pct.cov.row$Start)
			hab.zone.class<-rbind(hab.zone.class,temp)
		} else if (bound)
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=33)
			hab.zone.class<-rbind(hab.zone.class,temp)
		} else 
		{
			temp<-data.frame(zone=zones$zone[z],TransectID=pct.cov.row$TransectID,HabitatType=pct.cov.row$HabitatType,widthInZone=0)
			hab.zone.class<-rbind(hab.zone.class,temp)
		}
	}
}

#get indices for all maximums
#do things the hard way
transects<-unique(hab.zone.class$TransectID)
zones<-1:3
hab.zone.class$maxWidth<-0

for( z  in zones)
{
	for(t in 1:length(transects))
	{
		#find max width corresponding to each transect and zone
		max.width<-max(hab.zone.class$widthInZone[hab.zone.class$zone==z & hab.zone.class$TransectID == transects[t]])
		hab.zone.class$maxWidth[hab.zone.class$zone==z & hab.zone.class$TransectID == transects[t] & hab.zone.class$widthInZone==max.width]=1
	}
}

zone.mid<-c(33/2,33+(67-33)/2,67+(100-67)/2)

compare<-subset(complete,select=c('TransectID','VegStartEasting','VegStartNorthing','VegEndEasting','VegEndNorthing'))
names(compare)[2:5] = c('start.east','start.north','stop.east','stop.north')
compare<-subset(compare,compare$start.east>0&compare$start.north>0&compare$stop.east>0&compare$stop.north>0)

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
	
compare<-zone.coords(compare) #(4th leg is bad)
compare$zoneID<-paste(compare$TransectID,compare$zone,sep='_')
drop.names=c('TransectID','zone')
compare<-subset(compare,select=names(compare)[names(compare)%in%drop.names==FALSE])

dominant.class<-hab.zone.class[hab.zone.class$maxWidth==1,]
dominant.class$zoneID<-paste(dominant.class$TransectID,dominant.class$zone,sep='_')

compare<-merge(compare,dominant.class,by='zoneID')

#some are fucked up
#need to fix them
compare<-na.omit(compare)
swap.e=compare$stop.east[compare$dist > 1000]
compare$stop.east[compare$dist > 1000] = compare$stop.north[compare$dist > 1000]
compare$stop.north[compare$dist > 1000] = swap.e

#temporary
compare=compare[-grep('1_37',compare$zoneID),]
compare=subset(compare,compare$dist < 300)

#Why are there duplicates????

compare

#make the shapefile

#Point
dd<-data.frame(Id = compare$zoneID,X=compare$X,Y=compare$Y)

#temp duplicate fix- habs are halved for most

ddTable<-compare[!duplicated(dd),]
dd<-dd[!duplicated(dd),]

names(ddTable)[1]='Id'
compare.shp<-convert.to.shapefile(dd,ddTable,'Id',1)#1 means point shapefile

write.shapefile(compare.shp, "c:/users/vppatil/desktop/yrb_gis/compare", arcgis=T)

######################################################################################
#
#
#							read in shp file already made
#
#
#######################################################################################

compare.pts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/compare')

proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

lcc.proj<-"+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

lakepts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/compare',proj4string=CRS(proj4.string))

lakepts.lcc<-spTransform(lakepts,CRS(lcc.proj))
julproj<-readShapePoly('c:/users/vppatil/desktop/yrb_gis/July2009mosaic_wgs84',proj4string=CRS(proj4.string))
julproj.lcc<-spTransform(julproj,CRS(lcc.proj))
lc3<-raster('c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif')
lc.cells<-cellFromXY(lc3,lakepts.lcc)
lakepts.lcc$Value<-extract(lc3,lakepts.lcc)



#get dbf
lc.dbf<-read.dbf( "c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif.vat.dbf")$dbf
lakepts.lcc<-merge(lakepts.lcc,lc.dbf,by='Value')

res<-lakepts.lcc@data
table(res$HabitatTyp,res$Revised)

setwd('c:/users/vppatil/desktop/')
res<-read.csv('lc_comparison.csv',header=TRUE,stringsAsFactors=FALSE)
library(tree)
res$habsimple='F'
res$habsimple=ifelse(res$HabitatTyp=='LS' | res$HabitatTyp == 'TS','S',ifelse(res$HabitatTyp == 'GS' , 'G',res$habsimple))
res$habsimple[grep('/',res$HabitatTyp)] = 'S'
res$habsimple[res$HabitatTyp == 'LS burn'] = 'S'
res$habsimple<-factor(res$habsimple)

b<-tree(habsimple~Value,data=res)

class.table<-table(res$OriginalLC)
present<-row.names(class.table[class.table > 0])

res2<-subset(res,res$OriginalLC %in% present)
res2$OriginalLC<-factor(res2$OriginalLC)
#hab vij is my classifications

res2$Hab2=res2$HabitatTyp
res2$Hab2[res2$Hab2 == 'TS'] = 'LS'
res2$Hab2[res2$Hab2 == 'DT'] = 'CF'
res2$Hab2[res2$Hab2 == 'DF'] = 'MF'
res2$Hab2[grep('/',res2$HabitatTyp)] = 'Other'
res2$Hab2[res2$HabitatTyp == 'LS burn'] = 'LS'
res2$Hab2<-factor(res2$Hab2)
res2$reclass<-'GS'
res2$OriginalLC<-as.character(res2$OriginalLC)
res2$Hab2<-as.character(res2$Hab2)

reclass<-read.csv('c:/users/vppatil/desktop/lc_reclass_key.csv',header=TRUE,stringsAsFactors=FALSE)
names(reclass)[1] = 'Hab2'

GS.i<-c(5,6,26,1)
LS.i<-c(11,12,13,14,15,16,17,18,24)
MF.i<-c(8,9,10,7)
DF.i<-c(20,2)
CF.i<-c(10,21,22,23,3,4)

res2$reclass[res2$OriginalLC %in% present[GS.i]] = 'GS'
res2$reclass[res2$OriginalLC %in% present[LS.i]] = 'LS'
res2$reclass[res2$OriginalLC %in% present[CF.i]] = 'CF'
res2$reclass[res2$OriginalLC %in% present[DF.i]] = 'DF'
res2$reclass[res2$OriginalLC %in% present[MF.i]] = 'MF'

#now do a tree of hab2 ~ reclass
c<-tree(Hab2~Revised,data=res2)



rows<-length(res$Hab2)
classes = 21
for ( i in 1:rows)
{
	for (j in 1:classes)
	{
		res2$reclass[i] <-ifelse(res2$OriginalLC[i] == reclass$reclass[j],reclass$Hab2[j],res2$reclass[i])
	}
}

res<-merge(res,

crop(lc3,drawExtent())

##IT WORKS!!!!!!!!