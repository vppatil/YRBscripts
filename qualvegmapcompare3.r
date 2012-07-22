#this is the most up to date version


library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)
setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
yrb11<-odbcConnectAccess2007('2011YRBiodiversity.accdb')
setwd('c:/users/vppatil/desktop/yrb')
yrb<-odbcConnectAccess2007('YRBiodiversity.accdb')


meta<-sqlFetch(yrb,'tblVegTransectMetadata')
meta.names<-names(meta)
pct.cov<-sqlQuery(yrb,"Select TransectID,Start,End,HabitatTypeLength,HabitatType,End-Start as GoodLength,(End-Start)/2 as mid  from tblVegTransectPctCover where start is not null and end is not null and HabitatType not in ('EM','FM','AQ','MU')")

transects<-sqlFetch(yrb,'tblTransects')

#get meta part
#need TID, Observer1
lastyr<-readShapePoints('c:/users/vppatil/desktop/YRB_GIS/2010_GRTS_Sampling_Lakes_final')
thisyr<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/GRTS_2011_lakeareas')
jul09<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/July2009mosaic_wgs84')

yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
complete<-sqlQuery(yrb,"select TransectID, Azimuth,LakeEdgeEasting,LakeEdgeNorthing,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects where VegEndEasting is not null and vegendnorthing is not null and VegEndEasting > 0 and VegEndNorthing > 0")

	#check the column headings depending on data source
	coords<-old.t[,c(5:8)]
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


#loop to extract habitat types by zones for all transects
#new hab zones- get midpoints of alll hab zones > 30m.

#so select all habs with dist > 30m. ignore zones
# keep start and end points, calculate midpoint distances, then get midpoint coordinates.

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

#now calculate coords.
#something wrong with distances- need to merge better.

euclid<-function(lat1,lon1,lat2,lon2)
{
	x = lat1-lat2
	y = lon1-lon2
	dist = sqrt((x^2)+(y^2))
	return (dist)
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
	
compare<-subset(complete,select=c('TransectID','VegStartEasting','VegStartNorthing','VegEndEasting','VegEndNorthing'))
names(compare)[2:5] = c('start.east','start.north','stop.east','stop.north')
		
compare<-zone.coords(compare) #(4th leg is bad)
compare$zoneID<-paste(compare$TransectID,compare$zone,sep='_')
drop.names=c('TransectID','zone')
compare<-subset(compare,select=names(compare)[names(compare)%in%drop.names==FALSE])

dominant.class<-hab.zone.class[hab.zone.class$maxWidth==1,]
dominant.class$zoneID<-paste(dominant.class$TransectID,dominant.class$zone,sep='_')

compare<-merge(compare,dominant.class,by='zoneID')
compare<-na.omit(compare)
#temporary
compare=subset(compare,compare$dist < 300)

#Why are there duplicates????


#make the shapefile

#Point


compare$HabID<-paste(compare$zoneID,compare$HabitatType,sep='_')
	
fauxdups<-compare$HabID[duplicated(compare$HabID)]

compare$HabID<-paste(compare$zoneID,compare$HabitatType,sep='_')
	
fauxdups<-compare$HabID[duplicated(compare$HabID)]

compare$HabID[duplicated(compare$HabID)]=paste(fauxdups,'b',sep='')

dd<-data.frame(Id = compare$HabID,X=compare$X,Y=compare$Y)

#temp duplicate fix- habs are halved for most

ddTable<-compare[!duplicated(dd),]
dd<-dd[!duplicated(dd),]

names(ddTable)[16]='Id'
compare.shp<-convert.to.shapefile(dd,ddTable,'Id',1)#1 means point shapefile

write.shapefile(compare.shp, "c:/users/vppatil/desktop/yrb_gis/", arcgis=T)

###start here to read files

######################################################################################
#
#
#							read in shp file already made
#
#
#######################################################################################

#wgs84 projection string
proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


#projection string for landcover map
lcc.proj<-"+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

#wgs84 shapefile
lakepts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/',proj4string=CRS(proj4.string))

lakepts.lcc<-spTransform(lakepts,CRS(lcc.proj))
	julproj<-readShapePoly('c:/users/vppatil/desktop/yrb_gis/July2009mosaic_wgs84',proj4string=CRS(proj4.string))
julproj.lcc<-spTransform(julproj,CRS(lcc.proj))

lakepoly<-readShapePoly('c:/users/vppatil/desktop/yrb_gis/YRB_Sample_Date_Polygons',proj4string=CRS(proj4.string))
lakes.lcc<-spTransform(lakepoly,CRS(lcc.proj))

#read in ak nlcd
setwd('C:/users/vppatil/desktop/yrb_gis/AK_NLCD_2001_land_cover_3-13-08')
filename="ak_nlcd_2001_land_cover_3-13-08_se5.img"  
nlcd=raster(filename)
nlcd.proj=" +proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#read in buffer layer
setwd('c:/users/vppatil/desktop/yrb_gis/')
	buffers<-readShapePoly('yrb_sampledate_transect_erased.shp',proj4string=CRS(nlcd.proj))
	
	e=extent(buffers)
	

#transform compare pts
lakepts.nlcd=spTransform(lakepts,CRS(nlcd.proj))
lakepts.nlcd$Value = extract(nlcd,lakepts.nlcd)
lakepts.nlcd<-data.frame(lakepts.nlcd@data,stringsAsFactors=FALSE)

#get nlcd dbf
setwd('C:/users/vppatil/desktop/yrb_gis/AK_NLCD_2001_land_cover_3-13-08')
nlcd.dbf=read.dbf("ak_nlcd_2001_land_cover_3-13-08_se5.img.vat.dbf")$dbf
#merge
lakepts.nlcd=merge(lakepts.nlcd,nlcd.dbf,by='Value',stringsAsFactors=FALSE)


#limit habitat types
lakepts.nlcd$HabitatTyp=factor(lakepts.nlcd$HabitatTyp)
lakepts.nlcd$HabitatTyp=as.character(lakepts.nlcd$HabitatTyp)
lakepts.nlcd$Land_Cover<-factor(lakepts.nlcd$Land_Cover)
lakepts.nlcd$Land_Cover=as.character(lakepts.nlcd$Land_Cover)


lakepts.nlcd$Hab2=ifelse(lakepts.nlcd$HabitatTyp %in% c('LS','TS'),'SH',ifelse(lakepts.nlcd$HabitatTyp %in% c('DF','MF'),'DMF',lakepts.nlcd$HabitatTyp))


nlcd.sub=subset(lakepts.nlcd,lakepts.nlcd$Hab2 %in% c('CF','DMF','SH','GS') & lakepts.nlcd$Land_Cover !='Open Water')


nlcd.crop=crop(nlcd,e)

#overlay of landcover raster with points file
lc3<-raster('c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif')
lakepts.lcc$Value<-extract(lc3,lakepts.lcc)

#get dbf
lc.dbf<-read.dbf( "c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif.vat.dbf")$dbf
lakepts.lcc<-merge(lakepts.lcc,lc.dbf,by='Value')



#reading in the landfire map image
setwd('c:/users/vppatil/desktop/yrb_gis/landfire/')
filename=dir()[2]
landfire<-raster(filename)
landfire.proj<-spTransform(landfire,CRS(proj4.string))

jul.forlandfire=spTransform(julproj,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
compare.landfire=spTransform(lakepts,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#overlay compare with landfire
lf.cell<-cellFromXY(landfire,compare.landfire)
res<-extract(landfire,compare.landfire)
compare.landfire$VALUE = res
compare.landfire=subset(compare.landfire,compare.landfire$VALUE > 11)

#get landfire dbf
lf.dbf<-read.dbf(dir()[4])$dbf

#mergeh
compare.landfire<-merge(compare.landfire,lf.dbf,by='VALUE')
	
#error matrices

#nlcd
nlcd.table<-table(lakepts.nlcd$HabitatTyp,lakepts.nlcd$Land_Cover)
nlcd.table=nlcd.table[,apply(nlcd.table,2,sum)>0]

#removing water and rare habitat types
nlcd.table2=table(nlcd.sub$Hab2,nlcd.sub$Land_Cover)
nlcd.table2=nlcd.table2[,apply(nlcd.table2,2,sum)>0]
	
#merging forest
nlcd.sub2=nlcd.sub
nlcd.sub2$Land_Cover=as.character(nlcd.sub2$Land_Cover)
nlcd.sub2<-subset(nlcd.sub2,nlcd.sub2$Land_Cover != 'Woody Wetlands')

nlcd.sub2$Land_Cover=ifelse(nlcd.sub2$Land_Cover %in% c('Deciduous Forest','Mixed Forest'),'Mixed Forest',nlcd.sub2$Land_Cover)
nlcd.sub2$Land_Cover=ifelse(nlcd.sub2$Land_Cover %in% c('Shrub/Scrub','Woody Wetlands'),'Shrub',nlcd.sub2$Land_Cover)

#nlcd.sub2$Hab2=ifelse(nlcd.sub2$Hab2 %in% c('CF','DF','MF','DMF'),'F',nlcd.sub2$Hab2)

nlcd.table3=table(nlcd.sub2$Hab2,nlcd.sub2$Land_Cover)
nlcd.table3=nlcd.table3[,apply(nlcd.table3,2,sum)>0]

nlcd.comptable=nlcd.table3[,c(4,5,3,6)]
nlcd=matrix(nlcd.comptable,4,4,byrow=TRUE)
nlcd.k<-kappa(nlcd)

correct=0
for(i in 1:3)
{
	correct=correct+nlcd.comptable[i,i]
}
correct=correct/sum(nlcd.comptable)


a<-rpart(Hab2~Land_Cover,method='class',data=nlcd.sub2)
pred.df<-base::subset(nlcd.sub2,select=c('Land_Cover'))
nlcd.pred=predict(a,newdata=pred.df,type='class')

sum(nlcd.pred==nlcd.sub2$Hab2)/length(nlcd.sub2$Hab2)# 62 percent correct even with woody wetland type

comp1<-table(compare.landfire$NVCSCLASS,compare.landfire$HabitatTyp)
comp1<-comp1[apply(comp1,1,sum)>0,]

compare.landfire<-subset(compare.landfire,compare.landfire$NVCSORDER != 'Non-vegetated')
comp2<-table(compare.landfire$NVCSORDER,compare.landfire$HabitatTyp)
comp2<-comp2[apply(comp2,1,sum)>0,]
	
#crop and plot
e=extent(compare.landfire)
landfire.crop=crop(landfire,e)

#"CF"      "CF/LS"   "DF"      "DT"      "FM"      "GS"      "GS/LS"   "LS"      "LS burn" "LS/DT"   "LS/MF"   "MF"      "TS" 
#habitat types

#simple hab compare
compare.landfire$habSimple=as.character(compare.landfire$HabitatTyp)
compare.landfire$habSimple<-'CF'
compare.landfire$habSimple=ifelse(compare.landfire$HabitatTyp %in% c("CF/LS" ,"GS/LS" ,  "LS"   ,   "LS burn", "LS/DT" ,  "LS/MF","TS"),'S',compare.landfire$habSimple)
#compare.landfire$habSimple=ifelse(compare.landfire$HabitatTyp %in% c('DF','MF'),'DMF',compare.landfire$habSimple)
compare.landfire$habSimple=ifelse(compare.landfire$HabitatTyp %in% c("GS","FM"),'GS',compare.landfire$habSimple)

comp3<-table(compare.landfire$NVCSORDER,compare.landfire$habSimple)
comp3<-comp3[apply(comp3,1,sum)>0,]
comp3<-comp3[,apply(comp3,2,sum)>2]	

comp3=comp3[,c(2,3,1)]

	correct=0
	for(i in 1:3)
	{
		correct=correct + comp3[i,i]
	}
	correct/sum(comp3)


#compare with tree
library(rpart)
a<-rpart(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,method='class',data=compare.landfire)
b<-rpart(HabitatTyp~RED+GREEN+BLUE,method='class',data=compare.landfire)

compare.landfire$HabitatTyp<-factor(compare.landfire$HabitatTyp)
rf1<-randomForest(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,data=compare.landfire)
rf2<-randomForest(HabitatTyp~RED+GREEN+BLUE,data=compare.landfire)

#nlcd tree
c=rpart(HabitatTyp~Land_Cover,method='class',data=lakepts.nlcd)
pred.df<-base::subset(lakepts.nlcd@data,select=c('Land_Cover'))
nlcd.pred=predict(c,newdata=pred.df,type='class')
lakepts.sub=subset(lakepts.nlcd,lakepts.nlcd$HabitatTyp%in% c('GS','CF','MF','DF') &lakepts.nlcd$Land_Cover != 'Open Water')
lakepts.sub2=subset(lakepts.nlcd,lakepts.nlcd$Land_Cover != 'Open Water')

	

#naive comparison
landfire.tree<-rpart(habSimple~NVCSORDER+NVCSCLASS+NVCSSUBCLA,data=compare.landfire)
pred.df<-subset(compare.landfire@data,select=c('NVCSORDER','NVCSCLASS','NVCSSUBCLA'))
tree.pred<-predict(a,newdata=pred.df,type='class')

#subset
compare.sub<-subset(compare.landfire,compare.landfire$HabitatTyp %in% c('CF','MF','LS','GS'))@data
compare.sub$HabitatTyp<-factor(compare.sub$HabitatTyp)

rf1<-randomForest(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,data=compare.sub)
rf2<-randomForest(HabitatTyp~RED+GREEN+BLUE,data=compare.sub)

#make dummy variables for fuzzy
compare.sub$CF = compare.sub$MF = compare.sub$GS = compare.sub$LS = 0
compare.sub$CF=ifelse(compare.sub$HabitatTyp =='CF',1,0)
compare.sub$LS=ifelse(compare.sub$HabitatTyp =='LS',1,0)
compare.sub$MF=ifelse(compare.sub$HabitatTyp =='MF',1,0)
compare.sub$GS=ifelse(compare.sub$HabitatTyp =='GS',1,0)

habs.veg<-subset(compare.sub,select=c('CF','MF','LS','GS'))
library(vegan)
habs.dist<-vegdist(habs.veg)

#fuzzy 1
f1<-fso(~NVCSORDER+NVCSCLASS,habs.dist,data=compare.sub)

#refit, using holdout of 20% as test
#change the success line depending on response variable
pred.boot<-function(b,df,cols,form)
{
	success.boot = vector()
	rows = dim(df)[1]
	for(i in 1:b)
{

	#get sample for validation
	samp=sample(rows,rows/10,replace=FALSE)
	validate=df[samp,]
	pred.df=subset(validate,select=cols)
	train=df[-samp,]
	
	#fit the model
	a<-rpart(form,method='class',data=train)
	pred=predict(a,pred.df,type='class')
	
	success=sum(pred==validate$Hab2)/length(pred)
	success.boot<-c(success.boot,success)
}
	return(success.boot)
}
a<-rpart(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,method='class',data=compare.sub)

#reconvert field to factor
nlcd.sub2$Land_Cover<-factor(nlcd.sub2$Land_Cover)

#get cross-validated classification correctness
lakepts.pred=pred.boot(500,lakepts.sub,'Land_Cover',list(formula=HabitatTyp~Land_Cover))
nlcd.subpred=pred.boot(500,nlcd.sub2,'Land_Cover',list(formula=Hab2~Land_Cover))	
lcc.pred=pred.boot(500


res<-lakepts.lcc@data
table(res$HabitatTyp=='GS',res$Revised)

res$CF = 0
res$CF[grep('needleaf',res$OriginalLC)]=1
table(res$HabitatTyp == 'CF',res$CF == 1)

res2<-subset(res,res$OriginalLC != 'Water')
table(res2$HabitatTyp == 'CF',res2$CF == 1)
names
cbind(table(res$HabitatTyp=='GS',res$OriginalLC)[2,])
cbind(table(res$HabitatTyp=='CF',res$OriginalLC)[2,])
cbind(table(res$HabitatTyp=='LS',res$OriginalLC)[2,])
cbind(table(res$HabitatTyp=='TS',res$OriginalLC)[2,])

habs<-table(compare$HabitatType)


crop(lc3,drawExtent())

##IT WORKS!!!!!!!!


##########################################################################

#now to read in dbase tables for the poly_SpatialJoin layers
#they will need to be merged with the dbfs for each raster

setwd('c:/users/vppatil/desktop/yrb_gis')
dir()[grep('dbf',dir())]
lf.convert=read.dbf('lf_join.dbf')$dbf	
lc.convert=read.dbf('lc_join.dbf')$dbf
nlcd.convert=read.dbf('nlcd_join.dbf')$dbf

setwd('c:/users/vppatil/desktop/yrb_gis')
lf=read.dbf("landfire_masked.img.vat.dbf" )$dbf
names(lf)[1]='GRIDCODE'
lc=read.dbf("LC_masked.tif.vat.dbf"  )$dbf
names(lc)[1]='GRIDCODE'
setwd('c:/users/vppatil/desktop/old compare/AK_NLCD_2001_land_cover_3-13-08')
nlcd=read.dbf( "ak_nlcd_2001_land_cover_3-13-08_se5.img.vat.dbf")$dbf
names(nlcd)[1]='GRIDCODE'

yrb.erase<-read.dbf('yrb_sampledate_transect_erased.dbf')$dbf
#merge all on 

lf.convert=merge(lf.convert,lf,by='GRIDCODE')
lc.convert=merge(lc.convert,lc,by='GRIDCODE')
nlcd.convert=merge(nlcd.convert,nlcd,by='GRIDCODE')

#now table ShapeArea based on lakeID,landcover
lf.xtab<-cbind(xtabs(Shape_Area~NVCSORDER+LakeID,data=lf.convert))
lf.xtab<-tapply(lf.convert$Shape_Area,list(lf.convert$NVCSORDER,lf.convert$LakeID),sum)
lf.xtab[is.na(lf.xtab)]=0
lf.xtab<-lf.xtab[apply(lf.xtab,1,sum)>0,]
lc.xtab<-cbind(xtabs(Shape_Area~OriginalLC+LakeID,data=lc.convert))
lc.xtab<-lc.xtab[apply(lc.xtab,1,sum)>0,]
nlcd.xtab<-cbind(xtabs(Shape_Area~Land_Cover+LakeID,data=nlcd.convert))
nlcd.xtab<-nlcd.xtab[apply(nlcd.xtab,1,sum)>0,]

#get proportions of habitat types from PCT cover data (mean per lake)
habs=sqlQuery(yrb,"select TransectID,HabitatType,HabitatTypeLength,Start,End from tblVegTransectPctCover",stringsAsFactors=FALSE)
habs=na.omit(habs)
for(i in 1:dim(habs)[1])
{
	habs$LakeID[i] =paste(strsplit(habs$TransectID[i],split='_')[[1]][2:4],collapse='_')
}
lakes=unique(habs$LakeID)

#add in zeros for transects without a habitat type
for (k in 1:length(lakes))
{
	lakesub=subset(habs,habs$LakeID == lakes[k])
	habtable=table(lakesub$TransectID,lakesub$HabitatType)

	for(i in 1:dim(habtable)[1])
	{
		for(j in 1:dim(habtable)[2])
		{
			if(habtable[i,j]==0)
				habs=rbind(habs,data.frame(TransectID = row.names(habtable)[i],HabitatType=colnames(habtable)[j],HabitatTypeLength = 0,Start = 0,End = 0,LakeID = paste(strsplit(row.names(habtable)[i],split='_')[[1]][2:4],collapse='_')))
		}
	}
}

#now get mean of habs by LakeID
field.lengths=tapply(habs$HabitatTypeLength,list(habs$LakeID,habs$HabitatType),mean)
start.means<-tapply(habs$Start,list(habs$LakeID,habs$HabitatType),mean)
end.means<-tapply(habs$End,list(habs$LakeID,habs$HabitatType),mean)

sqlQuery(yrb,"select distinct(HabitatType) from tblVegTransectPctCover where TransectID like '%0_15_1'")

field.lengths[is.na(field.lengths)]=0
start.means[is.na(start.means)]=0
end.means[is.na(end.means)]=0

starts=unique(c(start.means))
ends=unique(c(end.means))
write.csv(starts,'c:/users/vppatil/desktop/unique_start.csv')
write.csv(ends,'c:/users/vppatil/desktop/unique_end.csv')

#now need to multiply this by perimeters for all lakes
#read in areas dbf
setwd('c:/users/vppatil/desktop/yrb_gis')
yrb.dbf=read.dbf('YRB_Sample_Date_Polygons.dbf')$dbf
yrb.dbf$perim_km=yrb.dbf$perim_km*1000
field.lengths=data.frame(field.lengths)

#multiply by areasBADDD!!!!
#for(i in 1:dim(field.lengths)[1])

#{
#	field.lengths[i,]=field.lengths[i,]*yrb.dbf$perim_km[yrb.dbf$LakeID == row.names(field.lengths)[i]]
#}

field.lengths=t(field.lengths)
field.lengths=field.lengths[apply(field.lengths,1,sum)>0,]
dim(field.lengths)

start.means=t(start.means)
start.means=start.means[apply(start.means,1,sum)>0,]
dim(start.means)

end.means=t(end.means)
end.means=end.means[apply(end.means,1,sum)>0,]
dim(end.means)

#now fix names so comparable

	#landfire
	lfxtab.merge=rbind(lf.xtab[1:3,],(lf.xtab[4,]+lf.xtab[5,]))
	row.names(lfxtab.merge)[4] = 'Forest'
	lfxtab.merge=lfxtab.merge[c(4,3,1,2),]

	nlcdxtab.merge=rbind((nlcd.xtab[2,]+nlcd.xtab[5,]+nlcd.xtab[6,]),
						  (nlcd.xtab[1,]+nlcd.xtab[4,]+nlcd.xtab[8,]),
						  (nlcd.xtab[3,]+nlcd.xtab[9,]),
						  nlcd.xtab[10,])
	row.names(nlcdxtab.merge)=c('F','GS','SH','Woody Wetlands')
	nlcdxtab.merge=nlcdxtab.merge[c(1,3,2,4),]


	#30 class landcover
	frows=c(2,3,4,8,9,10,11,21,22,23,24)
	srows=c(7,12:20,25)
	gsrows=c(1,5,6,27)

	lc.f=lc.xtab[frows[1],]
	for(i in 2:length(frows))
		lc.f=lc.f+lc.xtab[frows[i],]
		
	lc.s=lc.xtab[srows[1],]
	for(i in 2:length(srows))
		lc.s=lc.s+lc.xtab[srows[i],]
		
	lc.gs=lc.xtab[gsrows[1],]
	for(i in 2:length(gsrows))
		lc.gs=lc.gs+lc.xtab[gsrows[i],]
	lcxtab.merge=rbind(lc.f,lc.s,lc.gs)


#field lengths merge
[1,] "CF"                 
 [2,] "CF.LS"              
 [3,] "CF.TS"              
 [4,] "DF"                 
 [5,] "DT"                 
 [6,] "EM"                 
 [7,] "FM"                 
 [8,] "GS"                 
 [9,] "GS.LS"              
[10,] "GS.TS"              
[11,] "LS"                 
[12,] "LS.burn"            
[13,] "LS.CF"              
[14,] "LS.DT"              
[15,] "LS.MF"              
[16,] "MF"                 
[17,] "MUFL"               
[18,] "not.defined.thicket"
[19,] "TS"    
       
end.means=rbind(  (end.means[1,]+end.means[4,]+end.means[5,]+end.means[15,]), #forest
					 
					 (end.means[7,]+end.means[8,]+end.means[9,]),	#gs
					 
					 (end.means[10,]+end.means[11,]+end.means[12,]+end.means[13,]+	#shrub
					 end.means[14,]+end.means[2,]+end.means[3,]+end.means[18,]))
					 
row.names(end.means) = c('Forest','GS','Shrub')

start.means=rbind(  (start.means[1,]+start.means[4,]+start.means[5,]+start.means[14,]), #forest
					 
					 (start.means[7,]+start.means[8,]),	#gs
					 
					 (start.means[9,]+start.means[10,]+start.means[11,]+start.means[12,]+	#shrub
					 start.means[13,]+start.means[2,]+start.means[3,]+start.means[17,]))
row.names(start.means)=c('Forest','GS','Shrub')

#convert to hectares
#match up lake names
lcxtab.merge=lcxtab.merge[,match(colnames(field.merge),colnames(lcxtab.merge))]
lfxtab.merge=lfxtab.merge[,match(colnames(field.merge),colnames(lfxtab.merge))]
nlcdxtab.merge=nlcdxtab.merge[,match(colnames(field.merge),colnames(nlcdxtab.merge))]


setwd('c:/users/vppatil/desktop/yrb_gis')
multi<-read.dbf('bufferall_zeros.dbf')$dbf
starts<-read.dbf('starts.dbf')$dbf
ends<-read.dbf('ends.dbf')$dbf
starts$distance=round(starts$distance,3)
ends$distance=round(ends$distance,3)
names(starts)[15]='starts'
names(ends)[15]='ends'

f.starts=vector()
g.starts=vector()
s.starts=vector()

f.ends=vector()
g.ends=vector()
s.ends=vector()

for(i in 1:dim(start.means)[2])
{
	f.starts=c(f.starts,starts$Shape_Area[starts$LakeID==colnames(start.means)[i] & starts$starts == start.means[1,i]])
	g.starts=c(g.starts,starts$Shape_Area[starts$LakeID==colnames(start.means)[i] & starts$starts == start.means[2,i]])
	s.starts=c(s.starts,starts$Shape_Area[starts$LakeID==colnames(start.means)[i] & starts$starts == start.means[3,i]])
	
	f.ends=c(f.ends,ends$Shape_Area[ends$LakeID==colnames(end.means)[i] & ends$ends == end.means[1,i]])
	g.ends=c(g.ends,ends$Shape_Area[ends$LakeID==colnames(end.means)[i] & ends$ends == end.means[2,i]])
	s.ends=c(s.ends,ends$Shape_Area[ends$LakeID==colnames(end.means)[i] & ends$ends == end.means[3,i]])
}

field.areas=rbind(f.starts,g.starts,s.starts,f.ends,g.ends,s.ends)
colnames(field.areas)=colnames(field.merge)

#for every lake, there is a buffer of every width
#for each comm type, I want the buffer corresponding to the comm width.
#some are missing suggesting inexact matches.
# i need to find those matches.

#get list of unique widths from fields
fieldwidths=sort(unique(c(field.merge)))
fieldwidths[is.na(match(fieldwidths,multi$distance))]

round(fieldwidths,3)

#correlations for forest
a=data.frame(
lc.f=lcxtab.merge[1,],
lf.f=lfxtab.merge[1,],
nlcd.f=nlcdxtab.merge[1,],
field.f=field.areas[1,])
a[is.na(a)]=0

#correlations for shrubs
b=data.frame(
lc.f=lcxtab.merge[2,],
lf.f=lfxtab.merge[2,],
nlcd.f=nlcdxtab.merge[2,],
field.f=field.areas[2,])
b[is.na(b)]=0

#correlations for grass/sedge
c=data.frame(
lc.f=lcxtab.merge[3,],
lf.f=lfxtab.merge[3,],
nlcd.f=nlcdxtab.merge[3,],
field.f=field.areas[3,])
c[is.na(c)]=0






















