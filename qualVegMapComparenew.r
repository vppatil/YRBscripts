#note that the end transect coordinates merging is a work in progress, and hasn't been integrated into either the yrb2011 or yrb databases yet. See the coordsmerge.r file for more details


library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)
setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
#setwd('c:/users/vppatil/desktop/yrb_gis')
yrb11<-odbcConnectAccess2007('2011YRBiodiversity.accdb')
meta<-sqlFetch(yrb11,'tblVegTransectMetadata')
meta.names<-names(meta)
pct.cov<-sqlFetch(yrb11,'tblVegTransectPctCover')
#pct.cov<-sqlFetch(yrb,'tblVegTransectPctCover')
 #pct.cov<-pct.cov[grep('0_32_1',pct.cov$TransectID),]
 
 #may want to keep the next part in- or remove aquatic zones
 pct.cov<-pct.cov[is.na(pct.cov$Start)==FALSE,]

#transects<-sqlFetch(yrb,'tblTransects')
#transects<-sqlFetch(yrb11,'tblTransects')

#if working off coords merge from coordsmerge.r, then do:
transects=t

#get meta part
#need TID, Observer1
lastyr<-readShapePoints('c:/users/vppatil/desktop/YRB_GIS/2010_GRTS_Sampling_Lakes_final')
thisyr<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/GRTS_2011_lakeareas')
jul09<-readShapePoly('c:/users/vppatil/desktop/YRB_GIS/July2009mosaic_wgs84')

yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
old.t<-sqlQuery(yrb,"select TransectID, Azimuth,LakeEdgeEasting,LakeEdgeNorthing,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects where transectid in (select distinct transectid from tblvegtransectmetadata) and vegendeasting > 0")

#option for when data are taken straight from the database
#complete<-old.t[is.na(old.t$VegEndEasting)==FALSE,]

#option from coords merge
#complete = t[!is.na(t$vegStopEast),]

	#check the column headings depending on data source
	coords<-complete[,c(4,5,15,14)]
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
#new hab zones- get midpoints of alll hab zones > 30m.

#so select all habs with dist > 30m. ignore zones
# keep start and end points, calculate midpoint distances, then get midpoint coordinates.


hab.zone.class<-data.frame()
rows<-length(row.names(pct.cov))
hab.zone.noclass<-data.frame()

 #big<-sqlQuery(y11,"Select TransectID,Start,End,HabitatTypeLength,HabitatType,End-Start as GoodLength,(End-Start)/2 as mid  from tblVegTransectPctCover")
 big.old<-sqlQuery(yrb,"Select TransectID,Start,End,HabitatTypeLength,HabitatType,End-Start as GoodLength,(End-Start)/2 as mid  from tblVegTransectPctCover where HabitatTypeLength > 20")
#use HabitatTypeLength for big.old

#remove aquatic zones
big.old<-big.old[!is.na(big.old$Start),]

#remove small zones
big.old=subset(big.old,big.old$HabitatTypeLength > 20)


big.merge=big.old

#now calculate coords.
#something wrong with distances- need to merge better.

compare<-subset(complete,select=c('TransectID','LakeEdgeEasting','LakeEdgeNorthing','VegEndEasting','VegEndNorthing'))
names(compare)[2:5] = c('start.east','start.north','stop.north','stop.east')

compare<-merge(compare,big.merge,by='TransectID')

b=sqlQuery(yrb,"select tblTransects.LakeEdgeEasting,tblTransects.LakeEdgeNorthing,tblTransects.VegEndEasting,tblTransects.VegEndNorthing,tblVegTransectPctCover.* from tblTransects left join tblVegTransectPctCover on tblTransects.TransectID = tblVegTransectPctCover.TransectID")

b=subset(b,is.na(b$Start)==FALSE)
names(b)[1:4]=c('start.north','start.east','stop.north','stop.east')

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
	df$Y<-df$start.north+df$dY*(df$mid/df$dist)
	df$X<-df$start.east+df$dX*(df$mid/df$dist)
	
	return(df)
}
	
pythagoras<-function(a,b)
{
	return(sqrt(a^2+b^2))
}
	
compare=b
compare$mid=(compare$End+compare$Start)/2
compare<-zone.coords(compare) #(4th leg is bad)
#some are fucked up
#need to fix them
compare=compare[is.na(compare$dist)==FALSE,]

#temporary
compare=subset(compare,compare$dist < 300)

#Why are there duplicates????


#make the shapefile

#Point


compare$HabID<-paste(compare$TransectID,compare$HabitatType,sep='_')

#fauxdups<-compare$HabID[duplicated(compare$HabID)]
#compare$HabID[duplicated(compare$HabID)]=paste(fauxdups,'b',sep='')

dups<-duplicated(compare[,c(5,6)]) 
compare[dups,]
compare<-compare[!dups,]
dd<-data.frame(Id = compare$HabID,Y=compare$Y,X=compare$X)

#temp duplicate fix- habs are halved for most




ddTable<-compare

names(ddTable)[33]='Id'
compare_simple.shp<-convert.to.shapefile(dd,ddTable,'Id',1)#1 means point shapefile

write.shapefile(compare_simple.shp, "c:/users/vppatil/desktop/yrb_gis/compare_simple", arcgis=T)

cs<-readShape
###start here to read files

######################################################################################
#
#
#							read in shp file already made
#
#
#######################################################################################


#transect points file
compare.pts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/compare_simple',proj4string=CRS(proj4.string))

#wgs84 projection string
proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#projection string for landcover map
lcc.proj<-"+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

#wgs84 shapefile
lakepts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/compare',proj4string=CRS(proj4.string))

lakepts.lcc<-spTransform(lakepts,CRS(lcc.proj))
julproj<-readShapePoly('c:/users/vppatil/desktop/yrb_gis/July2009mosaic_wgs84',proj4string=CRS(proj4.string))
julproj.lcc<-spTransform(julproj,CRS(lcc.proj))

#overlay of landcover raster with points file
lc3<-raster('c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif')
lc.cells<-cellFromXY(lc3,lakepts.lcc)
lakepts.lcc$Value<-extract(lc3,lakepts.lcc)

#reading in the landfire map image
setwd('c:/users/vppatil/desktop/yrb_gis/landfire/')
filename=dir()[2]
landfire<-raster(filename)
landfire.proj<-spTransform(landfire,CRS(proj4.string))

jul.forlandfire=spTransform(julproj,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
compare.landfire=spTransform(compare.pts,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#overlay compare with landfire
lf.cell<-cellFromXY(landfire,compare.landfire)
res<-extract(landfire,compare.landfire)
compare.landfire$VALUE = res
compare.landfire=subset(compare.landfire,compare.landfire$VALUE > 11)

#get landfire dbf
lf.dbf<-read.dbf(dir()[4])$dbf

#merge
compare.landfire<-merge(compare.landfire,lf.dbf,by='VALUE')
 
#error matrices
comp1<-table(compare.landfire$NVCSCLASS,compare.landfire$HabitatTyp)
comp1<-comp1[apply(comp1,1,sum)>0,]


comp2<-table(compare.landfire$NVCSORDER,compare.landfire$HabitatTyp)
comp2<-comp2[apply(comp2,1,sum)>0,]

#crop and plot
e=extent(compare.landfire)
landfire.crop=crop(landfire,e)

#"CF"      "CF/LS"   "DF"      "DT"      "FM"      "GS"      "GS/LS"   "LS"      "LS burn" "LS/DT"   "LS/MF"   "MF"      "TS" 
#habitat types

#simple hab compare
compare.landfire$habSimple='CF'
compare.landfire$habSimple=ifelse(compare.landfire$HabitatTyp %in% c("CF/LS" ,"GS/LS" ,  "LS"   ,   "LS burn", "LS/DT" ,  "LS/MF","TS"),'S',compare.landfire$habSimple)
compare.landfire$habSimple=ifelse(compare.landfire$HabitatTyp %in% c("GS","FM"),'GS',compare.landfire$habSimple)

comp3<-table(compare.landfire$NVCSORDER,compare.landfire$habSimple)
comp3<-comp3[apply(comp3,1,sum)>0,]


#compare with tree
library(rpart)
a<-rpart(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,method='class',data=compare.landfire)
b<-rpart(HabitatTyp~RED+GREEN+BLUE,method='class',data=compare.landfire)

compare.landfire$HabitatTyp<-factor(compare.landfire$HabitatTyp)
rf1<-randomForest(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,data=compare.landfire)
rf2<-randomForest(HabitatTyp~RED+GREEN+BLUE,data=compare.landfire)



#naive comparison
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
b = 100
success.boot = vector()
rows = dim(compare.sub)[1]
for(i in 1:b)
{

	#get sample for validation
	samp=sample(rows,40,replace=FALSE)
	validate=compare.sub[samp,]
	pred.df=subset(validate,select=c('NVCSORDER','NVCSCLASS','NVCSSUBCLA'))
	train=compare.sub[-samp,]
	
	#fit the model
	a<-rpart(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,method='class',data=train)
	pred=predict(a,pred.df,type='class')
	
	success=sum(pred==validate$HabitatTyp)/length(pred)
	success.boot<-c(success.boot,success)
}
	
a<-rpart(HabitatTyp~NVCSORDER+NVCSCLASS+NVCSSUBCLA,method='class',data=compare.sub)
	

	


#get dbf
lc.dbf<-read.dbf( "c:/users/vppatil/desktop/yrb_gis/30class_LC_full.tif.vat.dbf")$dbf
lakepts.lcc<-merge(lakepts.lcc,lc.dbf,by='Value')



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