#####based on montana state tutorials
##### trying to find distinct habitat classes using a variety of methods

#### first need to read in the data
library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
old<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')
	
### you will be using the pct cover table, so might as well read it in
pct<-sqlFetch(yrb,'tblVegTransectPctCover',stringsAsFactors=FALSE)
oldpct<-sqlFetch(old,'tblVegTransectPctCover',stringsAsFactors=FALSE)

#can be used to flag 2011 or 2010 lakes
lakes<-sqlQuery(old,"select LakeID from tblLakeBaseInfo where SampleYear = '2010'",stringsAsFactors=FALSE)$LakeID
pct$LakeID<-sapply(pct[,1],spl,2,4)
oldpct$LakeID<-sapply(oldpct[,1],spl,2,4)


###load vegan
library(labdsv)
library(vegan)

#take out
#oldpct<-pct
oldpct<-subset(oldpct,oldpct$LakeID %in% lakes)	

oldpct<-oldpct[!is.na(oldpct[,6]),]
oldpct<-oldpct[oldpct[,6] != 'DNR',]
oldpct<-oldpct[oldpct[,6] != '-1',]
oldpct<-oldpct[oldpct[,9] != '-1',]

oldpct<-subset(oldpct,oldpct$HabitatType %in% c('CF','DF','MF','DT','TS','LS','GS'))
oldpct<-oldpct[!is.na(oldpct$HabitatType),]

covers<-oldpct[,6:13]
habs<-oldpct[,2]
	
#########################################################################################
##start up here again for ord

#covers=pct[,c(6:13,15,16)]
#remove weird designations
covers[covers=='1.5'] = '1'
covers[covers == '0'] = '-'
covers[covers == 'N/A'] = '-'
covers[covers == 'Na'] = '-'
covers[covers == '-+'] = '-'
covers[covers == ''] = '-'
covers[covers == '6'] ='5'
covers[covers == 'sphag 2. nonsphag 1. lichen 1. eq na.'] = '3' 
covers[is.na(covers)]='-'
	
for(i in 1:dim(covers)[2])
		print(table(covers[,i]))
	
x=c('-','t','1','2','3','4','5','DNR')
y=c(0,.5,(1+10)/2,35/2,75/2,125/2,175/2,-99999)	

nona.sum<-function(x) sum(na.omit(x))
cover.vals<-vegtrans(covers,x,y)
	
habs<-habs[apply(cover.vals,1,sum)>0]
cover.vals<-cover.vals[apply(cover.vals,1,sum)>0,]

#do ord of habitat type by functional type
d<-decorana(cover.vals)

habSimple=rep('F',length(habs))
habSimple=ifelse(habs %in% c("CF/LS" ,"GS/LS" ,  "LS"   ,  "TS", "LS burn", "LS/DT" ,  "LS/MF","TS"),'S',habSimple)
habSimple=ifelse(habs %in% c("GS","FM"),'GS',habSimple)

plot(d,display='none',xlim=c(-2,2),ylim=c(-1.5,1.5))
text(d,display='sites',labels=habSimple)


e=envfit(d,data.frame(hab=habSimple),col=c('red','blue','green')
p=envfit(d,cover.vals)
plot(d,display='species')
text(d,display='species')
cca.hab<-cca(cover.vals~habSimple)

#make sure eq is not getting na where it should be dnr.

#try fso
library(vegan)
names=habs
name.df<-data.frame(names)
n.t<-data.frame(cbind(table(row.names(name.df),names)))
n.t[order(as.numeric(row.names(name.df))),]
cov.dist<-vegdist(cover.vals)
a<-fso(~name.df$names,cov.dist)

names=oldpct$HabitatType[row.names(oldpct) %in% row.names(cover.vals)]

clean<-subset(cover.vals[names %in% c('LS','GS','CF','MF','DF','TS'),])
habs<-names[names %in% c('LS','GS','CF','LS','GS','CF','MF','DF','TS')]
simple.f<-ifelse(habs %in% c('CF','MF','DF'),'F',habs)

fs<-subset(cover.vals[names %in% c('CF','MF','DF'),])
f.names=names[names %in% c('CF','MF','DF')]
row.names(fs) = f.names
d3<-decorana(fs)

plot(d3,display='none')
text(d3,display = 'sites',labels=f.names)
envfit(d3,data.frame(hab = f.names))

d2<-decorana(clean)
plot(d2,display='none',xlim=c(-2,2),ylim=c(-1.5,1.5))
text(d2,display='sites',labels = simple.f,xlim=c(-1,1),ylim=c(-1.5,1.5))
e2=envfit(d2,data.frame(hab=habs))

cover.vals.weight<-cover.vals*tem.pct$HabitatTypeLength/100
sph.weight=tapply(cover.vals.weight[,1],tem.pct$LakeID,nona.sum)
mss.weight=tapply(cover.vals.weight[,2],tem.pct$LakeID,nona.sum)
lic.weight=tapply(cover.vals.weight[,3],tem.pct$LakeID,nona.sum)
grm.weight=tapply(cover.vals.weight[,4],tem.pct$LakeID,nona.sum)
frb.weight=tapply(cover.vals.weight[,5],tem.pct$LakeID,nona.sum)
esh.weight=tapply(cover.vals.weight[,6],tem.pct$LakeID,nona.sum)
dsh.weight=tapply(cover.vals.weight[,7],tem.pct$LakeID,nona.sum)
tre.weight=tapply(cover.vals.weight[,8],tem.pct$LakeID,nona.sum)
dw.weight=tapply(cover.vals.weight[,9],tem.pct$LakeID,nona.sum)
bare.weight=tapply(cover.vals.weight[,10],tem.pct$LakeID,nona.sum)

weighted<-data.frame(sph.weight,mss.weight,lic.weight,grm.weight,frb.weight,esh.weight,dsh.weight,tre.weight,eq.weight,dw.weight,bare.weight)
weighted=weighted[apply(weighted,1,sum)>0,]
cover.vals<-cover.vals[!is.na(cover.vals$Gram),]
cover.vals<-na.omit(cover.vals)
cover.vals<-cover.vals[apply(cover.vals,1,sum)>0,]

#now make a distance matrix
cover.dist<-vegdist(cover.vals)

weighted.dist<-vegdist(weighted)

demo.clust<-hclust(weighted.dist,'average')	
ex=lake.class$expansion[match(row.names(weighted),lake.class$LakeID)]
plot(demo.clust,labels=ex)
demo.cut<-cutree(demo.clust,k=2)
demo.cv<-cv.tree
	
#dec
colnames(weighted) = names(tem.pct)[c(6:16)]
colnames(weighted)[9:11] = c('Equiset','DeadWood','Bare')

colnames(weighted)[4] = 'Gram\n\n'
try=t(weighted)

flood.dec<-decorana(weighted)

dec.tiff<-tiff('pctcovDec.tif',height=9,width=9,units='in',res=200)

f.plot<-plot(flood.dec,display='none')
#sub='\n\nDetrended correspondence analysis of lakes grouped by functional type percent cover\nBlack circles are lakes, red crosses are functional types, and blue text shows the centroids of groupings based on flooding history.\nFunctional type abundance was significantly associated with flood history (r^2 = 0.41,p=0.04)'

#text(f.plot,display='species',type='points',col='red')
flood.class=lake.class[match(row.names(weighted),lake.class$LakeID),]
flood.class$flood='High Flood'
flood.class$flood=ifelse(flood.class$expansion!='Flood','Low Flood',flood.class$flood)
flood.class$flood=factor(flood.class$flood)

text(flood.dec,display='sites',labels=flood.class$flood,cex=1.5)
f.fit=envfit(f.plot,data.frame(weighted))
plot(f.fit)
dev.off()

f.plot
f.fit<-envfit(f.plot,data.frame(lakeHist = lake.class$flood,)
f.fit2<-envfit(f.plot(flood.class$expansion))

plot(f.fit)

text(
covers.meta=oldpct[row.names(oldpct) %in% row.names(cover.vals),]


demo.cut2<-cutree(demo.clust,k=3)
attach(covers.meta)

table(HabitatType,demo.cut)
table(HabitatType,demo.cut2)		


#load code from vegmap compare- need to merge cover vals with compare.pts shapefile.
library(RODBC)
library(shapefiles)
library(sp)
library(rgdal)
library(raster)
library(proj4)
library(maptools)
proj4.string<-"+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
compare.pts<-readShapePoints('c:/users/vppatil/desktop/yrb_gis/compare',proj4string=CRS(proj4.string))

#find the rows in cover.vals that belong to the transectID's in compare.pts
#do row by row for each row in compare.pts
#oldpct is raw datasheet
#compare.pts is shapefile
#compare.cover is cover class data from oldpct
#compare.vals is transformed cover classes

oldpct$HabitatType=as.character(oldpct$HabitatType)
oldpct$TransectID=as.character(oldpct$TransectID)
oldpct$habTrans=paste(oldpct$HabitatType,oldpct$TransectID,sep='_')

compare.pts$HabitatTyp=as.character(compare.pts$HabitatTyp)
compare.pts$TransectID=as.character(compare.pts$TransectID)
compare.pts$habTrans=paste(compare.pts$HabitatTyp,compare.pts$TransectID,sep='_')

compare.cover=vector()
name=vector()
for (i in 1:length(compare.pts$TransectID))
{
	temp=covers[compare.pts$habTrans[i]==oldpct$habTrans,]
	temp2=oldpct[compare.pts$habTrans[i]==oldpct$habTrans,]
	name=rbind(name,temp2)
	compare.cover=rbind(compare.cover,temp)
}

name=name[!is.na(name$EG_Shrub),]
compare.cover<-name[,6:13]#extract cover vals
#now the row names for what's left of old pct (now called name)
#and compare cover (the cover vals) should sync up.

#next you need to convert the cover classes
#finally, you need to merge compare.covers with compare.pts

compare.cover[compare.cover=='1.5'] = '1'
compare.cover[compare.cover == '0'] = '-'
compare.cover[compare.cover == 'N/A'] = '-'
compare.cover[compare.cover == ''] = '-'
	
x=c('-','t','1','2','3','4','5')
y=c(0,.5,(1+10)/2,35/2,75/2,125/2,175/2)

nona.sum<-function(x) sum(na.omit(x))
compare.vals<-vegtrans(compare.cover,x,y)
compare.vals<-compare.vals[apply(compare.vals,1,sum)>0,]
compare.vals<-compare.vals[!is.na(compare.vals[,1]),]
compare.vals$habTrans=name$habTrans	

#now you need to try the overlay, and merge in the landfire classifications
#then the ordination can occur.

setwd('c:/users/vppatil/desktop/yrb_gis/landfire/')
filename=dir()[2]
landfire<-raster(filename)
#landfire.proj<-projectRaster(landfire,crs=proj4.string)

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

#finally, you should be ready to merge compare.vals and compare.pts
#have to do this after the landfire overlay.
d<-compare.landfire@data
<-merge(compare.landfire@data,compare.vals,by='habTrans')

library(vegan)
compare.vals<-compare.vals[,-9]


name$NVCSORDER=compare.landfire$NVCSORDER[match(name$habTrans,compare.landfire$habTrans)]
name$NVCSCLASS=compare.landfire$NVCSCLASS[match(name$habTrans,compare.landfire$habTrans)]
name$NVCSSUBCLA=compare.landfire$NVCSSUBCLA[match(name$habTrans,compare.landfire$habTrans)]


compare.vals=compare.vals[!is.na(name$NVCSORDER),]
name=name[!is.na(name$NVCSORDER),]
b<-decorana(compare.vals)

n=data.frame(n=name$NVCSORDER,n2=name$NVCSCLASS,n3=name$NVCSSUBCLA)

n$n=factor(n$n)
n$n2=factor(n$n2)
n$n3=factor(n$n3)
e<-envfit(b,n)

name$NVCSORDER
##still messed up.



