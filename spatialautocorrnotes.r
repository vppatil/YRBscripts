	#notes/analyses for spatial autocorrelation

#detecting autocorr with mantel test - semiparametric, moran's i- parametric
#give positive or negative spatial autocorr values with p values

#using package ape
install.packages('ape')
library(ape)
library(shapefiles)
library(sp)

#for variogram
install.packages('geoR')
library(geoR)
#for data, you need an id field, coords x,y (lat lon), and a response variable (or more than one?)

setwd('c:/users/vppatil/desktop/yrb_gis')

lakefile<-dir(pattern = 'Polygons.dbf')
lakepoly<-read.dbf(lakefile)$dbf

library(RODBC)
yrb=odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')
lakes<-sqlFetch(yrb,'tblLakeBaseInfo')


#avg spp richness across transects
rich.query2="select tblLakeBaseInfo.*,rich.avgRich from tblLakeBaseInfo inner join (select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblTransects.LakeID,tblVegTransectSppList.* from tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID)) group by LakeID,TransectID) group by LakeID) as rich on tblLakeBaseInfo.LakeID = rich.LakeID"

#some lakes are missing, some are okay

lake.data<-sqlQuery(yrb,rich.query2)

#all lake coords
lakefordist<-sqlQuery(yrb,'select LakeID,Record_Easting,Record_Northing from tblLakeBaseInfo')

#remove lakes without area estimates
lake.data<-subset(lake.data,lake.data$AreaActual > 0)

#make proportional dif
lake.data$propdif<-(lake.data$AreaActual-lake.data$AreaNHD)/lake.data$AreaNHD
plot(lake.data$AreaActual,lake.data$avgRich)
plot(lake.data$propdif,lake.data$avgRich,xlim = c(-2,2))
plot(lake.data$SVRatio,lake.data$avgRich,xlim=c(0,5))



#next you generate a distance matrix using the lat and lon fields
#as.matrix(dist(cbind(lat,lon)))
dist.mat<-as.matrix(dist(cbind(lake.data$Record_Easting,lake.data$Record_Northing)))

#look into spdep

#now make inverse matrix
#inv<-1/mat
dist.inv<-1/dist.mat

#replace diagonal with 0
#diag(inv) = 0
diag(dist.inv) = 0

#now for moran's i- use function moran.i(dat$response,inv)
Moran.I(lake.data$avgRich,dist.inv)


#significant result means that there IS sig. spatial autocorrelation
#highly significant spatial autocorrelation in spp richness

#can also bin distances- so that they are less than 1.5 km for example, to examine spatial autocorr at two sampling scales
#this would use the distance matrix, but you would have to be careful about units
#ozone.dists.bin <- (ozone.dists > 0 & ozone.dists <= .75)
#Moran.I(ozone$Av8top, ozone.dists.bin)

lake.dists.bin<-(dist.mat > 5000)
bin.inv<-1/lake.dists.bin

Moran.I(lake.data$avgRich,lake.dists.bin)
#so it is still highly significant when you separate by lake cluster

#########################################VARIOGRAM######################################################
#for visualizing spatialautocorr

#first create a vector of dist intervals using seq()
dist.seq=seq(0,max(dist.mat),l = 10)

#next use the variog() function, where coords = coord df, data = response, breaks = breaks
coord.df<-subset(lake.data,select=c('Record_Easting','Record_Northing'))
dat.df<-subset(lake.data,select = c('avgRich'))

rich.var<-variog(coords=coord.df,data=dat.df,breaks=dist.seq)
plot(rich.var,type='b')

var.summary<-cbind(dist.seq[2:10],rich.var$v,rich.var$n)

#those are the arguments
#response return includes $v (semi-variance), $n (number pairs)

#variogram plot- plot(variog.name,type='b') #don't know what the other types are

#doesn't change much over large differences- would be difficult to plot.

################################################################################################
#regression with autocorrelation

#try linear for species richness
library(nlme)
lake.data$dummy<-1

rich.model<-lme(fixed=avgRich~AreaActual,data=lake.data,random = ~1|dummy,method = 'ML')




#############################kriging#############################################################

#spatial interpolation- requires knowing the model for a semi-variogram (exp, spherical, etc)
#seems best done in ArcGIS

#get a layer with response data and lakeID, merge it with a centroids layer, export to arcMAP
#use the kriging tool

#need to apply spatial autocorr to richness, community,pct cover data
