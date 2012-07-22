#1) identify all lakes for which you have slope estimates
#2) include slope and cv estimates as well as lake size, proportional difference as predictor variables
#3) lake pH as well?
#4) get spp list for these


#first load the rodbc package
library(RODBC)

#next 2 odbc connections are created) or TransectID like ( called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
yrb2011<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
Lakes<-sqlQuery(yrb2011,"select distinct LakeID from tblTransects")
#oldLakes<-sqlQuery(yrb,"select distinct LakeID from tblTransects")
#lakes<-unique(rbind(Lakes,oldLakes))

names(sqlFetch(yrb,'tblVegTransectSppList'))
names(sqlFetch(yrb,'tblVegTransectMetadata'))

Lakes<-sqlQuery(yrb2011,"select Bearing,LakeID from tblVegTransectSppList")
dates<-sqlQuery(yrb2011,"select TransectID,TransectDate from tblVegTransectMetadata")
dates=subset(dates,dates$TransectDate > as.POSIXct("2011-01-01"))
Lakes$TransectID=paste(Lakes$Bearing,Lakes$LakeID,sep='_')
Lakes=subset(Lakes,Lakes$TransectID %in% dates$TransectID)

lake.list<-unique(Lakes$LakeID)
setwd('c:/users/vppatil/desktop/yrb_gis/')
vijslopes<-read.csv('C:/users/vppatil/desktop/YRB_GIS/vijslopes_finaljoin.dbf')
vijest<-read.csv('C:/users/vppatil/dropbox/alaskafiles/rscripts/lake slope files/vijslopes.csv')
vijslopes<-read.csv('C:/users/vppatil/dropbox/alaskafiles/rscripts/lake slope files/vij_estimates_simple.csv')
vijest<-subset(vijest,select=c('OBJECTID','siteID'))
vijest<-subset(vijest,vijest$siteID !='')
names(vijest='id')
vijslopes<-merge(vijslopes,vijest,by='id')
write.csv(vijslopes,'c:/users/vppatil/desktop/vijslopes.csv')

lakes<-sqlQuery(yrb2011,"select * from tblVegTransectSppList")
comp<-read.csv('c:/users/vppatil/dropbox/alaskafiles/rscripts/lake slope files/slopes for compare.csv')
sqlTables(yrb2011)
#comp<-merge(comp,lakes,by='LakeID')
base<-sqlFetch(yrb,'tblLakeBaseInfo')
comp<-merge(comp,base,by='LakeID')
comp=comp[-4,]
row.names(comp)=comp$LakeID
	compvars=subset(comp,select=c('AreaJuly09','SVRatio','slope','cv'))

	
comp<-merge(comp,lakes,by='LakeID')
comp.lakes<-subset(lakes,lakes$LakeID %in% comp$LakeID)
comp.table<-table(comp$LakeID,comp$Species)
comp.table<-ifelse(comp.table >1,1,comp.table)
library(vegan)
comp.dist<-vegdist(comp.table)
comp.dec<-decorana(comp.dist)
cp<-plot(comp.dec,display='sites')
envfit(cp,compvars,na.rm=TRUE)

comp.lakes<-subset(lakes,lakes$LakeID %in% comp$LakeID)
comp.table<-table(comp$LakeID,comp$Species)
comp.table<-ifelse(comp.table >1,1,comp.table)
library(vegan)
comp.dist<-vegdist(comp.table)
comp.dec<-decorana(comp.dist)
cp<-plot(comp.dec,display='sites')
envfit(cp,comp)

bigspp<-sqlQuery(yrb2011,'Select * from tblVegTransectSppList')
bigspp.table<-table(bigspp$LakeID,bigspp$Species)
bigspp.table<-ifelse(bigspp.table>1,1,bigspp.table)
bigspp.dec<-decorana(bigspp.table)
bigvars<-subset(bigspp,select=c('GS','LS','TS','CF','EM','MU'))
bigplot<-plot(bigspp.dec)
a<-envfit(bigplot,a)





