

setwd('~/Dropbox/AlaskaFiles/transferWorkspace')
load('transferWSpace.RData')
tablenames<-vector()
for(i in 1:dim(summary(yrb2011.list))[1])
{
	tablenames[i]<-sapply(yrb2011.list[[i]][1],c)
}

tblVegTransectMetadata<-data.frame(yrb2011.list[[5]][2],stringsAsFactors=FALSE)
names(tblVegTransectMetadata)
tblVegTransectPctCover<-data.frame(yrb2011.list[[6]][2],stringsAsFactors=FALSE)
names(tblVegTransectPctCover)[18]='PKey'
tblVegTransectSppList<-data.frame(yrb2011.list[[7]][2],stringsAsFactors=FALSE)

SoilMoisture<-data.frame(yrb2011.list[[2]][2],stringsAsFactors=FALSE)
LakeVegHabitatSummaries<-data.frame(yrb2011.list[[1]][2],stringsAsFactors=FALSE)
tblLakeBaseInfo<-data.frame(yrb2011.list[[3]][2],stringsAsFactors=FALSE)
tblTransects<-data.frame(yrb2011.list[[4]][2],stringsAsFactors=FALSE)

md.names<-paste(names(tblVegTransectMetadata),collapse=',')

spplist.names<-paste(names(tblVegTransectSppList),collapse=',')

library(MySQL)

yrb2011<-dbConnect(MySQL(),user='root',password='!nuksh00K',host='localhost',dbname='yrb2011')
dbListTables(yrb2011)

drop database yrb2011;
create database yrb2011; 

dbWriteTable(yrb2011,"tblLakeBaseInfo",tblLakeBaseInfo);
dbWriteTable(yrb2011,"SoilMoisture",SoilMoisture);
ldbWriteTable(yrb2011,"tblVegTransectMetadata",tblVegTransectMetadata);
dbWriteTable(yrb2011,"tblVegTransectPctCover",tblVegTransectPctCover);
dbWriteTable(yrb2011,"LakeVegHabitatSummaries",LakeVegHabitatSummaries);
dbWriteTable(yrb2011,"tblTransects",tblTransects);
dbWriteTable(yrb2011,"tblVegTransectSppList",tblVegTransectSppList);








