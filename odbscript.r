/*
Demo code to fuck with open office dbase
*/

create table tblTransects(rownames varchar(100),LakeId varchar(20),TransectID varchar(30),Azimuth varchar(20),LakeEdgeEasting int,LakeEdgeNorthing int,EdgeElevation int,VegStartEasting int, VegStartNorthing int,VegEndEasting int,VegEndNorthing int, VegBearingCenter int);

create table tblLakeBaseInfo(rownames varchar(100),Stratum varchar(20),FocalNum varchar(30), SiteNum varchar(20),LakeID varchar(20),AlternateName varchar(100),Team varchar(100),Record_Easting int,Record_Northing int,Record_Longitude int,Record_Latitude int, SampleYear int,SampleSession int,AreaJuly09 int, AreaNHD int,SVRatio int,LakeIDNumber int);

create table SoilMoisture(ID int not null auto_increment,primary key(ID),TransectID varchar(30),Stratum varchar(20),LakeID varchar(20),CommunityType varchar(100),Layer varchar(100),FrozenTotalWeight int, AirDryAll int, AirDryAllSub int,WTareFroze int,WTareAirDried int,OvenDry1 int,OvenDry2 int,AirDryStart datetime,OvenDryStart datetime,TimeWeigh1 datetime,TimeWeigh2 datetime,OvenTemp int
,NotesFroze varchar(100),NotesAirDry varchar(100),NotesOven varchar(100));

create table tblLakeHabitatSummaries(rownames varchar(100),TransectID varchar(20),LakeID varchar(20),AreaJuly09 int,AreaNHD int,Record_Easting int,Record_Northing int,habnums int, shannon int, AreaDif int, PropDif int);

create table tblVegTransectPctCover(TransectID varchar(50),HabitatType varchar (100),Start int,End int,HabitatTypeLength int,Sphag char (1),Non_Sphag char(1),Lichen char(1),Gram char(1),Forb char(1),EG_Shrub char(1),Dec_Shrub char(1),Tree char(1),Eq char(1),DW char(1),Bare char(1),Notes varchar(100),PKey int not null auto_increment,primary key(PKey));

create table tblVegTransectMetadata(TransectID varchar(100),Observer1 varchar(100),Observer2 varchar(100),TransectDate datetime,TransectTime varchar(100),PF_GS int,FIB_GS int,HUM_GS int,PF_SH int,FIB_SH int,HUM_SH int,PF_FOR int,FIB_FOR int,HUM_FOR int,PF_DEPTH_L int,FIB_DEPTH_L int,HUM_DEPTH_L int,PF_DEPTH_FE int,FIB_DEPTH_FE int,HUM_DEPTH_FE int,FM varchar(100),EM varchar(100),MU varchar(100),PestsPathogens varchar(100),GrowthAnomalies varchar(100),Phenology varchar(100),AnimalUse varchar(100),Disturbance varchar(100),Drawings varchar(100),PF_Peat_Notes varchar(100),Notes1 varchar(100),gthan varchar(100),lthan varchar(100));

create table tblVegTransectSppList(Bearing varchar(50),LakeID varchar(50),Notes varchar(200),Species varchar(100),GS varchar(50),LS varchar(50),TS varchar(50),DT varchar(50),DF varchar(50),MF varchar(50),CF varchar(50),AQ varchar(50),FM varchar(50),MU varchar(50),EM varchar(50),Other varchar(50),TransectSppAutonumber int);



setwd('~/Dropbox/AlaskaFiles/transferWorkspace')
load('transferWSpace.RData')
tablenames<-vector()
for(i in 1:dim(summary(yrb2011.list))[1])
{
	tablenames[i]<-sapply(yrb2011.list[[i]][1],c)
}

tblVegTransectMetadata<-data.frame(yrb2011.list[[5]][2])
names(tblVegTransectMetadata)
tblVegTransectPctCover<-data.frame(yrb2011.list[[6]][2])
names(tblVegTransectPctCover)[18]='PKey'
tblVegTransectSppList<-data.frame(yrb2011.list[[7]][2])

SoilMoisture<-data.frame(yrb2011.list[[2]][2])
LakeVegHabitatSummaries<-data.frame(yrb2011.list[[1]][2])
tblLakeBaseInfo<-data.frame(yrb2011.list[[3]][2])
SoilMoisture<-data.frame(yrb2011.list[[4]][2])

md.names<-paste(names(tblVegTransectMetadata),collapse=',')

spplist.names<-paste(names(tblVegTransectSppList),collapse=',')

