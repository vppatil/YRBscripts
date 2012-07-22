	library(RODBC)
	yrb2011<-odbcConnectAccess2007('c:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
	yrb<-odbcConnectAccess2007('c:/users/vijay/desktop/yrb/YRBiodiversity.accdb')

	a<-sqlQuery(yrb2011,"select * from tblVegTransectSppList where LakeID = '1_13_3'")
	b<-sqlQuery(yrb,"select * from tblVegTransectSppList where TransectID = '%1_13_3'")
	
	sqlQuery(yrb2011,"select Species,LakeID from tblVegTransectSppList")


	sqlQuery(yrb2011,"select TransectID from tblVegTransectMetadata where TransectID like '%0_10_1'")
	
	sqlQuery(yrb2011,"select TransectID,LakeID from tblTransects where LakeID = '0_35_1'") 
	sqlQuery(yrb2011,"select LakeID,Bearing,Species from tblVegTransectSppList where LakeID = '1_58_1'") 
	sqlQuery(yrb2011,"select distinct(Bearing),LakeID from tblVegTransectSppList where LakeID = '1_58_1'") 
	
names(sqlFetch(yrb2011,"tblVegTransectSppList"))
	
South<-sqlQuery(yrb2011,"select count(Species) as SppCount from (select distinct(Species),LakeID from tblVegTransectSppList) where LakeID like '0_%' group by LakeID ")
North<-sqlQuery(yrb2011,"select count(Species) as SppCount from (select distinct(Species),LakeID from tblVegTransectSppList) where LakeID like '1_%' group by LakeID ")


sqlQuery(yrb2011,"select count(Species) as sppCount,LakeID from (select distinct(Species),LakeID from tblVegTransectSppList group by LakeID)")
GScount<-sqlQuery(yrb2011,"select count(Species) as GScount from (select  distinct(Species) from tblVegTransectSppList where GS in ('TRUE','x'))")

LScount<-sqlQuery(yrb2011,"select count(Species) as LScount from (select  distinct(Species) from tblVegTransectSppList where LS in ('TRUE','x'))")

CFcount<-sqlQuery(yrb2011,"select count(Species) as CFcount from (select  distinct(Species) from tblVegTransectSppList where CF in ('TRUE','x'))")

FMcount<-sqlQuery(yrb2011,"select count(Species) as FMcount from (select  distinct(Species) from tblVegTransectSppList where FM in ('TRUE','x'))")

names.meta<-names(sqlFetch(yrb2011,"tblVegTransectMetadata")
)

max.fm<-sqlQuery(yrb2011,"select max(tblVegTransectMetadata.FM) as fmMax,tblTransects.LakeID from tblVegTransectMetadata Inner Join tblTransects on tblVegTransectMetadata.TransectID = tblTransects.TransectID"
	
	sqlQuery
		
	#extract all tables from database to a dataframe list
	a<-sqlTables(yrb2011)
	a<-a$TABLE_NAME[a$TABLE_TYPE == 'TABLE']
	yrb2011.list<-list()
	for(i in 1:length(a))
	{
		yrb2011.list[[i]]<-list(a[i],sqlFetch(yrb2011,a[i]))
	}
	save.image("C:\\Users\\Vijay\\Documents\\My Dropbox\\AlaskaFiles\\transferWorkspace\\transferWSpace.RData")

	yq<-function(LakeID,Bearing)
	{
		q<-paste("select Species,GS,LS,TS,DF,MF,CF,FM,EM,MU,AQ from tblVegTransectSppList where LakeID in ('",LakeID,"') and Bearing in ('",Bearing,"')",sep='')
		sqlQuery(yrb2011,q)
	}