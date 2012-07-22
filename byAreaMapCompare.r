library(RODBC)

#check
#sqlQuery(copy,"select TransectID,VegEndEasting,VegEndNorthing from tblTransects where TransectID like '%0_32_1'") 
	

#next 2 odbc connections are created) or TransectID like ( called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

#get average habitat type length for each habitat and lakeID from tblVegTransectMetadata

t=sqlQuery(yrb,"select LakeID,TransectID,VegStartEasting,VegStartNorthing,VegEndEasting,VegEndNorthing from tblTransects where VegEndEasting is not Null and VegStartEasting > 0 and VegStartNorthing > 0 and VegEndNorthing > 0 and VegEndEasting > 0")
m=sqlQuery(yrb,"select TransectID,HabitatType,HabitatTypeLength from tblVegTransectPctCover where HabitatTypeLength > 0 ")

hab=base::merge(t,m,by="TransectID")

means<-tapply(hab$HabitatTypeLength,list(hab$HabitatType,hab$LakeID),mean)

query="select LakeID,HabitatType,avg(HabitatTypeLength) from (select tblTransects.LakeID,tblVegTransectPctCover.* from tblTransects inner join tblVegTransectPctCover on tblTransects.TransectID = tblVegTransectPctCover.TransectID) group by LakeID,HabitatType"

a<-sqlQuery(yrb,query)

