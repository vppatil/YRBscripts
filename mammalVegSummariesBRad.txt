#need a table of mean pct cover by habtiat type
#### first need to read in the data
library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')

#use mammalpctcover and mammaltraptraits

#need to convert cover classes, selected for trapid, gridid, and vegtypefinal, then take the average across vegtypefinal only, excluding na's

#first get the general datatable

mamm.pct<-sqlQuery(yrb,"select tblmammaltraptraits.vegtypefinal,tblmammaltraptraits.gridid,tblmammalpctcover.* from tblmammaltraptraits right join tblmammalpctcover on tblmammaltraptraits.trapid = tblmammalpctcover.trapid where tblmammaltraptraits.vegtypefinal not like ('DNR') and (tblmammaltraptraits.trapid not like ('1_13_2') or tblmammaltraptraits.trapid not like ('1_26_1'))",stringsAsFactors = FALSE)[,1:19]

#remove weird designations

#now isolate cover vals and convert
mamm.cov<-mamm.pct[,8:19]
mamm.cov[mamm.cov == '0'] = 'N/A'
mamm.cov[mamm.cov == 'n/A'] = 'N/A'
mamm.cov[mamm.cov == 'm'] = 'N/A'
mamm.cov[mamm.cov == 'N/At'] = 'N/A'
mamm.cov[mamm.cov == ''] = 'DNR'
mamm.cov[mamm.cov == '6'] ='5'
mamm.cov[mamm.cov == '7'] ='5'
mamm.cov[mamm.cov == '2-3'] ='3'
mamm.cov[mamm.cov == 'DNR'] = NA



for(i in 1:12)
print(table(mamm.cov[,i]))

x=c('-','t','1','2','3','4','5')
y=c(0,.5,(1+10)/2,35/2,75/2,125/2,175/2)	
library(labdsv)
cover.vals<-vegtrans(mamm.cov,x,y)

mamm.pct[,8:19] = cover.vals

nona.mean<-function(x) mean(na.omit(x))
library(reshape)

mamm.melt<-mamm.pct[,c(1:3,8:19)]


for(i in 3:15)
print(table(mamm.melt[,i]))

rich.melt<-melt(mamm.melt,id.vars=c('gridid','TrapID','vegtypefinal'),variable_name = 'functionalgroup')

avg<-tapply(rich.melt$value,list(rich.melt$vegtypefinal,rich.melt$functionalgroup),nona.mean)


	hab<-sqlQuery(yrb,"select vegtypefinal,avg(sppcount) as richAvg from( select gridid,trapid,vegtypefinal,count(vegspecies) as sppcount from (select distinct VegSpecies,GridID,TrapID,vegtypefinal from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.vegtypefinal from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID where tblmammaltraptraits.vegtypefinal not like ('DNR') and tblmammaltraptraits.trapid not like ('1_13_2') and tblmammaltraptraits.trapid not like ('1_26_1'))) group by vegtypefinal,gridid,trapid) group by vegtypefinal")
	
write.csv(avg,'c:/users/vppatil/desktop/updatedAvgPctCover.csv')
write.csv(hab,'c:/users/vppatil/desktop/updatedAvgRich.csv')