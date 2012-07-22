#try species area curves for plant data
#need lake areas- proper estimates. come from arcgis?

setwd('c:/users/vppatil/desktop/yrb')
library(RODBC)

yrb<-odbcConnectAccess2007('YRBiodiversity.accdb')

setwd('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/')
lakeareas<-read.csv('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/lake_areas.csv',header=TRUE)

#include observer bias in nls fit somehow?

#some outliers present- need decision criteria for how to remove those.

#need to merge lakeareas, richness summary by lakeid

#get richness summaries, ignoring unknowns for now
#would need to join richness with spp list table to get names anyway
#use the following syntax to do that
weirdnames<-sqlQuery(yrb,"select tblVegTransectSppList.*,tblVegSpeciesList.ScientificName from tblVegTransectSppList inner join tblVegSpeciesList on tblVegTransectSppList.Species = tblVegSpeciesList.VegSpID")


#right now, unknown names are gone.
#still need to be merged in from the original data in the 2011 table

#species counts by transect
rich.query="select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblTransects.LakeID,tblVegTransectSppList.* from tblTransects inner join select tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID)) group by LakeID,TransectID"

rich.query<-"select TransectID,count(Species) as sppcount from ( select distinct TransectID,Species from tblVegTransectSppList)group by TransectID"

#avg across transects
rich.query2="select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblTransects.LakeID,tblVegTransectSppList.* from tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID)) group by LakeID,TransectID) group by LakeID "

#richness for gs species only
gs.rich<-"select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblTransects.LakeID,tblVegTransectSppList.* from tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID where tblVegTransectSppList.GS = 1)) group by LakeID,TransectID) group by LakeID "

#richness for gs species only
gs.rich<-"select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblVegTransectMetadata.Observer1,tblTransects.LakeID,tblVegTransectSppList.* from (tblVegTransectMetadata inner join tblTransects on tblVegTransectMetadata.TransectID = tblTransects.TransectID) inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID where tblVegTransectSppList.GS = 1)) group by LakeID,TransectID) group by LakeID "

#for ls/ts only

shrub.rich<-"select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblVegTransectMetadata.Observer1,tblTransects.LakeID,tblVegTransectSppList.* from (tblVegTransectMetadata inner join tblTransects on tblVegTransectMetadata.TransectID = tblTransects.TransectID) inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID where tblVegTransectSppList.LS = 1 OR tblVegTransectSppList.TS = 1)) group by LakeID,TransectID) group by LakeID "

#Selecting specific observers
connorrich<-"select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblVegTransectMetadata.Observer1,tblTransects.LakeID,tblVegTransectSppList.* from tblVegTransectMetadata inner join( tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID) on tblVegTransectMetadata.TransectID = tblTransects.TransectID  where Observer1 like '%Connor%')) group by LakeID,TransectID) group by LakeID "

vijrich<-"select LakeID,avg(sppcount) as avgRich from (select LakeID,TransectID,count(Species) as sppcount from (select distinct LakeID,TransectID,Species from (select tblVegTransectMetadata.Observer1,tblTransects.LakeID,tblVegTransectSppList.* from tblVegTransectMetadata inner join( tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID) on tblVegTransectMetadata.TransectID = tblTransects.TransectID  where Observer1 like '%Vijay%')) group by LakeID,TransectID) group by LakeID "


connor.richness<-sqlQuery(yrb,connorrich)
vij.richness<-sqlQuery(yrb,vijrich)

rich<-sqlQuery(yrb,rich.query2) ##done-
gs.richdat<-sqlQuery(yrb,gs.rich)
shrub.rich<-sqlQuery(yrb,shrub.rich)

#now merge with lake areas from lake table
rich<-merge(rich,lakeareas,by='LakeID')
gs.rich<-merge(gs.richdat,lakeareas,by='LakeID')
shrub.rich<-merge(shrub.rich,lakeareas,by = 'LakeID')
connor.richness<-merge(connor.richness,lakeareas,by='LakeID')
vij.richness<-merge(vij.richness,lakeareas,by = 'LakeID')

plot(connor.richness$area_ha,connor.richness$avgRich,ylab = 'richness',xlab = 'area ha',main = 'Connor lake SAR curves for Veg Transects')
plot(vij.richness$area_ha,vij.richness$avgRich,ylab = 'richness',xlab = 'area ha',main = 'Vijay lake SAR curves for VegTransects')

plot(shrub.rich$area_ha,shrub.rich$avgRich,ylab = 'richness',xlab = 'area ha',main = 'lakeSAR curves for shrub habitat on transects')
plot(gs.rich$area_ha,gs.rich$avgRich,ylab = 'richness',xlab = 'area ha',main = 'lakeSAR curves for gs habitat on transects')

plot(rich$area_ha,rich$avgRich)
#averages look way too low when you parse out individual observers.
#something wonky going on?


#remove 0
rem0<-function(df)
{
	return(subset(df,df$avgRich > 0 & df$area_ha > 0))
}
rich<-rem0(rich)
gs.rich<-rem0(gs.rich)

connor.rich<-rem0(connor.richness)
vij.richness<-rem0(vij.richness)

create.mmsardf<-function(df,nm)
{
	df<-subset(df,select = c('area_ha','avgRich'))
	names(df)<-c('a','s')
	df<-list(name = nm,data=df)
	return(df)
}

rich.forSAR<-create.mmsardf(rich,'allplants')
gs.rich.SAR<-create.mmsardf(gs.rich,'gs')
connor.mmSAR<-create.mmsardf(connor.rich,'connor')
vij.mmSAR<-create.mmsardf(vij.richness,'vij')

library(mmSAR)
#creating a vector of model names
mods <- c("power","expo","negexpo","monod","logist","ratio","lomolino","weibull")

data(power)
data(expo)
data(negexpo)
data(monod)
data(logist)
data(ratio)
data(lomolino)
data(weibull)

#fit several, figure out how to plot
pow.fit<-rssoptim(power,rich.forSAR)
log.fit<-rssoptim(expo,rich.forSAR)
negexpo.fit<-rssoptim(negexpo,rich.forSAR)
monod.fit<-rssoptim(monod,gs.rich.SAR)

veg.multi<-multiSAR(c('power','expo','negexpo','logist','monod'),rich.forSAR)

gs.mutli<-multiSAR(c('power','expo','negexpo','logist','monod'),gs.rich.SAR)

connor.multi<-multiSAR(c('power','expo','negexpo'),connor.mmSAR)
vij.multi<-multiSAR(c('power','expo','negexpo','logist','monod'),vij.mmSAR)


allplants.ne<-rssoptim(negexpo,rich.forSAR)
all.coef<-allplants.ne$par
gs.ne<-rssoptim(negexpo,gs.rich.SAR)
gs.coef<-gs.ne$par


connor.ne<-rssoptim(negexpo,connor.mmSAR)
connor.coef<-connor.ne$par
vij.ne<-rssoptim(negexpo,vij.mmSAR)
vij.coef<-vij.ne$par

#functions for creating points using each of the two models
exp.plot<-function(a,b,x)
	{y = a * log(x) +b
		return(y)}
		
negexp.plot<-function(a,b,x)
{
	y = a * (1- exp(-b * x))
	return(y)
}

x = 0:400

#demo plots with coefs extracted from sar curve models
plot(vij.ne$data,xlab = 'lake area (ha)',ylab = 'number of species',main = 'lakes surveyed by Vijay Patil')
lines(negexp.plot(vij.coef[1],vij.coef[2],x),lty =2,col = 'red')

plot(connor.ne$data,xlab = 'lake area (ha)',ylab = 'number of species',main = 'lakes surveyed by Connor Jandreau')
lines(negexp.plot(connor.coef[1],connor.coef[2],x),lty=2,col='red')

plot(gs.rich$area_ha,gs.rich$avgRich,ylab = 'richness',xlab = 'area ha',main = 'lakeSAR curves for gs habitat on transects')
lines(negexp.plot(gs.coef[1],gs.coef[2],x),lty=2,col='red')

plot(rich$area_ha,rich$avgRich,ylab = 'richness',xlab = 'area ha',main = 'lakeSAR curve for total plant richness \naveraged across transects per lake')
lines(negexp.plot(all.coef[1],all.coef[2],x),lty=2,col='red')

#now to mess with habitat width vs. num species per habitat
#need to modify rich.query to join with veg pct cover table by transectid

#for gs only right now
gs.habrich.query="select HabitatType,HabitatTypeLength,LakeID,TransectID,count(Species) as sppcount from (select distinct HabitatType,HabitatTypeLength,LakeID,TransectID,Species from (select tblTransects.LakeID,tblVegTransectSppList.*,tblVegTransectPctCover.HabitatType,tblVegTransectPctCover.HabitatTypeLength from tblVegTransectPctCover inner join (tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID) on tblVegTransectPctCover.TransectID = tblTransects.TransectID) where HabitatType = 'GS') group by LakeID,TransectID,HabitatType,HabitatTypeLength"

gs.habrich<-sqlQuery(yrb,gs.habrich.query)

plot(gs.habrich$HabitatTypeLength,gs.habrich$sppcount)

weird<-subset(gs.habrich,gs.habrich$sppcount > 40)