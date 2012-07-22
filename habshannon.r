library(RODBC)

old<-odbcConnectAccess2007('C:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
new<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
	
#put the lake info table into the new dbase
lake.info<-sqlFetch(old,'tblLakeBaseInfo')
tblTransects<-sqlFetch(old,'tblTransects')

sqlSave(new,lake.info,'tblLakeBaseInfo')
sqlSave(new,tblTransects,'tblTransects')

#for lake area query, table is tblLakeBaseInfo, col is AreaJuly09 and AreaNHD. 
# for coords, also use Record_Easting, Record_Northing
	q1<-"SELECT LakeID,AreaJuly09,AreaNHD,Record_Easting,Record_Northing from tblLakeBaseInfo where LakeID in (select LakeID from tblTransects)"
	q2<-"SELECT TransectID,LakeID from tblTransects where (LakeID in (select LakeID from tblLakeBaseInfo) and TransectID in (select TransectID from tblVegTransectMetadata))"

	old.lakeq<-sqlQuery(old,q1)
	old.tq<-sqlQuery(old,q2)

	old.complete<-merge(old.lakeq,old.tq,by='LakeID')

	new.lakeq<-sqlQuery(new,q1)
	new.tq<-sqlQuery(new,q2)

	new.complete<-merge(new.lakeq,new.tq,by='LakeID')

all.complete<-rbind(old.complete,new.complete)
all.lakeq<-rbind(new.lakeq,old.lakeq)[1:130,]


#the next section calculates habitat type numbers and shannon diversity indices
#get the two PctCover tables
old.pctcov<-sqlFetch(old,'tblVegTransectPctCover')
new.pctcov<-sqlFetch(new,'tblVegTransectPctCover')

hab.func<-function(df)
{
	df<-subset(df,select=c('TransectID','HabitatType','HabitatTypeLength','Start','End'))
	return(df)
}

old<-hab.func(old.pctcov)
new<-hab.func(new.pctcov)

hab.all<-rbind(old,new)

bad<-which(is.na(hab.all$HabitatTypeLength))
hab.all$End[hab.all$TransectID=='277_1_28_2'&hab.all$HabitatType=='MF']=100
hab.all$HabitatTypeLength[bad]=hab.all$End[bad]-hab.all$Start[bad]
hab.all<-subset(hab.all,is.na(hab.all$HabitatType)==FALSE)

hab.all<-subset(hab.all,hab.all$HabitatTypeLength>0) #removes aquatics from this calculation
#fm widths need to come from metadata
sqlSave(new,hab.all,'habAlltemp')

t<-table(hab.all$TransectID,hab.all$HabitatType)
hab.nums<-apply(t,1,sum)

transects<-row.names(cbind(hab.nums))
hab.df<-data.frame(TransectID=transects,hab.nums)

hab.list<-list()
for (i in 1:length(transects))
{
	hab.list[[i]]=subset(hab.all,hab.all$TransectID==transects[i])
}

num.func<-function(hab)
{
	num<-dim(hab)[1]
}

hab.nums<-sapply(hab.list,num.func)

#now you need a 
#shannon function
#requires a vector of the lengths for each habitat  (n sub i)
#will have to be applied to each lake
Shannon.func<-function(hab.df)
{
	length.vector<-hab.df$HabitatTypeLength
	pi.vec<-length.vector/sum(length.vector)
	shannon<-sum(pi.vec*log(pi.vec))*-1
	return(shannon)
}
shannon<-sapply(hab.list,Shannon.func)

#list of all lakes, plus the number of habitat zones and their shannon diversity indices
hab.df<-data.frame(transects,hab.nums,shannon)
names(hab.df)[1]='TransectID'

for.plot<-merge(all.complete,hab.df,by='TransectID')
for.plot$AreaDif<-for.plot$AreaJuly09-for.plot$AreaNHD
for.plot$PropDif<-for.plot$AreaDif/for.plot$AreaNHD

#export for.plot to database
sqlSave(new,for.plot,'LakeVegHabitatSummaries')

#How to deal with infinity values?
#remove temporarily

for.plot<-subset(for.plot,for.plot$PropDif<100)


#A number of transects are missing. not clear why.

plot(for.plot$PropDif,for.plot$hab.nums)
plot(for.plot$AreaDif,for.plot$hab.nums)
plot(for.plot$PropDif,for.plot$shannon)


growing.lakes<-unique(for.plot$LakeID[for.plot$PropDif>0])
growing.lakes

names(for.plot)

#tapplys aren't working for some reason.
s.mean<-tapply(for.plot$shannon,for.plot$LakeID,mean)
h.mean<-tapply(for.plot$hab.num,for.plot$LakeID,mean)
means<-data.frame(s.mean,h.mean)
means<-na.omit(means)
means$LakeID=row.names(means)

all.lakeq$AreaDif<-all.lakeq$AreaJuly09-all.lakeq$AreaNHD
all.lakeq$PropDif<-all.lakeq$AreaDif/all.lakeq$AreaNHD

means<-merge(means,all.lakeq,by='LakeID')
#means<-na.omit(means)

plot(means$AreaDif,means$s.mean)
iplot(means$PropDif,means$s.mean)

grow.means<-means[means$LakeID %in%growing.lakes,]
plot(grow.means$PropDif,grow.means$s.mean)
a<-lm(grow.means$PropDif~grow.means$s.mean)


####################################################
#												   #
#												   #
#	Spatial autocorrelation section				   #
#												   #
#												   #
#################################################### 

#load ape package
library(ape)

#isolate coords
div.coords<-subset(for.plot,select=c('Record_Easting','Record_Northing'))

#make distance matrix
div.dists<-as.matrix(dist(div.coords))

#check
dim(div.dists)				
div.dists[1:10,1:10]   

#make inverse
div.inv<-1/div.dists
diag(div.inv)<-0
div.inv<-ifelse(div.inv>10000000000,0,div.inv)
div.inv[1:10,1:10]

#calculate Spatial autocorrelation
div.MI<-Moran.I(for.plot$hab.nums,div.inv)

#make SAS inverse distance matrix
div.invSAS<-1/(1+div.dists)

#sas MI
div.MI.SAS<-Moran.I(for.plot$hab.nums,div.invSAS)

#could look at how moran's I changes at the level of the lake, lake group, and stratum
#also look at ade4 package

#try correlogram
# For an Introduction to geoR go to http://www.leg.ufpr.br/geoR
library(geoR)
simple.dist<-dist(div.coords)
summary(simple.dist)
breaks=c(0,100,500,1000,10000,20000,30000,40000,50000,100000,200000,250700)
v2 <- variog(coords=div.coords, data = for.plot$hab.nums, breaks = breaks)
plot(v2,type='b')
#oops, need to find the colocated data and remove it
dup.loc<-row.names(div.dists==0)

#make dist matrix for lakes
lake.coords<-means[,6:7]
lake.dist<-dist(lake.coords)

breaks=c(0,100,500,1000,10000)
v1 <- variog(coords=lake.coords, data = means$h.mean, breaks = breaks)
plot(v1,type='b')

#also check out lm.morantest
#can then plot residuals, plot effects of spatial lag on autocorrelation
