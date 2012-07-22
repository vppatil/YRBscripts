library(RODBC)
	yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
	
	hab<-sqlQuery(yrb,"select vegtypefinal,avg(sppcount) as richAvg from( select gridid,trapid,vegtypefinal,count(vegspecies) as sppcount from (select distinct VegSpecies,GridID,TrapID,vegtypefinal from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.vegtypefinal from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID)) group by vegtypefinal,gridid,trapid) group by vegtypefinal"
	

m.q<-"select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapChecks.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID"

test<-sqlQuery(yrb,m.q)

#next is to get species richness for each trap

mrich<-"select GridID,TrapID,HabitatType1,count(VegSpecies) as sppCount from (select distinct VegSpecies,GridID,TrapID,HabitatType1 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapChecks.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID) group by gridid, trapid,habitattype1)"

test2<-"select habitattype1,avg(sppcount) as richAvg from( select gridid,trapid,HabitatType1,count(vegspecies) as sppcount from (select distinct VegSpecies,GridID,TrapID,HabitatType1 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID)) group by habitattype1,gridid,trapid) group by habitattype1"

a<-sqlQuery(yrb,test2)


test2<-"select habitattype2,avg(sppcount) as richAvg from( select gridid,trapid,HabitatType2,count(vegspecies) as sppcount from (select distinct VegSpecies,GridID,TrapID,HabitatType2 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.HabitatType2 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID)) group by habitattype2,gridid,trapid) group by habitattype2"


test4<-"select HabitatType2,count(vegspecies) as sppcount from (select distinct VegSpecies,GridID,TrapID,HabitatType2 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.HabitatType2 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID)) group by habitattype2"


a<-sqlQuery(yrb,test4)


#now you know if each species has shown up in the plot

#just loop it.
allcurves<-vector()
bad=vector()
grids=row.names(table(a$GridID))
for(l in 1:length(unique(a$GridID)))
{

	plot.table<-table(a$TrapID,a$VegSpecies,a$GridID)[,,l]
	plot.table<-plot.table[,apply(plot.table,2,sum) > 0]
	plot.table<-plot.table[apply(plot.table,1,sum) > 0,]
	accum<-plot.table[1,]
	for (i in 2:dim(plot.table)[1])
	{
		accum.vec = apply(plot.table[1:i,],2,sum)
		accum.vec<-ifelse(accum.vec > 0,1,accum.vec)
		accum<-rbind(accum,accum.vec)
	}

	forcurve<-apply(accum,1,sum)

	if (length(forcurve) == 25)
	allcurves<-cbind(allcurves,forcurve)
	else
		bad=c(bad,l)
}

colnames(allcurves)=grids[-bad]
 ts.plot(allcurves,col=rainbow(54),xlab='num plots',ylab = 'cumulative richness',main = 'species accum curves for all mammal grids')
 m<-apply(allcurves,1,mean)
 plot(m,type='l',main = 'mean species accumulation per mammal grid plot',xlab = 'num plots',ylab = 'richness')
 se<-function(x) {sd(x)/sqrt(length(x)) }
 lines(m+apply(allcurves,1,se),col='red')
 lines(m-apply(allcurves,1,se),col='red')
 
 #fit.func
 library(mmSAR)
 data(negexpo)
 data(expo)
 data.massage<-function(curve.vec,nm)
 {
	x = (.5^2) * 1:25 # area sampled
	mmsar.dat<-list(name = nm,data=data.frame(a = x,s = curve.vec))
	return(mmsar.dat)
}

#extract negexp asymptotes for all
all.asym<-vector()
for(i in 1:dim(allcurves)[2])
{
	i=i+1
	dat=data.massage(allcurves[,i],colnames(allcurves)[i])
	res<-rssoptim(negexpo,dat)$par[1]
	all.asym<-c(all.asym,res)
}

asym.df<-data.frame(grid=colnames(allcurves),asym=all.asym)

#read in lakes
setwd('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/')
lakeareas<-read.csv('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/lake_areas.csv',header=TRUE)

get.lid<-function(x) {
	l<-strsplit(as.character(x),split='_')[[1]]
	l<-paste(l[3:5],collapse='_')
	return(l)}
	
#add lakeid column
asym.df$LakeID<-apply(data.frame(asym.df[,1]),1,get.lid)

#now merge with lakeareas
asym.df<-merge(asym.df,lakeareas,by='LakeID')

plot(asym.df$area_ha,asym.df$asym,xlab = 'number plots',ylab = 'estimated asymtotic site richness')