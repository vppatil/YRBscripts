library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
old<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

#explore active layer data
names(sqlFetch(yrb,'tblVegTransectMetadata'))
layers<-sqlQuery(yrb,"select TransectID,PF_GS,PF_SH,PF_FOR,FIB_GS,FIB_SH,FIB_FOR,HUM_GS,HUM_SH,HUM_FOR from tblVegTransectMetadata where (TransectID like '%1_99_1' or TransectID like '%1_99_2' or TransectID like '%0_9_1' or TransectID like '%0_9_2' or TransectID like '%1_58_1' or TransectID like '%0_2_1' or TransectID like '%0_2_2') and FIB_GS >-1",stringsAsFactors=FALSE)
layers$LakeID<-sapply(strsplit(layers$TransectID,split='_'),function(x) paste(x[2:4],collapse='_'))
layers<-layers[order(layers$LakeID),]


temlakes<-c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2')

tem.meta<-sqlQuery(yrb,"select distinct(TransectID) from tblVegTransectMetadata where TransectID like ('%0_9_1')")
tem.pct<-sqlQuery(yrb,"select distinct(TransectID) from tblVegTransectPctCover where TransectID like ('%0_9_1')")
	
tem.pct<-sqlQuery(yrb,"select Distinct TransectID,HabitatType from tblVegTransectPctCover where TransectID like ('%0_9_1')")

tem.lake<-sqlQuery(yrb,"select * from tblLakeBaseInfo where LakeID = ('1_99_1')")
tem.lake<-sqlQuery(yrb,"select DeepestPointDepth,SecchiDepth from tblLakeDesciption where LakeID = '1_99_1'")

tbllakeChemCharacteristics
tblBathymetry

tem.lake<-sqlQuery(yrb,"select * from tblLakeChemCharacteristics where LakeID = '1_99_1'")


tem.spplist<-sqlQuery(yrb,"select LakeID,Bearing,count(Species) as countSpp from tblVegTransectSppList where LakeID in ('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2') group by LakeID,Bearing")
tem.spplist<-sqlQuery(old,"select TransectID,count(Species) from tblVegTransectSppList where TransectID like '%0_2_2' group by TransectID")


tem.spplist<-sqlQuery(yrb,"select LakeID,Bearing,Species as countSpp from tblVegTransectSppList where LakeID in ('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2')")

compare pca to lake area? to lake change estimates?
	what about species richness data?
	compare soil moisture to other lake variables, to plant community structure
	come up with list of parameter estimates
	Friday = start some soil samples drying, put in oven. 
	pcadat<-sqlQuery(yrb,"select *  from PCAdat")
	sqlQuery(yrb,"select LakeID,count(TransectID) from PCAdat where LakeID in ('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2') group by LakeID")
	
	tem.veg<-sqlQuery(yrb,"select * from tblVegTransectSppList where LakeID in('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2')")
	tem.veg<-subset(tem.veg,select=c('Bearing','LakeID','Species','GS','LS','TS','DT','DF','MF','CF','AQ','FM','MU','EM','Other','Notes','TransectSppAutonumber'))
	tem.veg$TransectID<-paste(tem.veg$Bearing,tem.veg$LakeID,sep='_')
	tem.veg=tem.veg[,-c(1:2)]
	tem.veg=tem.veg[,c(16,1:15)]
	
	l27.spp<-read.csv('c:/users/vppatil/desktop/l27_spp.csv')
	l27.spp<-subset(l27.spp,select=c('TransectID','Species','GS','LS','TS','DT','DF','MF','CF','AQ','FM','MU','EM','Other','Notes','TransectSppAutonumber'))
	 
	 tem.veg<-rbind(tem.veg,l27.spp)	 

	
	temsum<-sqlQuery(yrb,"select LakeID,avgHabNum,avgshannon,avgGS,avgLS,avgLS+avgTS as avgShrub,avgCF,avgFM,shoreHab_GS,avgtransectLength from PCAmeans where LakeID in ('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2')")
	lake.class<-data.frame(LakeID=c('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2'),lake.class=c('shrink','stable','stable','shrink','fluct','increase','fluct','increase'))
	lake.class$expansion=ifelse(lake.class$lake.class=='fluct' | lake.class$lake.class=='increase','Flood','Not-flood')
	lake.class$trend=c('dec','nd','nd','dec','dec','nd','dec','nd')
	temsum<-merge(temsum,lake.class,by='LakeID')
	
	setwd('c:/users/vppatil/dropbox/alaskafiles/rscripts')
	tem.coord<-read.csv('temlakecoords.csv',header=TRUE)
	
	temsum<-merge(temsum,tem.coord,by='LakeID')
	
	tem.veg$LakeID <- ''
	for (i in 1:dim(tem.veg)[1])
	{
		tem.veg$LakeID[i]=paste(strsplit(tem.veg$TransectID[i],split='_')[[1]][2:4],collapse='_')
	}
	
	gs.veg<-subset(tem.veg,tem.veg$GS==TRUE)
	cf.veg<-subset(tem.veg,tem.veg$CF==TRUE)

	#now make table
	tem.vtab<-table(tem.veg$LakeID,tem.veg$Species)
	tem.vtab<-ifelse(tem.vtab>0,1,tem.vtab)
	
	#species dec using tem lakes
	library(vegan)
	
	#now decorana
	tem.dec<-decorana(tem.vtab)
	a<-plot(tem.dec,display='none')
	text(tem.dec,display='sites',labels=temsum$expansion)
#	identify(a,'species',col='red')
		
	#put in gs,ls,f moisture
	
	temsum$expansion<-factor(temsum$expansion)
	temsum$trend<-factor(temsum$trend)

	f<-envfit(tem.dec,temsum[,c(12:15)])
	b<-ordisurf(tem.dec,temsum[,c(14:15)])

	#######################################################################
	# using all entered spp.

	pca<-sqlQuery(yrb,"select LakeID,AreaJuly09,shannon,PropDif from LakeVegHabitatSummaries")
	pca2<-sqlQuery(yrb,"select * from PCAdat")
	lake.base<-sqlQuery(yrb,"select * from tblLakeBaseInfo")
	
	pca<-pca[pca$LakeID %in% tem.veg$LakeID,]
	
	chem<-sqlQuery(old,"select *  from LakeChemCharacteristics")
	#chem<-merge(chem,pca,by='LakeID')
	#chem<-chem[!duplicated(chem$LakeID),]
	#turn on or off to use chem data
	#lake.base<-merge(lake.base,chem,by='LakeID')
	
	tem.veg<-sqlQuery(yrb,"select LakeID,Bearing,Species,GS from tblVegTransectSppList",stringsAsFactors=FALSE)
	#tem.veg2<-sqlQuery(old,"select TransectID,Species from tblVegTransectSppList",stringsAsFactors=FALSE)
	tem.veg2<-read.csv('c:/users/vppatil/desktop/l27_spp.csv',stringsAsFactors=FALSE)
	#tem.veg2=subset(tem.veg2,tem.veg2$GS == 'TRUE',select=c('TransectID','Species'))
	tem.veg2$LakeID <- ''
	for (i in 1:dim(tem.veg2)[1])
	{
		tem.veg2$LakeID[i]=paste(strsplit(tem.veg2$TransectID[i],split='_')[[1]][2:4],collapse='_')
	}
	tem.veg<-subset(tem.veg,select=c('LakeID','Species'))
	tem.veg2<-subset(tem.veg2,select=c('LakeID','Species'))
	tem.veg<-rbind(tem.veg,tem.veg2)
	tem.veg$LakeID<-factor(tem.veg$LakeID)
	tem.veg=tem.veg[-c(grep('-',tem.veg$Species),grep('_',tem.veg$Species),grep('unk',tem.veg$Species),grep('Unk',tem.veg$Species)),]
	tem.veg$Species<-factor(tem.veg$Species)

	#corrections
	lake.base<-lake.base[lake.base$LakeID %in% tem.veg$LakeID,]	
	tem.vtab<-table(tem.veg$LakeID,tem.veg$Species)
	tem.vtab<-ifelse(tem.vtab>0,1,tem.vtab)
	tem.vtab<-tem.vtab[apply(tem.vtab,1,sum)!=0,]
	tem.vtab<-tem.vtab[,apply(tem.vtab,2,sum)!=0]
	#tem.vtab<-tem.vtab[row.names(tem.vtab) %in% lake.base$LakeID,]
	
	actual.tem=tem.vtab[match(lake.class$LakeID,row.names(tem.vtab)),]
	
	actual.tem<-actual.tem[apply(actual.tem,1,sum)!=0,]
	actual.tem<-actual.tem[,apply(actual.tem,2,sum)!=0]
		actual.tem<-actual.tem[,!is.na(actual.tem[1,])]


	tem.dec<-decorana(actual.tem)
	tem.dec2<-decorana(tem.vtab)
	a<-plot(tem.dec,display='none')
	text(tem.dec,display='sites',labels=lake.class$expansion,xlim=c(-1,1),ylim=c(-1,1))
	b<-plot(tem.dec2,display='sites',type='points')
	d<-envfit(tem.dec2,tem.vtab)
	indicators=tem.vtab[,d$vectors$pval < .01]
	e<-envfit(tem.dec2,indicators)
	plot(e)
	#use moistforplot from moisture, and lake.class
	lake.class$soilmoisture=moistforplot[match(lake.class$LakeID,names(moistforplot))]
	lake.class.ord=lake.class[match(lake.class$LakeID,row.names(actual.tem)),]
	fit.dat<-lake.class.ord[,c(3,4)]
	
	
	#fit.dat$Team<-factor(fit.dat$Team)
	colnames(index)[4] = '\n\nPotentilla norvegica'
		colnames(index)[2] = '\n\nCicuta douglasii'

	b<-envfit(a,actual.tem,na.rm=TRUE)
	index=actual.tem[,b$vectors$pval<.05]
	index=index[,!is.na(index[1,])]
		c<-envfit(a,index,na.rm=TRUE)
plot(c)
	good.fit<-index[,c$vectors$pval < 0.05]

	
	plot(b)
	plot(c)
	identify(a,'species',col='red')
	tem.dist<-vegdist(actual.tem)
	c<-hclust(vegdist)
	tem.veg=tem.veg[,-c(1:2)]
	tem.veg=tem.veg[,c(16,1:15)]
	
	g<-barplot(temsum$avgGS,names.arg=temsum$LakeID,main = 'avgGS')
	names(barplot)=temsum$LakeID
	
	
	tem.ord<-temsum[,-c(1,10:13,15)]
	library(vegan)
	tem.dist<-vegdist(tem.ord)
	tem.dec<-decorana(tem.dist)
	a<-plot(tem.dec,sub='Fig. 2. Detrended correspondence anlaysis of eight lakes based on coarse-scale habitat structure variables',display='none',xlim=c(-1,1),ylim=c(-.5,.5))
	text(tem.dec,labels=as.character(temsum$expansion))

	c<-envfit(a,data.frame(class=temsum$lake.class,expansion=temsum$expansion))
	
	b<-prcomp(~.,data=tem.ord)
	biplot(b,xlabs=temsum$expansion,sub='Fig. 2. Principal components analysis of Flooding vs. Non-Flooding lakes (n=8)')
	identify(tem.dec.plot,'species')

	tem.pca<-prcomp(tem.ord)
	tem.dist<-vegdist(temsum[,c(2:9,14:15)])
	tem.clust<-hclust(tem.dist)
	plot(tem.clust)
	
	
par(mfcol=c(3,1))
mean.gs<-tapply(temsum$avgGS,temsum$expansion,mean)
se.gs<-tapply(temsum$avgGS,temsum$expansion,function(x)sd(x)/sqrt(length(x)))
g<-barplot(mean.gs,names.arg=c('High Flood','Low Flood'),main='mean width of GS band',ylim=c(0,70))
superpose.eb(g,mean.gs,se.gs)

mean.Shrub<-tapply(temsum$avgShrub,temsum$expansion,mean)
se.Shrub<-tapply(temsum$avgShrub,temsum$expansion,function(x)sd(x)/sqrt(length(x)))
g<-barplot(mean.Shrub,names.arg=c('High Flood','Low Flood'),main='mean width of Shrub band',ylim=c(0,60))
superpose.eb(g,mean.Shrub,se.Shrub)

	temsum<-merge(temsum,lake.class,by='LakeID')
	

#get total forest edge from pct cover

temsum<-sqlQuery(yrb,"select LakeID,avgHabNum,avgshannon,avgGS,avgLS,avgLS+avgTS as avgShrub,avgCF,avgFM,shoreHab_GS,avgtransectLength from PCAmeans where LakeID in ('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2')")
	lake.class<-data.frame(LakeID=c('1_99_1','1_99_2','0_9_1','0_9_2','1_27_1','1_58_1','0_2_1','0_2_2'),lake.class=c('shrink','stable','stable','shrink','fluct','increase','fluct','increase'))
	lake.class$expansion=ifelse(lake.class$lake.class=='fluct' | lake.class$lake.class=='increase','Flood','Not-flood')
	lake.class$trend=c('dec','nd','nd','dec','dec','nd','dec','nd')
	temsum<-merge(temsum,lake.class,by='LakeID')
	
pct<-sqlFetch(yrb,'tblVegTransectPctCover',stringsAsFactors=FALSE)
pct2<-sqlFetch(old,'tblVegTransectPctCover',stringsAsFactors=FALSE)
pct2<-pct2[grep('1_27_1',pct2$TransectID),]


pct$LakeID<-sapply(strsplit(pct$TransectID,split='_'),function(x) paste(x[2:4],collapse='_'))

a<-subset(pct,pct$HabitatType %in% c('DF','MF','CF','DT'))
b<-subset(pct2,pct2$HabitatType %in% c('DF','MF','CF','DT'))

1_27 dist= mean(8,16,15,9)\
mean=8
se= 2.04

hits = 4 

for tem.forest

l27=data.frame(LakeID = '1_27_1',forest.mean = 8,forest.se = 2.04,lake.class = 'fluct',expansion='Flood',trend = 'nd')
se = function(x) sd(x)/sqrt(length(x))
mina<-tapply(a$Start,list(a$TransectID,a$HabitatType),min)
disttable<-xtabs(Start~TransectID+HabitatType,data=a)
doubles=table(a$TransectID,a$HabitatType)
disttable[doubles>1]=mina[doubles>1]

disttable<-apply(disttable,1,function(x) min(x[x>0]))
disttable[disttable==Inf]=0
f.tr<-row.names(cbind(disttable))
	
m<-match(f.tr,pct$TransectID)
pct$forest.dist=NA
pct$forest.dist[m]=disttable


forest.ts<-table(a$LakeID,a$TransectID,a$HabitatType)
forest.ts<-ifelse(forest.ts>0,1,forest.ts)
forest.ts<-apply(forest.ts,c(1,2),sum)
forest.ts<-ifelse(forest.ts>0,1,forest.ts)
forest.ts<-apply(forest.ts,1,sum)




se=function(x) sd(x)/sqrt(length(x))

forest.mean<-tapply(pct$forest,pct$LakeID,function(x) mean(na.omit(x)))
forest.se<-tapply(pct$forest,pct$LakeID,function(x) se(na.omit(x)))

forest<-data.frame(forest.mean,forest.se,LakeID=row.names(cbind(forest.mean)))

m2<-match(row.names(cbind(forest.ts)),forest$LakeID)
forest$num.foresthits=0
forest$num.foresthits[m2]=forest.ts

tem.forest=subset(forest,forest$LakeID %in% lake.class$LakeID)
#tem.forest<-merge(tem.forest,lake.class,by='LakeID')
tem.forest<-tem.forest[,c(1,2,3)]

l27<-l27[,c(2,3,1)]
tem.forest<-rbind(tem.forest,l27)
#tem.forest$weighted<-tem.forest$forest.mean*(tem.forest$num.foresthits/4)

#in wrong order for linking back to temsum for ord
temsum<-merge(temsum,tem.forest,by='LakeID')

	#look at GS diversity, Shrub diversity in particular, 
	# will also want to look at shrub stem density with distance from shore.
	#starting point of forest edge would be good
mean.hits<-tapply(tem.forest$num.foresthits,tem.forest$expansion,mean)/4
mean.fdist<-tapply(temsum$forest.mean,temsum$expansion,mean)
se.fdist<-tapply(temsum$forest.mean,temsum$expansion,se)


barplot(mean.hits,names.arg=c('High Flood','Low Flood'),main='Mean proportion of transects with forest edge < 100m')
fmean<-barplot(mean.fdist,names.arg=c('High Flood','Low Flood'),main='Mean Distance to Forest Edge',ylim=c(0,100))
superpose.eb(fmean,mean.fdist,se.fdist)


	barplot(temsum$avgtransectLength,names.arg=temsum$LakeID,col=temsum$cols)
	
	-check that alex went to the right 0_9 lake
	PCAmeans<-sqlFetch(yrb,'PCAmeans')
	
	 temtree<-rpart(LakeID~avgHabNum+avgshannon+avgGS+avgShrub+avgCF+avgFM+shoreHab_GS,data=PCAmeans)
	missing1<- sqlQuery(yrb,"select distinct LakeID from tblTransects where TransectID in (select TransectID from tblVegTransectMetadata) ")
	missing2<- sqlQuery(old,"select distinct LakeID from tblTransects where TransectID in (select TransectID from tblVegTransectMetadata) ")
	
intersect(missing1$LakeID,missing2$LakeID)
?setdiff	
	
 ################### PLOTTING SECTION################################################

 par(mfcol=c(3,2))
 ### by flood/not

mean.gs<-tapply(temsum$avgGS,temsum$expansion,mean)
se.gs<-tapply(temsum$avgGS,temsum$expansion,function(x)sd(x)/sqrt(length(x)))
g<-barplot(mean.gs,names.arg=c('High Flood','Low Flood'),main='Mean Width of Grass/Sedge community\n Along Transects',ylim=c(0,70),ylab='meters')
superpose.eb(g,mean.gs,se.gs)
text(locator(1),'t = -3.274\np = 0.03')

gs.t=t.test(temsum$avgGS[temsum$expansion=='Flood'],temsum$avgGS[temsum$expansion!='Flood'])

mean.Shrub<-tapply(temsum$avgShrub,temsum$expansion,mean)
se.Shrub<-tapply(temsum$avgShrub,temsum$expansion,function(x)sd(x)/sqrt(length(x)))
g<-barplot(mean.Shrub,names.arg=c('High Flood','Low Flood'),main='Mean width of Shrub Community Along Transects',ylim=c(0,60),ylab='meters')
superpose.eb(g,mean.Shrub,se.Shrub)
 
 ##forest plots
 
mean.hits<-tapply(tem.forest$num.foresthits,tem.forest$expansion,mean)/4
mean.fdist<-tapply(temsum$forest.mean,temsum$expansion,mean)
se.fdist<-tapply(temsum$forest.mean,temsum$expansion,se)
#barplot(mean.fdist,names.arg=c('High Flood','Low Flood'),main='mean proportion of transects with forest edge < 100m')
fmean<-barplot(mean.fdist,names.arg=c('High Flood','Low Flood'),main='Mean Distance to Forest Edge',ylim=c(0,100),ylab='meters')
superpose.eb(fmean,mean.fdist,se.fdist)

flood.m<-cbind(mean.gs,mean.Shrub,mean.fdist)
row.names(flood.m)=c('High Flood','Low Flood')
flood.se<-cbind(se.gs,se.Shrub,se.fdist)
f<-barplot(flood.m,beside=TRUE,names.arg=c('Grass/Sedge\nZone Width','Shrub\nZone Width','Distance to\nForest Edge'),legend=TRUE,args.legend=c(x=5),ylim=c(0,80),ylab='meters')
superpose.eb(f,flood.m,flood.se)

## by dec trend, not

		mean.gs<-tapply(temsum$avgGS,temsum$trend,mean)
		se.gs<-tapply(temsum$avgGS,temsum$trend,function(x)sd(x)/sqrt(length(x)))
		g<-barplot(mean.gs,names.arg=c('Drying','Not Drying'),main='Mean Width of Grass/Sedge Community\n Along Transects',ylim=c(0,70),ylab='meters')
		superpose.eb(g,mean.gs,se.gs)

		mean.Shrub<-tapply(temsum$avgShrub,temsum$trend,mean)
		se.Shrub<-tapply(temsum$avgShrub,temsum$trend,function(x)sd(x)/sqrt(length(x)))
		g<-barplot(mean.Shrub,names.arg=c('Drying','Not Drying'),main='Mean Width of Shrub Community Along Transects',ylim=c(0,60),ylab='meters')
		superpose.eb(g,mean.Shrub,se.Shrub)
		 
		 ##forest plots
		 
		mean.hits<-tapply(tem.forest$num.foresthits,tem.forest$trend,mean)/4
		mean.fdist2<-tapply(temsum$forest.mean,temsum$trend,mean)
		se.fdist2<-tapply(temsum$forest.mean,temsum$trend,se)
		#barplot(mean.hits,names.arg=c('Decreasing Trend','No Decreasing Trend'),main='mean Distance to Forest Edge')
		fmean2<-barplot(mean.fdist2,names.arg=c('Drying','Not Drying'),main='Mean Distance to Forest Edge',ylim=c(0,100),ylab='meters')
		superpose.eb(fmean2,mean.fdist2,se.fdist2)
		
		
dry.m<-cbind(mean.gs,mean.Shrub,mean.fdist2)
row.names(dry.m)=c('Drying','Not Drying')
dry.se<-cbind(se.gs,se.Shrub,se.fdist2)

hab.tiff<-tiff('habzones.tif',height=9,width= (15.67*9/10),units = 'in',res=300)		

par(mfcol=c(1,2))

f<-barplot(flood.m,beside=TRUE,col=c('Blue','Red'),names.arg=c('Grass/Sedge\nZone Width','Shrub\nZone Width','Distance to\nForest Edge'),legend=TRUE,args.legend=c(x=7),ylim=c(0,80),ylab='meters')
superpose.eb(f,flood.m,flood.se)

##flood.m2<-cbind(mean.gs,mean.Shrub,mean.fdist)
row.names(dry.m)=c('Drying Trend','Not Drying')
#flood.se2<-cbind(se.gs,se.Shrub,se.fdist)
f2<-barplot(dry.m,beside=TRUE,col=c('Red','Blue'),names.arg=c('Grass/Sedge\nZone Width','Shrub\nZone Width','Distance to\nForest Edge'),legend=TRUE,args.legend=c(x=7),ylim=c(0,80),ylab='meters')
superpose.eb(f2,dry.m,dry.se)

dev.off()