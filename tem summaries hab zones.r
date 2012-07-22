
	
#first load the rodbc package
library(RODBC)

#next 2 odbc connections are created) or TransectID like ( called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
yrb2011<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
Lakes<-sqlQuery(yrb2011,"select distinct LakeID,TransectID from tblTransects")
oldLakes<-sqlQuery(yrb,"select distinct LakeID,TransectID from tblTransects")

#get average GS length
pctold<-sqlFetch(yrb,'tblVegTransectPctCover',stringsAsFactors=FALSE)
pct<-sqlFetch(yrb2011,'tblVegTransectPctCover',stringsAsFactors=FALSE)

l27<-pctold[grep('1_27_1',pctold$TransectID),]
l27<-subset(l27,HabitatTypeLength>0,select=c('HabitatTypeLength','HabitatType','TransectID'))
pct<-subset(pct,pct$HabitatTypeLength>0,select=c('HabitatTypeLength','HabitatType','TransectID'))
pct<-rbind(pct,l27)
pct$LakeID<-sapply(strsplit(pct$TransectID,split='_'),function(x) paste(x[2:4],collapse='_'))

tem=subset(pct,pct$LakeID %in% c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2'))

gs=subset(tem,tem$HabitatType=='GS')
gs.mean<-tapply(gs$HabitatTypeLength,gs$LakeID,mean)
barplot(gs.mean)


c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2'))


#pct cover for tem hab zone comp
pct<-sqlFetch(yrb2011,'tblVegTransectPctCover')
pct2<-sqlQuery(yrb,"select * from tblVegTransectPctCover where TransectID like '%1_27_1'")
meta1_27<-sqlQuery(yrb,"select * from tblVegTransectMetadata where TransectID like '%1_27_1'")
pct2$LakeID='1_27_1'
pct2<-pct2[4:6,]
pct2$HabitatType<-factor(pct2$HabitatType)
pct2.hab<-tapply(pct2$HabitatTypeLength,list(pct2$LakeID,pct2$HabitatType),mean)




pct<-merge(pct,Lakes,by='TransectID')
tem.pc<-subset(pct,pct$LakeID %in% c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2'))
tem.pc$LakeID<-factor(tem.pc$LakeID)


#need to rank start points for all habitat types within all transects
ranks<-vector()
tem.pc$rank=0

for (i in 1:dim(tem.pc)[1])
{
	tem.pc$rank[tem.pc$TransectID==tem.pc$TransectID[i]]= rank(tem.pc$Start[tem.pc$TransectID==tem.pc$TransectID[i]])}
}

#constrain transects to 100m
tem.pc$HabitatTypeLength<-ifelse(tem.pc$Start>=100,0,tem.pc$HabitatTypeLength)
tem.pc$HabitatTypeLength<-ifelse(tem.pc$End > 100,100-tem.pc$Start,tem.pc$HabitatTypeLength)

#plot
test<-tapply(tem.pc$HabitatTypeLength,list(tem.pc$LakeID,tem.pc$HabitatType),mean)

test[is.na(test)]<-0
test<-test[,c(1,4,7,10,15,16)]
test<-rbind(test,c(0,0,6,2,92,0))
row.names(test)[8]='1_27_1'

lake.trends<-lake.class$lake.class[match(row.names(test),lake.class$LakeID)]
comp.names<-paste(lake.trends,row.names(test),sep='\n')
barplot(t(as.matrix(test)),col=rainbow(6),names.arg=comp.names,legend=TRUE,beside=TRUE)
	
#tem vs. non-tem meta
T<-sqlQuery(yrb2011,"select * from tblVegTransectMetadata where PF_Depth_L = -1")
nT<-sqlQuery(yrb2011,"select * from tblVegTransectMetadata where PF_GS = -1")



names(sqlFetch(yrb2011,'tblVegTransectPctCover'))

T<-merge(T,Lakes,by='TransectID')
Tlate<-T[T$TransectDate > as.POSIXct("2011-7-15 AKDT"),]
nT<-merge(nT,Lakes,by='TransectID')

#turn this on or off to get Tem lakes subset
T<-subset(T,T$LakeID %in% c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2'))
nT<-subset(nT,nT$LakeID %in% c('1_99_1',	'1_99_2'	,'0_9_1'	,'0_9_2'	,'1_27_1'	,'1_58_1'	,'0_2_1'	,'0_2_2'))

notes<-rbind(T,nT)

notes<-subset(notes,select=c('LakeID','TransectID','PF_Peat_Notes','Notes1','AnimalUse','gthan','lthan'))
waterlogged<-c(grep('water',notes$PF_Peat_Notes),grep('deep',notes$PF_Peat_Notes))
notes$PF_Peat_Notes[waterlogged]

beaver<-grep('beaver',notes$AnimalUse)	
	#read the notes!!!- look for beaver enteries


T$LakeID<-factor(T$LakeID)
Tlate$LakeID <-factor(Tlate$LakeID)
nT$LakeID<-factor(nT$LakeID)

pfrost<-function(df)
{
	df<-T
	df1<-subset(df,df$PF_GS>0)
	df2<-subset(df,df$PF_SH>0)
	df3<-subset(df,df$PF_FOR>0)
	pg<-tapply(df1$PF_GS,df1$LakeID,function(x) mean(na.omit(x)))
	ps<-tapply(df2$PF_SH,df2$LakeID,function(x) mean(na.omit(x)))
	pf<-tapply(df3$PF_FOR,df3$LakeID,function(x) mean(na.omit(x)))
	
	p<-cbind(pg,ps,pf)
	return(p)
}


pfrost2<-function(df)
{
	pl<-tapply(df$PF_DEPTH_L,df$LakeID,mean)
	pfe<-tapply(df$PF_DEPTH_FE,df$LakeID,mean)
	
	p<-cbind(pl,pfe)
	p<-data.frame(p)
	return(p)
}

#bring in 1_27 pfrost data
meta1_27<-meta1_27[,c(1,6,7)]
pf127<-cbind(mean(meta1_27$PF_Depth_L),mean(meta1_27$PF_Depth_F))

pT<-data.frame(pfrost(T))
pT$LakeID<-row.names(pT)
pT<-merge(pT,lake.class,by='LakeID')
#for hab/lake means
#otherwise leave as pT

nona.mean<-function(x) mean(na.omit(x))

pg.hab<-tapply(pT$pg,pT$expansion,nona.mean)
ps.hab<-tapply(pT$ps,pT$expansion,nona.mean)
pf.hab<-tapply(pT$pf,pT$expansion,nona.mean)


pg.lake<-tapply(pT$pg,pT$LakeID,nona.mean)
ps.lake<-tapply(pT$ps,pT$LakeID,nona.mean)
pf.lake<-tapply(pT$pf,pT$LakeID,nona.mean)

se<-function(x) sd(x)/sqrt(length(x))
nona.se<-function(x) se(na.omit(x))
pg.se<-tapply(pT$pg,pT$expansion,nona.se)
ps.se<-tapply(pT$ps,pT$expansion,nona.se)
pf.se<-tapply(pT$pf,pT$expansion,nona.se)

pg.se2<-tapply(pT$pg,pT$LakeID,nona.se)
ps.se2<-tapply(pT$ps,pT$LakeID,nona.se)
pf.se2<-tapply(pT$pf,pT$LakeID,nona.se)

hab.mean<-cbind(pg.hab,ps.hab,pf.hab)
lake.mean=cbind(pg.lake,ps.lake,pf.lake)
hab.se<-cbind(pg.se,ps.se,pf.se)
hab.se2<-cbind(pg.se2,ps.se2,pf.se2)

pf.hab<-barplot(hab.mean,names.arg=c('Grass/Sedge','Shrub','Forest'),beside=TRUE,ylim=c(0,100),main='Active Layer Depth by lake type and community type',ylab='cm',legend=TRUE,args.legend=c(x=5))
superpose.eb(pf.hab,hab.mean,hab.se)

par(mfrow=c(1,1))	
pT.mean<-tapply(pT,2,function(x) mean(na.omit(x)))
pT.se<-apply(pT,2,function(x) se(na.omit(x)))
p.plot<-barplot(pT.mean,names.arg=c('GS','Shrub','Forest'),main='Mean Active Layer Depth',ylim=c(0,100),ylab='cm')
superpose.eb(p.plot,pT.mean,pT.se)

habs<-rep(c('GS','SH','FOR'),each=dim(pT)[1])
p.dat<-data.frame(Hab=habs,depth=c(pT[,1],pT[,2],pT[,3]))
p.dat<-na.omit(p.dat)
p.anova<-lm(depth~Hab,data=p.dat)
aov(p.anova)

pTlate<-pfrost(Tlate)

par(mfrow=c(1,1))	
pTlate.mean<-apply(pTlate,2,function(x) mean(na.omit(x)))
pTlate.se<-apply(pTlate,2,function(x) se(na.omit(x)))
p.plot<-barplot(pTlate.mean,names.arg=c('GS','Shrub','Forest'),main='Mean Active Layer Depth',ylim=c(0,100),ylab='cm')
superpose.eb(p.plot,pTlate.mean,pTlate.se)


p.dat<-data.frame(Hab=habs,depth=c(pTlate[,1],pTlate[,2],pTlate[,3]))
p.dat<-na.omit(p.dat)
p.anova<-lm(depth~Hab,data=p.dat)
aov(p.anova)

pT2<-as.matrix(pfrost2(nT))
pT2=subset(pT2,row.names(pT2)%in%lake.class$LakeID)
se<-function(x) sd(x)/sqrt(length(x))

#for all lakes
 pT2=subset(pT2,row.names(pT2)%in%lake.class$LakeID)
 pT2[7,]=c(33.5,56.7)#interpolated from pT
pT2.mean<-apply(pT2,2,function(x) mean(na.omit(x)))
pT2.se<-apply(pT2,2,function(x) se(na.omit(x)))
p2.plot<-barplot(pT2.mean,names.arg=c('Shore','Forest Edge'),main='Mean Active Layer Depth',ylim=c(0,100))
superpose.eb(p2.plot,pT2.mean,pT2.se)  #turn off for Tem lake plots

t.test(pT2[,1],pT2[,2])


#for tem lakes subset

pf12lake.trends<-lake.class$lake.class[match(row.names(pT),lake.class$LakeID)]

pT[is.na(pT)]<-0
barplot(as.matrix(t(pT)),beside=TRUE)
pfrost.e<-lake.class$expansion[match(row.names(pT),lake.class$LakeID)]
flood.pt<-pT[pfrost.e == 'Flood',]
noflood.pt<-pT[pfrost.e=='Not-flood',] 


lake.trends<-lake.class$lake.class[match(row.names(pT2),lake.class$LakeID)]
comp.names<-paste(lake.trends,row.names(pT2),sep='\n')

tem.bar2<-barplot(t(pT2),beside=TRUE,names.arg=comp.names[c(1:4,6:8)],col=c('blue','darkgreen'),legend=TRUE)

#get total forest edge from pct cover
pct<-sqlFetch(yrb2011,'tblVegTransectPctCover',stringsAsFactors=FALSE)
pct$LakeID<-sapply(strsplit(pct$TransectID,split='_'),function(x) paste(x[2:4],collapse='_'))

a<-subset(pct,pct$HabitatType %in% c('DF','MF','CF','DT'))

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
tem.forest<-merge(tem.forest,lake.class,by='LakeID')

barplot(tem.forest$num.foresthits,names.arg=row.names(tem.forest))
barplot(tem.forest$forest.mean,names.arg=row.names(tem.forest))

#need weighted average to account for presence, distance.

Flooding frequency