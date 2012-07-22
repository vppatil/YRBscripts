
#use hab.all

library(RODBC)
new<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
old<-odbcConnectAccess2007('C:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

#table called 'habAlltemp')

old.pctcov<-sqlFetch(old,'tblVegTransectPctCover')
new.pctcov<-sqlFetch(new,'tblVegTransectPctCover')

hab.func<-function(df)
{
	df<-subset(df,select=c('TransectID','HabitatType','HabitatTypeLength','Start','End'))
	return(df)
}

old.hab<-hab.func(old.pctcov)
new.hab<-hab.func(new.pctcov)

habAll<-rbind(old.hab,new.hab)
aq<-which(habAll$HabitatType%in% c('MUFL','AQ','EM','FM'))
hab.aq.old<-habAll[aq,,drop=TRUE]
habAll<-habAll[-aq,,drop=TRUE]


lakeid.func<-function(df)
{
	trans<-as.character(df$TransectID)
	tsplit<-strsplit(trans,split='_')
	
	#now paste the last three elements together, return as a vector
	lid<-sapply(tsplit,function(x) paste(x[2:4],collapse='_'))
	lid.df<-data.frame(TransectID=trans,LakeID=lid)
	df2<-merge(df,lid.df,by='TransectID')
}


habAll<-lakeid.func(habAll)
sqlQuery(new,'drop table habAlltemp')
sqlSave(new,habAll,'habAlltemp')


#get num habs by transect
numhabs<-sqlQuery(new,"select LakeID,TransectID,count(HabitatType) as HabNum from habAlltemp group by LakeID,TransectID")

#get shannon index by transect
Shannon.func<-function(hab.df)
{
	length.vector<-hab.df$HabitatTypeLength
	pi.vec<-length.vector/sum(length.vector)
	shannon<-sum(pi.vec*log(pi.vec))*-1
	return(shannon)
}

transects<-unique(numhabs$TransectID)

hab.list<-list()
for (i in 1:length(transects))
{
	hab.list[[i]]=subset(habAll,habAll$TransectID==transects[i])
}

num.func<-function(hab)
{
	num<-dim(hab)[1]
}

shannon<-data.frame(TransectID=numhabs$TransectID,shannon=sapply(hab.list,Shannon.func))

habs<-unique(habAll$HabitatType)

hablength<-sqlQuery(new,"select TransectID,HabitatType,sum(HabitatTypeLength) as totalLength from habAlltemp group by TransectID,HabitatType")

#get hab lengths for all Habs for each transect

hablengths.tab<-xtabs(HabitatTypeLength~TransectID+HabitatType,data=habAll)
hablengths.df<-data.frame(cbind(hablengths.tab))
hablengths.df$TransectID<-row.names(hablengths.df)

#now get shoreline type for each transect
shorehab<-sqlQuery(new,"select TransectID,HabitatType as ShoreHab from (select TransectID,HabitatType,Start from habAlltemp where Start=0)")


#get total transect length per transect
transectLength<-sqlQuery(new,"select TransectID,sum(HabitatTypeLength) as transectLength from habAlltemp group by TransectID")


#maybe need to do some cleaning/postprocessing here- get the habitat type widths constrained to 100m?

#######FM width
newFM<-sqlQuery(new,"select TransectID,FM from tblVegTransectMetadata",stringsAsFactors=FALSE)
oldFM<-sqlQuery(old,"select TransectID,FM_w as FM from tblVegTransectMetadata",stringsAsFactors=FALSE)

old.diff<-which(!(oldFM$TransectID %in%newFM$TransectID))
oldFM<-oldFM[old.diff,]

FMwidths<-rbind(newFM,oldFM)


#habitat proportions
habAll.t<-merge(habAll,transectLength,by='TransectID')
habproportions = hablengths.tab/xtabs(transectLength~TransectID+HabitatType,data=habAll.t)
habproportions[is.na(habproportions)]=0
habproportions = data.frame(cbind(habproportions))

#now to merge everything
a<-merge(numhabs,shannon,by='TransectID')
b<-merge(a,transectLength,by='TransectID')
c<-merge(b,hablengths.df,by='TransectID')
d<-merge(c,shorehab,by='TransectID')
e<-merge(d,FMwidths,by='TransectID')
names(habproportions)<-paste(names(habproportions),'Prop',sep='_')
habproportions$TransectID=row.names(habproportions)


############################################################
transectDataPCA<-merge(e,habproportions,by='TransectID')
############################################################

sqlSave(new,transectDataPCA,'PCAdat')

 [1] "TransectID"     "LakeID"         "HabNum"         "shannon"       
 [5] "transectLength" "CF"             "DF"             "DT"            
 [9] "EM"             "FMx"            "GS"             "LS"            
[13] "MF"             "MUFL"           "TS"             "CFLS"          
[17] "CFTS"           "GSLS"           "GSTS"           "LSburn"        
[21] "LSCF"           "LSDT"           "LSMF"           "ShoreHab"      
[25] "FMy"            "CF_Prop"        "DF_Prop"        "DT_Prop"       
[29] "EM_Prop"        "FM_Prop"        "GS_Prop"        "LS_Prop"       
[33] "MF_Prop"        "MUFL_Prop"      "TS_Prop"        "CFLS_Prop"     
[37] "CFTS_Prop"      "GSLS_Prop"      "GSTS_Prop"      "LSburn_Prop"   
[41] "LSCF_Prop"      "LSDT_Prop"      "LSMF_Prop"     



#get shore.freq
shore.table<-table(transectDataPCA$LakeID,transectDataPCA$ShoreHab)
shorenames=paste(colnames(shore.table),'shorefreq',sep='_')
shore.matrix<-data.frame(matrix(shore.table,dim(shore.table)[1],dim(shore.table)[2]))
names(shore.matrix)=shorenames


lakemean.func<-function(df,colnum)
{
	n<-names(df)
	l<-df$LakeID
	m<-tapply(df[,colnum],df$LakeID,mean)
	mean.df<-data.frame(m)
	names(mean.df)=paste(names(df)[colnum],'mean',sep='_')
	return (mean.df)
}

lakemeans<-list()
numeric.cols<-c(2:length(transectDataPCA[,1]))
l=length(numeric.cols)
for(i in 1:l)
{
	lakemeans[[i]]<-lakemean.func(transectDataPCA,numeric.cols[i])
}
Lake.df<-data.frame(LakeID=row.names(lakemean.func(transectDataPCA,numeric.cols[l])))
lakemeans<-sapply(lakemeans,cbind)
Lake.df<-cbind(Lake.df,lakemeans)
Lake.df<-cbind(Lake.df,shore.matrix)
#still need to deal with floating mat

transectDataPCA<-lakeid.func(transectDataPCA)
transectDataPCA<-merge(transectDataPCA,shorehab,by='TransectID')

library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
transectDataPCA<-sqlFetch(yrb,'PCAdat',stringsAsFactors=TRUE)
hist(log(transectDataPCA$GS[transectDataPCA$GS<50]))

#### calculate averages for everything by lakeID
forq<-names(transectDataPCA)[3:38]
forq<-paste('avg(',forq,') as avg',forq,collapse=',',sep='')

q="select LakeID,avg(HabNum) as avgHabNum,avg(shannon) as avgshannon,avg(transectLength) as avgtransectLength,avg(CF) as avgCF,avg(DF) as avgDF,avg(DT) as avgDT,avg(GS) as avgGS,avg(LS) as avgLS,avg(MF) as avgMF,avg(TS) as avgTS,avg(CFLS) as avgCFLS,avg(CFTS) as avgCFTS,avg(GSLS) as avgGSLS,avg(GSTS) as avgGSTS,avg(LSburn) as avgLSburn,avg(LSCF) as avgLSCF,avg(LSDT) as avgLSDT,avg(LSMF) as avgLSMF,avg(FM) as avgFM,avg(CF_Prop) as avgCF_Prop,avg(DF_Prop) as avgDF_Prop,avg(DT_Prop) as avgDT_Prop,avg(GS_Prop) as avgGS_Prop,avg(LS_Prop) as avgLS_Prop,avg(MF_Prop) as avgMF_Prop,avg(TS_Prop) as avgTS_Prop,avg(CFLS_Prop) as avgCFLS_Prop,avg(CFTS_Prop) as avgCFTS_Prop,avg(GSLS_Prop) as avgGSLS_Prop,avg(GSTS_Prop) as avgGSTS_Prop,avg(LSburn_Prop) as avgLSburn_Prop,avg(LSCF_Prop) as avgLSCF_Prop,avg(LSDT_Prop) as avgLSDT_Prop,avg(LSMF_Prop) as avgLSMF_Prop from PCAdat group by LakeID"


PCAmeans<-sqlQuery(yrb,q)

#now work out the shorehab counts by habitat type
shore.q<-sqlQuery(yrb,"select LakeID,ShoreHab from PCAdat")
shore.t<-table(shore.q$LakeID,shore.q$ShoreHab)
shore.n<-colnames(shore.t)
shore.t<-data.frame(cbind(shore.t))
names(shore.t)<-paste('shoreHab_',shore.n,sep='')
shore.t$LakeID=row.names(shore.t)
PCAmeans<-merge(PCAmeans,shore.t,by='LakeID')
sqlSave(yrb,PCAmeans,'PCAmeans')

#now run PCA
vars<-PCAmeans[,-1]

names(vars)<-gsub('/','.',names(vars))
pc1<-prcomp(~.,data=vars,cor=T)
biplot(pc1)

vars<-vars[-67,]
vars$avgFM[is.na(vars$avgFM)] = 0
varspc2<-prcomp(~.,data=vars,cor=T)
biplot(pc2)

pc3<-prcomp(~avgGS+avgCF+avgFM,data=vars,cor=TRUE)

install.packages('psych')

# Determine Number of Factors to Extract
install.packages('nFactors')
library(nFactors)
ev <- eigen(cor(vars)) # get eigenvalues
ap <- parallel(subject=nrow(vars),var=ncol(vars),
  rep=100,cent=.05)
nS <- nScree(ev$values, ap$eigen$qevpea)
plotnScree(nS) 