yrb2011<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011yrbiodiversity.accdb')


sqlSave(yrb,dat=lake.input,tablename='tblVegTransectSppList',rownames=FALSE,append=TRUE,safer=TRUE,nastring = '')

query<-"select tblVegSpeciesList.ScientificName,tblVegSpeciesList.VegSpID,tblVegTransectSppList.*,(tblVegTransectSppList.Bearing + '_' + tblVegTransectSppList.LakeID) as TransectID from tblVegSpeciesList left join tblVegTransectSppList on tblVegSpeciesList.VegSpID = tblVegTransectSppList.TransectSppAutonumber where tblVegTransectSppList.LakeID = '1_37_2'"



sql.entry<-function(sql.df,con)
{
	sql.names<-names(sql.df)
	sql.names<-paste(sql.names,collapse=',')
	char.vals<-c(1,15)
	sql.df[,char.vals]<-query.quote(sql.df[,char.vals])
	vals<-paste(sql.df,collapse=',')
	query<-paste("Insert into tblVegTransectSppList (",sql.names,") VALUES (",vals,")",collapse='')

	q<-sqlQuery(con,query)
	q
	print(q)
}

 and tblvegtransectspplist.bearing <> '213'

query<-"select TransectID,VegSpID as Species,GS,LS,TS,DT,DF,MF,CF,AQ,FM,MU,EM,Other,Notes from (select tblVegSpeciesList.ScientificName,tblVegSpeciesList.VegSpID,tblVegTransectSppList.*,(tblVegTransectSppList.Bearing + '_' + tblVegTransectSppList.LakeID) as TransectID from tblVegSpeciesList left join tblVegTransectSppList on tblVegSpeciesList.ScientificName = tblVegTransectSppList.Species where tblVegTransectSppList.LakeID = '1_28_2')"

lake.input<-sqlQuery(yrb2011,query,stringsAsFactors=FALSE)
rows<-dim(lake.input)[1]
lake.input

for(i in 1:rows)
{

	write<-lake.input[i,]
	write[,3:14] <-as.numeric(write[,3:14])
	write$Notes[is.na(write$Notes)]<-''

	sql.entry(write,yrb)
}
	
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}

.simpleCap <- function(x) 
{
    
	a <-substr(x,1,1)
	a<-toupper(a)
	b <-substring(x,2)
	paste(a,b,sep='')
}

n=names(lake.input)

setwd('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
luke<-read.csv('lukeData.csv',header=TRUE)
luke$TransectID = paste(luke$Transect,luke$Lake,sep='_')
luke=subset(luke,select=c('TransectID','Species','GS','LS','TS','DF','MF','CF','Other','Notes'))
names(luke)[1]='TransectID'
luke$AQ = 0
luke$FM = 0
luke$EM = 0
luke$MU = 0
luke$EM<-ifelse(luke$Other == 'EM',1,luke$EM)
luke[,3:9]<-ifelse(luke[,3:9] == 'X' ,1,0)
luke$DT = 0


#write.csv(cbind(table(luke$Notes)),'c:/users/vppatil/desktop/lukenotes.csv')
fm<-c(grep('fm',luke$Notes),grep('FM',luke$Notes))
mu<-c(grep('mu',luke$Notes),grep('M ',luke$Notes),grep('mf',luke$Notes))
em<-c(grep('em',luke$Notes),grep('EM',luke$Notes))
luke$FM[fm] = 1
luke$MU[mu] = 1
luke$EM[em] = 1
name.order<-match(n,names(luke))
luke=luke[,name.order]


yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
spplist<-sqlQuery(yrb,"select distinct ScientificName as Species,VegSpId from tblVegSpeciesList")
spplist

sppnum=merge(luke,spplist,by = 'Species')
luke$Species = sppnum$VegSpId[match(luke$Species,sppnum$Species)]


missing= unique(luke$Species[luke$Species%in%sppnum$Species ==FALSE])
write.csv(missing,'c:/users/vppatil/desktop/luke_badnames.csv')

#a few more to fix. can do that after you get into the lab perhaps.
luke.lakes<-unique(luke$TransectID)

sqlQuery(yrb,"select distinct transectid from tblVegTransectSppList where transectid like '%1_63_1' and fm  = 0  and aq = 0 and mu = 0 and em = 0")
luke.lakes

lake.input=luke[grep('1_63_1',luke$TransectID),]
rows<-dim(lake.input)[1]
lake.input

for(i in 1:rows)
{

	write<-lake.input[i,]
	write$Notes[is.na(write$Notes)]<-''

	sql.entry(write,yrb)
}
