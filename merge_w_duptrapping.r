#open the conns
library(RODBC)
setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
yrb11<-odbcConnectAccess2007('2011YRBiodiversity.accdb')

#change this to the real version when ready
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

#send in 2011
#read in 2011 data, omitting bad Rows
y11.merge=sqlQuery(yrb11,"select (Bearing + '_' + LakeID) as TransectID,Species,GS,LS,TS,DT,DF,MF,CF,AQ,FM,MU,EM,Other,Notes from tblVegTransectSppList",stringsAsFactors=FALSE)

yesno=matrix(0,dim(y11.merge)[1],12)
habs=y11.merge[,3:14]

dims=dim(habs)
for(i in 1:dims[1])
	{
		for(j in 1:dims[2])
		{
			yesno[i,j]<-ifelse(habs[i,j]==TRUE | habs[i,j] == 1,1,yesno[i,j])
		}
	}

y11.merge[,3:14]=yesno
names(y11.merge)[3:14]=c('GS','LS','TS','DT','DF','MF','CF','AQ','FM','MU','EM','Other')

#now set up query
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}

sql.entry<-function(sql.df,con)
{
	sql.names<-names(sql.df)
	sql.names<-paste(sql.names,collapse=',')
	char.vals<-c(1,15)
	sql.df[,char.vals]<-query.quote(sql.df[,char.vals])
	sql.df$Species = min(sqlQuery(yrb,	paste("select VegSpID as Species from tblVegSpeciesList where ScientificName = ('",sql.df$Species,"')",sep=''))$Species)
	
	vals<-paste(sql.df,collapse=',')
	query<-paste("Insert into tblVegTransectSppList (",sql.names,") VALUES (",vals,")",collapse='')

	q<-sqlQuery(con,query)
	return(q)
}


#now for the loop
errors=vector()
for(i in 1:dim(y11.merge)[1])
fixes=
#for(i in 1:500)
{
	q=sql.entry(y11.merge[i,],yrb)
	
	if(length(nchar(q[1])>0))
		{
			df=data.frame(index=i,error=q[1])
			errors=rbind(errors,df)
		}
}

#some are query syntax, some are wrong transects.
errors=errors[!is.na(errors$error),]
syntax=errors[grep('07002',errors$error),]
badnames=errors[-grep('07002',errors$error),]

syntax.i=syntax$index
badname.i=badnames$index
i=syntax.i[1]
i2=badname.i[1]

sql.df=y11.merge[badname.i,]
badnames.list<-unique(sql.df$Species)

#merge all habitat designations across duplicate rows

q="select transectid,species,count(*) from tblvegtransectspplist group by transectid,species having count(*) > 1"
dups<-sqlQuery(yrb,q)


#now pick one, takeout all records corresponding,sum together
for (i in 1:dim(dups)[1])
{
	mod<-dups[i,]
	q=paste("select gs,ts,ls,dt,df,mf,cf,aq,mu,fm,em,other from tblvegtransectspplist where transectid = '",mod$transectid,"' and species = ",mod$species,sep='')
	habs<-sqlQuery(yrb,q)

	summed<-apply(habs,2,sum)
	summed<-ifelse(summed > 0,1,summed)

	entries<-paste(names(summed),"=",summed,collapse=',')
	up.q=paste("update tblvegtransectspplist set ",entries," where transectid = '",mod$transectid,"' and species = ",mod$species,sep='')
	sqlQuery(yrb,up.q)
}