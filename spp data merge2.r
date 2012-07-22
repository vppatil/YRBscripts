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

query<-"select TransectID,VegSpID as Species,GS,LS,TS,DT,DF,MF,CF,AQ,FM,MU,EM,Other,Notes from (select tblVegSpeciesList.ScientificName,tblVegSpeciesList.VegSpID,tblVegTransectSppList.*,(tblVegTransectSppList.Bearing + '_' + tblVegTransectSppList.LakeID) as TransectID from tblVegSpeciesList left join tblVegTransectSppList on tblVegSpeciesList.ScientificName = tblVegTransectSppList.Species where tblVegTransectSppList.LakeID = '1_')"

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

tblVegTransectSppList.