#open the conns
library(RODBC)
setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
yrb11<-odbcConnectAccess2007('2011YRBiodiversity.accdb')

#change this to the real version when ready
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

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

#then run dups
lakes<-sqlQuery(yrb,"select distinct lakeid from tbllakebaseinfo")$LakeID

check<-sqlQuery(yrb,"select lakeid,count(transectID) as transectCount from (select distinct lakeid,transectid from (select tbltransects.lakeid,tblvegtransectspplist.* from tbltransects right join tblvegtransectspplist on tbltransects.transectid = tblvegtransectspplist.transectid) where aq = 0 and fm = 0 and mu = 0 and em = 0) group by lakeid")

check2<-sqlQuery(yrb,"select lakeid,count(transectID) as transectCount from (select distinct lakeid,transectid from (select tbltransects.lakeid,tblvegtransectpctcover.* from tbltransects right join tblvegtransectpctcover on tbltransects.transectid = tblvegtransectpctcover.transectid)) group by lakeid")

check3<-sqlQuery(yrb,"select lakeid,count(transectID) as transectCount from (select distinct lakeid,transectid from (select tbltransects.lakeid,tblvegtransectmetadata.* from tbltransects right join tblvegtransectmetadata on tbltransects.transectid = tblvegtransectmetadata.transectid)) group by lakeid")

#get transectids from y11 that are not in final dbase
q = "select lakeid,transectid from tbltransects where transectid not in (select distinct transectid from tblvegtransectspplist where aq = 0 and fm = 0 and mu = 0 and em = 0) order by lakeid"
q2<-"select distinct transectid from tblvegtransectspplist where aq = 0 and fm = 0 and mu = 0 and em = 0"
missings<-sqlQuery(yrb,q2)
compare<-sqlQuery(yrb,"select distinct lakeid,transectid from tbltransects")

in11<-sqlQuery(yrb11,"select distinct transectid from tblvegtransectspplist where transectid in ('154_0_19_1',
'76_0_31_1',
'154_0_8_1',
'244_0_8_1',
'64_0_8_1',
'158_0_87_1',
'248_0_87_1',
'338_0_87_1',
'68_0_87_1',
'154_0_97_1',
'64_0_97_1',
'182_0_97_3',
'2_0_97_3',
'272_0_97_3',
'92_0_97_3',
'1_1_27_2',
'30_1_29_1',
'151_1_32_1',
'241_1_32_1',
'331_1_32_1',
'60_1_32_1',
'43_1_32_5',
'173_1_33_1',
'263_1_33_1',
'353_1_33_1',
'123_1_37_2',
'303_1_37_2',
'33_1_37_2',
'144_1_37_5',
'1A_1_88_1',
'1B_1_88_1',
'2A_1_88_1',
'56_1_90_3'
)")

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

trans<-unique(y11.merge$TransectID)
length(trans)

entered<-sqlQuery(yrb,"select distinct TransectID from tblVegTransectSppList")

length(trans[trans%in%entered$TransectID == FALSE])
missing<-trans[trans%in%entered$TransectID == FALSE]

 check=sqlQuery(yrb,"select transectid,species,gs,ls,ts,dt,df,mf,cf,aq,fm,mu,em,other,notes from tblVegtransectSppList where aq = 0 and fm = 0 and em = 0 and mu = 0",stringsAsFactors=FALSE)
 
 names(check) = names(y11.merge)
 check<-rbind(check,y11.merge)
 str(check)

check<-check[,-15] 
check.p<-apply(check,1,paste,collapse='')
y11.p<-apply(y11.merge[,-15],1,paste,collapse='')

y11.dups=match(check.p,y11.p)


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
i=badname.i[2]

sql.df=y11.merge[badname.i,]
badnames.list<-unique(sql.df$Species)
try<-sql.df[sql.df$Species %in% badnames.list[1],]
sql.entry(try,yrb)

###########################fixes
sqlQuery(yrb11,"update tblvegtransectspplist set bearing  = '244' where bearing = '264' and lakeid = '0_9_1'")
sqlQuery(yrb11,"update tblvegtransectspplist set bearing  = '83' where bearing = '84' and lakeid = '1_36_4'")

#remove blank transectid
sqlQuery(yrb11,"delete from tblvegtransectspplist where lakeid is null or bearing is null")
sqlQuery(yrb11,"delete from tblvegtransectspplist where Species = 'Transect'")
sqlQuery(yrb11,"delete from tblvegtransectspplist where Species is null")
sqlQuery(yrb11,"delete from tblvegtransectspplist where Species = ''")

#remove all rows where sum of habs = 0

#need to fix 1_28_1,1_28_2, and enter 294_1_42_1
#then remove duplicates

#now check the syntax issues.

sql.df<-y11.merge[syntax.i,]
badnames.list<-unique(sql.df$Species)
sql.df[sql.df$Species %in% badnames.list[187],]
check<-function(i)
{
	return(sql.df[sql.df$Species %in% badnames.list[i],])
	}

sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Salix sp.' where Species = 'Salix'")
sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Rumex maritimus' where Species = 'Rumx maritimus'")
sqlQuery(yrb11,"update tblvegtransectspplist set aq = TRUE,Species = 'aquatic algae' where notes  like ('%aquatic%') and Species like '%algae'")
sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Carex sp.' where Species = 'Carex'")
sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Unknown graminoid',notes = 'Carex 2' where Species = 'Carex 2'")

sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Unknown graminoid',notes = 'PI-22' where Species = 'PI-22'")
sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'Unknown',notes = 'PI-7' where Species = 'PI-7'")
sqlQuery(yrb11,"update tblvegtransectspplist set Species = 'purple chlorantha',notes = 'purple chlorantha' where Species = 'purple chlorantha'")

sqlQuery(yrb11,"update tblvegtransectspplist set Species = replace(Species, 'spp.','sp.') where Species like '%spp.%'")

unk<-function(i)
{
	code=badnames.list[i]
	q=paste("update tblvegtransectspplist set notes = '",code,"',Species = 'Unknown' where Species = '",code,"'",sep='')
	sqlQuery(yrb11,q)
}

swap2<-function(old,new)
{
	
	q=paste("update tblvegtransectspplist set Species = '",new,"' where Species = '",old,"'",sep='')
	sqlQuery(yrb11,q)
}

swap<-function(i,new)
{
	old= badnames.list[i]
	q=paste("update tblvegtransectspplist set Species = '",new,"' where Species = '",old,"'",sep='')
	sqlQuery(yrb11,q)
}

swap.notes<-function(i,new)
{
	old=badnames.list[i]
	q=paste("update tblvegtransectspplist set notes = notes+'; ",old,"',Species = '",new,"' where Species = '",old,"'",sep='')
	sqlQuery(yrb11,q)
}

swap.notes2<-function(old,new)
{
	q=paste("update tblvegtransectspplist set notes = notes+'; ",old,"',Species = '",new,"' where Species = '",old,"'",sep='')
	sqlQuery(yrb11,q)
}

itis.check<-function(sppname)
{
	spp.split<-sapply(strsplit(sppname,split=' '),c)
	spp.plus<-paste(spp.split,collapse='+')
	searchstr<-paste("http://www.catalogueoflife.org/annual-checklist/2011/search/all/key/",spp.plus,"/match/1",sep='')
	shell.exec(searchstr)
	#a<-getURL(searchstr)
	#htmlTreeParse(a)
}
	
google.check<-function(sppname)
{
	spp.split<-sapply(strsplit(sppname,split=' '),c)
	spp.plus<-paste(spp.split,collapse='+')
	
	string<-paste("http://www.google.com/search?hl=en&site=&q=",spp.plus,sep='')
	str2<-"&hl=en&client=firefox-a&hs=Tx4&rls=org.mozilla:en-US:official&prmd=imvns&lr=lang_en"
	string<-paste(string,str2,sep='')
	shell.exec(string)
}

sqlQuery(yrb,"select * from tblVegSpeciesList where ScientificName like '%Polygonum%'")
sqlUpdate(yrb11,sql.df
