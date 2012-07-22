library(RODBC)

#check
#sqlQuery(copy,"select TransectID,VegEndEasting,VegEndNorthing from tblTransects where TransectID like '%0_32_1'") 
	

#next 2 odbc connections are created) or TransectID like ( called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
yrb2011<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')

copy=odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversityCopy.accdb')


#read in corrected coords
cc=read.csv(file='c:/users/vppatil/desktop/desktop 2_21/correctedcoords.csv',stringsAsFactors=FALSE)
#make transectid
cc$TransectID=paste(cc$bearing,cc$LakeID,sep='_')

#functions for insert
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}


for (i in 3:dim(cc)[1])
{

index=c(9,10,18)
ends=cc[i,index]
ends$TransectID = query.quote(ends$TransectID)
df.names=names(ends)

query.p1<-paste ("Update tblTransects set VegEndEasting = ",ends$stopeast,sep='')
query.p2<-paste(" where VegEndEasting is NULL and TransectID = ",ends$TransectID,sep='')
query<-paste(query.p1,query.p2,sep='')

a<-sqlQuery(yrb,query)

query2.p1<-paste ("Update tblTransects set VegEndNorthing = ",ends$stopnorth,sep='')
query2.p2<-paste(" where VegEndNorthing is Null and TransectID = ",ends$TransectID,sep='')
query2<-paste(query2.p1,query2.p2,sep='')

b<-sqlQuery(yrb,query2)

print(a)
print(b)
}

#start coords
for (i in 1:dim(cc)[1])
{

index=c(7,8,18)
ends=cc[i,index]
ends$TransectID = query.quote(ends$TransectID)
df.names=names(ends)

query.p1<-paste ("Update tblTransects set VegStartEasting = ",ends$starteast,sep='')
query.p2<-paste(" where TransectID = ",ends$TransectID,sep='')
query<-paste(query.p1,query.p2,sep='')

a<-sqlQuery(yrb,query)

query2.p1<-paste ("Update tblTransects set VegStartNorthing = ",ends$startnorth,sep='')
query2.p2<-paste(" where TransectID = ",ends$TransectID,sep='')
query2<-paste(query2.p1,query2.p2,sep='')

b<-sqlQuery(yrb,query2)

print(a)
print(b)
}

query3 = "Update tblTransects set VegStartNorthing = LakeEdgeNorthing where VegStartNorthing is null and LakeEdgeNorthing is not null"

query4 = "Update tblTransects set VegStartEasting = LakeEdgeEasting where VegStartEasting is null and LakeEdgeEasting is not null"

c=sqlQuery(yrb,query3)
d=sqlQuery(yrb,query4)

q="(select LakeID from tblTransects where TransectID in (select TransectID from tblVegTransectMetadata where PF_Peat_Notes like '%inches%')"
q="select tblTransects.LakeID,tblVegTransectMetadata.* from tblTransects left join tblVegTransectMetadata on tblTransects.TransectID = tblVegTransectMetadata.TransectID where PF_Peat_Notes like '%inches%' order by LakeID"

a<-sqlQuery(yrb,q)
b<-a[!is.na(a$PF_Depth_L),]
c<-subset(b,select=c('LakeID','TransectID','PF_Peat_Notes'))

sqlQuery(yrb,"select TransectID,PF_Peat_Notes from tblVegTransectMetadata where TransectID like '%0_19_5'")


inches.q="Update tblVegTransectMetadata SET PF_Depth_L = PF_Depth_L * 2.54 WHERE PF_Peat_Notes like '%inches%' and PF_Depth_L > 0"
inches.q2="Update tblVegTransectMetadata SET Peat_Depth_L = Peat_Depth_L * 2.54 WHERE PF_Peat_Notes like '%inches%' and Peat_Depth_L > 0"
inches.q3="Update tblVegTransectMetadata SET PF_Depth_F = PF_Depth_F * 2.54 WHERE PF_Peat_Notes like '%inches%' and PF_Depth_F > 0"
inches.q4="Update tblVegTransectMetadata SET Peat_Depth_F = Peat_Depth_F * 2.54 WHERE PF_Peat_Notes like '%inches%' and Peat_Depth_F > 0"
inches.q5="update tblVegTransectMetadata set PF_Peat_Notes = PF_Peat_Notes & ';converted'"

sqlQuery(yrb,inches.q)
sqlQuery(yrb,inches.q2)
sqlQuery(yrb,inches.q3)
sqlQuery(yrb,inches.q4)
sqlQuery(yrb,inches.q5)


##################################################################################

#Percent Merge


###############################################################################

library(labdsv)

codes=c('-','t','1','2','3','4','5')
nums=c(0,.5,5.5,17.5,37.5,62.5,87.5)

cbind(names(sqlFetch(yrb2011,'tblVegTransectPctCover')))

pct=sqlQuery(yrb2011,"select * from tblVegTransectPctCover",stringsAsFactors=FALSE)
oldpct=sqlQuery(yrb,"select * from tblVegTransectPctCover",stringsAsFactors=FALSE)


fortrans=pct[,6:16]
fortrans[fortrans == '-+']='-'
fortrans[fortrans == '0']='-'
fortrans[fortrans == '']='-'
fortrans[fortrans == '1.5']='1'
fortrans[fortrans == 'Na']='0'

fortrans=fortrans[,-9]
fortrans=fortrans[!is.na(fortrans[,1]),]
veg=vegtrans(fortrans,codes,nums)

veg$Non_Vasc=veg$Sphag+veg$Non_Sphag+veg$Lichen
veg=veg[,-c(1:3)]

back<-function(x)
{
	mins=c(0,0,1,10,25,50,75)
	maxs=c(0,1,10,25,50,75,100)
	for(i in 1:7)
	{
		if(x > mins[i] & x <= maxs[i])
			code=codes[i]
	}
	return(code)
}

veg[is.na(veg)]=0
new=data.frame()

new=apply(veg,c(1,2),back)
new=data.frame(new)

a=pct[row.names(pct)%in%row.names(new),]

new$TransectID=a$TransectID
new$HabitatType=a$HabitatType
new$Start=a$Start
new$End=a$End

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}
errors=data.frame()
for (i in 1:dim(updat)[1])
{
	new=updat[i,]
	nums=c(1,2,6:14)
	df.names=names(new)
					for (i in 1:length(df.names))
					{
						if (i%in%nums)
						{
							new[,i]=query.quote(as.character(new[,i]))
						} else {new[,i]=as.character(new[,i])}
					}
	
		query.pctc1<-paste("INSERT INTO tblVegTransectPctCover(",trim(paste(df.names,collapse=',')),")",collapse='')
					
		pc.vals.forquery<-paste(new[,1:length(df.names)],collapse=',')
		query<-paste(query.pctc1," VALUES(",pc.vals.forquery,")",sep='')
									
		test=sqlQuery(yrb,query)	
		test
		if(length(nchar(test))>0)
		{
			errors<-rbind(errors,new)
		}
}

#now check for duplicates
r=r+1
new=errors[r,]
		pc.vals.forquery<-paste(new[,1:length(df.names)],collapse=',')
		query<-paste(query.pctc1," VALUES(",pc.vals.forquery,")",sep='')
									
		test=sqlQuery(yrb,query)	
		test

updated=sqlQuery(yrb,"select *  from tblVegTransectPctCover")
d=duplicated(updated[,1:4])


switched1="select TransectID,VegStartEasting,VegStartNorthing from tblTransects where VegStartNorthing<VegStartEasting"
sw.df<-sqlQuery(yrb,switched1)
sw.update=data.frame(TransectID=sw.df$TransectID,VegStartNorthing=sw.df$VegStartEasting,VegStartEasting=sw.df$VegStartNorthing)

sqlUpdate(yrb,sw.update,tablename='tblTransects',index='TransectID')



switched2="select TransectID,VegEndEasting,VegEndNorthing from tblTransects where VegEndNorthing<VegEndEasting"
sw.df2<-sqlQuery(yrb,switched2)
sw.update2=data.frame(TransectID=sw.df2$TransectID,VegEndNorthing=sw.df2$VegEndEasting,VegEndEasting=sw.df2$VegEndNorthing)

sqlUpdate(yrb,sw.update2,tablename='tblTransects',index='TransectID')


pct<-sqlQuery(yrb,"select *  from tblVegTransectPctCover")
pct$LakeID=pct$LakeID<-sapply(strsplit(as.character(pct$TransectID),split='_'),function(x) paste(x[2:4],collapse='_'))
pt<-table(pct$LakeID)

lakes=sqlQuery(yrb,"select distinct LakeID from tblTransects",stringsAsFactors)$LakeID
entered = row.names(pt)
lakes[lakes%in%entered == FALSE]

missing coords=	