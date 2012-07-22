# meta
# - convert
# - insert
# pct
# - convert
# - insert

library(RODBC)

#next 2 odbc connections are created) or TransectID like ( called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
yrb2011<-odbcConnectAccess2007('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')

#compare names for meta, old meta

newmeta=names(sqlFetch(yrb2011,'tblVegTransectMetadata'))
oldmeta = names(sqlFetch(yrb,'tblVegTransectMetadata'))

cbind(newmeta)
cbind(oldmeta)

#1) drop the _GS_SH_FOR layers
#change MU FM EM to _w
#drop gthan, lthan

newmetaq = "select TransectID,Observer1,Observer2,TransectDate as VegTransectDate,TransectTime as VegTransectTime,PF_DEPTH_L,PF_DEPTH_FE,FIB_DEPTH_L,HUM_DEPTH_L,FIB_DEPTH_FE,HUM_DEPTH_FE,PF_Peat_Notes,MU as MU_w,EM as EM_w,FM as FM_w,PestsPathogens,GrowthAnomalies,Phenology,AnimalUse,Disturbance,Drawings as Drawing,Notes1 from tblVegTransectMetadata"

nm=sqlQuery(yrb2011,newmetaq)

#fix drawing datatype
nm$Drawing<-as.character(nm$Drawing)
drawing=ifelse(is.na(nm$Drawing)|nm$Drawing == "0",0,1)
nm$Drawing=drawing

#fix cases
cbind(names(nm))
names(nm)[c(6:7)]=c('PF_Depth_L','PF_Depth_F')

#fix the hum/fib
nm$Peat_Depth_L = -1
nm$Peat_Depth_F = -1
for(i in 1:length(nm$TransectID))
{
	nm$Peat_Depth_L[i] = ifelse(nm$FIB_DEPTH_L[i] <0 & nm$HUM_DEPTH_L[i] < 0 ,-1,max(na.omit(c(nm$FIB_DEPTH_L[i],nm$HUM_DEPTH_L[i]))))
	nm$Peat_Depth_F[i] = ifelse(nm$FIB_DEPTH_FE[i] <0 & nm$HUM_DEPTH_FE[i] < 0 ,-1,max(na.omit(c(nm$FIB_DEPTH_FE[i],nm$HUM_DEPTH_FE[i]))))
}

nm=nm[,c(1:7,23,24,12:22)]

copy=odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversityCopy.accdb')
#now names are rearranged, fib/hum are merged.
#make insert into statement

#get string names

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}

errors=data.frame()
r=2
for (r in 7:length(nm$TransectID))
{	
	new=nm[r,]
	nums=c(1:5,10,14:18,20)
	df.names=names(new)
					for (i in 1:length(df.names))
					{
						if (i%in%nums)
						{
							new[,i]=query.quote(as.character(new[,i]))
						} else {new[,i]=as.character(new[,i])}
					}
		
		new$VegTransectTime=ifelse(new$VegTransectTime=="'NA'","NULL",new$VegTransectTime)
		new$VegTransectDate=ifelse(new$VegTransectDate=="'NA'","NULL",new$VegTransectDate)
		new$MU_w=ifelse(new$MU_w == 'a','-1',new$MU_w)
		new$FM_w=ifelse(new$FM_w == 'a','-1',new$FM_w)
		new$EM_w=ifelse(new$EM_w == 'a','-1',new$EM_w)
		
		query.pctc1<-paste("INSERT INTO tblVegTransectMetadata(",trim(paste(df.names,collapse=',')),")",collapse='')
					
		pc.vals.forquery<-paste(new[,1:length(df.names)],collapse=',')
		query<-paste(query.pctc1," VALUES(",pc.vals.forquery,")",sep='')
					
		test=sqlQuery(yrb,query)	
		test
		if(length(nchar(test))>0)
		{
			errors<-rbind(errors,new)
		}
}

#try 1 at a time.

	num.e=dim(errors)[2]
	errors2=data.frame()
	
	for(i in 1:num.e)
	{
	#	i=i+1
		new=errors[i,]
		
		new$VegTransectTime=ifelse(new$VegTransectTime=="'NA'"| new$VegTransectTime == "':'","NULL",new$VegTransectTime)
		new$VegTransectDate=ifelse(new$VegTransectDate=="'NA'" | new$VegTransectDate == "':'","NULL",new$VegTransectDate)
		new$MU_w=ifelse(new$MU_w == 'a','-1',new$MU_w)
		new$FM_w=ifelse(new$FM_w == 'a','-1',new$FM_w)
		new$EM_w=ifelse(new$EM_w == 'a','-1',new$EM_w)
		
		
		end=20
			query.pctc1<-paste("INSERT INTO tblVegTransectMetadata(",trim(paste(df.names[1:end],collapse=',')),")",collapse='')
						
			pc.vals.forquery<-paste(new[,1:length(df.names[1:end])],collapse=',')
			query<-paste(query.pctc1," VALUES(",pc.vals.forquery,")",sep='')
						
			test=sqlQuery(yrb,query)	
		if(length(nchar(test))>0)
			{
				errors2<-rbind(errors2,new)
			}
			print(test)
	}

	new=errors2[1,]