##the objective of this program is to do simple summaries of the 2010 and 2011 yrb datafiles.

#first load the rodbc package
library(RODBC)


#next 2 odbc connections are created, called yrb and yrb2011
yrb<-odbcConnectAccess2007('c:/users/vijay/desktop/yrb/YRBiodiversity.accdb')
yrb2011<-odbcConnectAccess2007('C:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')

table.list<-sqlTables(yrb)

#metadata tables and pct cover (those entered nov 2011) are drawn into datafiles
transects.2010<-sqlFetch(yrb,'tblTransects')
meta<-sqlFetch(yrb,'tblVegTransectMetadata')
meta.2011<-sqlFetch(yrb2011,'tblVegTransectMetadata')
pct.cover<-sqlFetch(yrb,'tblVegTransectPctCover')
pct.cover2011<-sqlFetch(yrb2011,'tblVegTransectPctCover')

#get sorted list of lakes in 2010
lake.list<-as.character(unique(transects.2010$LakeID))
lakes.2011<-c(read.csv('c:/users/vijay/desktop/yrb/2011lakes.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)[,1])
lakes.2010<-lake.list[lake.list%in%lakes.2011==FALSE]
lakes.2010<-sort(lakes.2010)
lakes.2011<-sort(lakes.2011)

#now the goal is to make a function that will step through each of the lakes in lake list.
# 1) get a list of all transectIDs from Metadata that match that lake.
# 2) record that number
# 3) make a dataframe that shows that number

#second function
# using the list of transects 
# step through sql queries from metadata of all those transects

#match function
#should return a vector of matching transect ids
lakeID.match<-function(lake,table)
{
	#lake is a lake id string
	#table is the table to search in
	#the TransectID col should have the same name regardless
	
	t.matches<-grep(lake,table$TransectID)
	transects<-table$TransectID[t.matches]
	
	return(transects)
}

#function to put single quotes around strings for sql queries
query.quote<-function(char) {paste("'",trim(char),"'",sep='')}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#currently designed specifically for metadata table in 2010
l.query<-function(lake.vector,table,con,start)
{
	#lake vector is list of lake names to check
	num.lakes<-length(lake.vector)
	
	for (i in start:num.lakes)
	{
		lake<-lake.vector[i]
		transects<-lakeID.match(lake,table)
	
		#change this bit depending on which year
		#2010
		cols<-c('TransectID','MU_w','EM_w','FM_w','PF_Depth_L','PF_Depth_F','Peat_Depth_L','Peat_Depth_F','PF_Peat_Notes')
		#2011
		#cols<-c('TransectID','MU','EM','FM','PF_GS','PF_Depth_L','PF_Peat_Notes','Notes1')

		col.paste<-paste(cols,collapse=',')
		query.pt1<-paste("SELECT ",col.paste," FROM tblVegTransectMetadata WHERE TransectID in (", sep='')

		transects<-query.quote(transects)
		t.paste<-paste(transects,collapse=',')
		query<-paste(query.pt1,t.paste,")",sep='')
		cat(paste("Lake ID: ",lake,"\n",sep=''))
		q<-sqlQuery(con,query)
		q
		print(q)
		cat('enter to continue, or x to quit')
		quit<-readLines(n=1)
		if (quit=='x') {break()}
	}
}



###most of the stuff below is garbage that should be reevaluated.
