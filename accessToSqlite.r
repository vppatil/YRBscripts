
access.to.sqlite<-function(accdb,sqliteDb)
{
	library (RODBC)
	yrb<-odbcConnectAccess2007(accdb)
	tables<-sqlTables(yrb)$TABLE_NAME

	#use tables 18 through 61, or possibly just stuff starting with tbl +
	#lakechemcharacteristics

	#access the sqldbase
	library(RSQLite)
	drv<-dbDriver('SQLite')
	con<-dbConnect(drv,sqliteDb)
	
	cat("Replace current contents of yrb dbase?")
	choice=readLines(n=1)


	yrb.datalist<-list()
	for(i in 18:61)
	{
		yrb.datalist[[i]]<-sqlFetch(yrb,tables[i])
		if(choice == 'Y' | choice == 'y')
		{
			drop.q<-paste("drop table ",tables[i],sep='')
			dbGetQuery(con,drop.q)
		}
		
		dbWriteTable(con,tables[i],yrb.datalist[[i]],row.names=FALSE)
	}

	odbcCloseAll()

}
access.to.sqlite('C:/users/vppatil/desktop/yrb/yrbiodiversity.accdb','C:/users/vppatil/ubuntu one/sqlite/yrb.db')
