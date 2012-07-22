library(RODBC)
	yrb2011<-odbcConnectAccess2007('c:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
	yrb<-odbcConnectAccess2007('c:/users/vijay/desktop/yrb/YRBiodiversity.accdb')
	
	#extract all tables from database to a dataframe list
	a<-sqlTables(yrb2011)
	a<-a$TABLE_NAME[a$TABLE_TYPE == 'TABLE']
	yrb2011.list<-list()
	for(i in 1:length(a))
	{
		yrb2011.list[[i]]<-list(a[i],sqlFetch(yrb2011,a[i]))
	}
	save.image("C:\\Users\\Vijay\\Documents\\My Dropbox\\AlaskaFiles\\transferWorkspace\\transferWSpace.RData")
