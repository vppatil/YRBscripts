yrb.to.df<-function(path)
{
	library(RODBC)
	yrb<-odbcConnectAccess2007('c:/users/vijay/desktop/yrb/YRBiodiversity.accdb')
	
	#extract all tables from database to a dataframe list
	a<-sqlTables(yrb)
	a<-a$TABLE_NAME[a$TABLE_TYPE == 'TABLE']
	yrb.list<-list()
	for(i in 1:length(a))
	{
		yrb.list[[i]]<-list(a[i],sqlFetch(yrb,a[i]))
	}
	setwd(path)	
	save.image("transferYRB.RData")	
}