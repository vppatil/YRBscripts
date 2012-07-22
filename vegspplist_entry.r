#also continue inserting into xl file, try using read.xls
#not working will need to merge this late somehow.
#still need to add spp to transect spp list- can do that later
#note spp autonum is all fucked up
#when merging with the full dataset, you will need to leave the num col out of the sql insert statement
#also, will need to remove all false spp.
library(RODBC)

#finally work on itis check
veg.spp.entry<-function()
{

	
	#setwd and open connection
	#yrb2011<-ht.odbc()
	#yrb2011<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
	yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')
	cat("New Row? ")
	choice<-readLines(n=1)
	entry.df<-data.frame()
	
	while(choice=='y'|choice=='Y')
	{
		#get spp.list
		spp.list.all<-spp.list.make(yrb)
		
		transect<-sqlFetch(yrb2011,'tblTransects')
		
		
		#get transect info
		TID.df<-TransectIDinput(transect,entry.df)
		#need transect validation
		
		#get species name & id
		Species.df<-species.entry(spp.list.all,yrb,yrb2011)
		
		#GetHabitatType checklist
		#two options
		Ht.df<-HabitatTypeInput2()
		
		Notes<-NotesInput()
		
		entry.df<-data.frame(TID.df,Species.df,Ht.df,Notes,stringsAsFactors=FALSE)
		
		cat("Need to correct mistakes? (Y/N)")
		mistake<-readLines(n=1)
		if(mistake=='Y'|mistake=='y')
		{entry.df<-edit(entry.df)}
	
		sql.entry(entry.df,yrb2011)
		
		cat("New Row? ")
		choice<-readLines(n=1)
	}
	
	odbcCloseAll()
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
spp.list.make<-function(oldcon)
		{
			spp.list<-sqlQuery(oldcon,"select distinct(ScientificName),VegSpID from tblVegSpeciesList")
			spp.list.all<-spp.list
			return(spp.list.all)
		}

sql.entry<-function(sql.df,con)
{
	sql.names<-names(sql.df)
	sql.names<-paste(sql.names,collapse=',')
	char.vals<-c(1:3,5:17)
	sql.df[,char.vals]<-query.quote(sql.df[,char.vals])
	vals<-paste(sql.df,collapse=',')
	query<-paste("Insert into tblVegTransectSppList (",sql.names,") VALUES (",vals,")",collapse='')

	q<-sqlQuery(con,query)
	q
	print(q)
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


	

species.entry<-function(allspp,oldcon,newcon)
{
#allspp is df version of tblVegSpeciesList
	cat("Enter species name: \n")
	spp<-readLines(n=1)
	spp<-.simpleCap(spp)
	while((spp %in% unique(allspp$ScientificName))==FALSE)
	{
		cat("Species not previously entered\n")
		
		cat("Search partial string?")
		s.p<-readLines(n=1)
	
		
		if(s.p == 'y' | s.p == 'Y')
		{
			#cat("how many letters? ")
			#lett<-as.numeric(readLines(n=1))
			spp.sub<-substr(spp,1,3)
			spp.sub.search<-(allspp$ScientificName[grep(spp.sub,allspp$ScientificName)])
			print(cbind(as.character(spp.sub.search)))
			cat("Enter choice or 0 if not present")
			choice<-as.numeric(readLines(n=1))
			spp<-ifelse(choice==0,{cat("Enter Species name: \n");spp=readLines(n=1)},as.character(spp.sub.search[choice]))
			spp<-.simpleCap(spp)
		} else
		{
			cat("check itis?")
			check.itis<-readLines(n=1)
			if(check.itis == 'y' | check.itis == 'Y')
			itis.check(spp)
			
			cat("check google?")
			check.google<-readLines(n=1)
			if(check.google == 'y' | check.google == 'Y')
			google.check(spp)
			
				cat("Keep entered name?")
				keep<-readLines(n=1)
				if (keep == 'n' | keep == 'N')
				{
					cat("Enter correct species name: \n")
					sppname<-readLines(n=1)
					spp<-.simpleCap(sppname)
				} else if(keep == 'y' | keep=='Y')
				{
							add<-data.frame(SpeciesName=spp)
							write.csv(add,'addedNames2011.csv')
							break()
				}
		
		
		} 
		
			
		
		
	}	
	ID<-sqlQuery(oldcon,paste("select min(VegSpID) from tblVegSpeciesList where ScientificName = '",spp,"'",sep=''))[1]
	
	if (is.na(ID))
	{
		ID=sqlQuery(newcon,"select max(TransectSppAutonumber) from tblVegTransectSppList")+1
	}
	names(ID)<-'TransectSppAutonumber'
	
	name.df<-data.frame(Species=spp,ID,stringsAsFactors=FALSE)
	return(name.df)
}

ht.odbc<-function()
{
	
	
	cat("use standard wd? ")
	wd<-readLines(n=1)
	if(wd=='y'|wd=='Y')
	{
		setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
	} else 
	{
		cat('enter the working directory, ending with a /')
		path<-readLines(n=1)
	}
	
	cat("Use the 2011YRBiodiversity file? ")
	use.std<-readLines(n=1)
	if(use.std=='y'|use.std=='Y')
		filename<-"2011YRBiodiversity.accdb"
	else 
	{
		cat("Enter filename with extension")
		filename=readLines(n=1)
		filename<-paste(path,filename,sep='')
	}
	
	con<-odbcConnectAccess2007(filename)
}

HabitatTypeInput2<-function()
{
	cat("Enter the number or code for all habitat types\n")

	ht.n<-c('GS','LS','TS','DT','DF','MF','CF','AQ','FM','MU','EM','Other')
	ht.c<-c('g','l','t','dt','df','m','c','a','f','mu','e','o')

	code.df<-data.frame(ht.n,ht.c)
	print(code.df)
	
	codes<-scan(what='char')

	h.bool<-vector()
	h.bool<-(ht.c %in%codes | as.character(1:length(ht.c)) %in% codes)

	ht.df<-data.frame(rbind(as.character(h.bool)),row.names=FALSE,stringsAsFactors=FALSE)
	names(ht.df)<-ht.n
	
		
		
	return(ht.df)
}
HabitatTypeInput<-function()
{
	
	
	ht.num<-length(ht.names)
	ht.df<-vector()

	cat("Enter the data below, as prompted\n")
	for(i in 1:ht.num)
	{
		cat(paste(ht.names[i],": "))
		ht.input<-readLines(n=1)
		ht.df<-cbind(ht.df,ht.input)
	}
	
	ht.df<-data.frame(ht.df=='1')
	names(ht.df)<-ht.names
	return(ht.df)
}

		
TransectIDinput<-function(transects.df,entry.df)
{
		vars<-c('Stratum','Lakenum','Focalnum','Transect')
		inputvals<-list()
		numvars<-length(vars)

		cat("Use most recent TransectID? (Y/N)\n")
		uselast<-readLines(n=1)
		
		if(uselast=='Y' | uselast=='y')
		{
			last.transect<-paste(entry.df[,2],entry.df[,1],sep='_')
			last.vals<-data.frame(TransectID=last.transect,LakeID=entry.df[,1],Bearing=entry.df[,2])
			print(last.vals)
		
			TransectID=last.vals$TransectID
			LakeID=last.vals$LakeID
			Bearing=last.vals$Bearing
		} else
		{
		
			cat("Enter the data below, as prompted\n")
			for(i in 1:numvars)
			{
				cat(paste(vars[i],": "))
				inputvals[[i]]<-readLines(n=1)
			}	
				#get complete TransectID
				TransectID<-paste(inputvals[[4]],inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
				
				#get lake id too
				LakeID<-paste(inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
				
				Bearing=inputvals[[4]]
				
				#ID=inputvals[[3]]

			
		}

		
		t.check<-TransectID%in% transects.df$TransectID
		

		while	 (!t.check)
		{
				cat("transect has not been entered")
		
			cat("Enter the data below, as prompted\n")
			for(i in 1:numvars)
			{
				cat(paste(vars[i],": "))
				inputvals[[i]]<-readLines(n=1)
			}

			#get complete TransectID
			TransectID<-paste(inputvals[[4]],inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			
			#get lake id too
			LakeID<-paste(inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			
			Bearing=inputvals[[4]]
		
			
			t.check<-TransectID%in% transects.df$TransectID
		
		}
		LakeID<-as.character(LakeID)
		Bearing=as.character(Bearing)
		
		Transect.df<-data.frame(LakeID,Bearing,stringsAsFactors=FALSE)
		return(Transect.df)
}

NotesInput<-function()
{
	cat("Enter Notes for species richness: \n")
	Notes<-readLines(n=1)
	return(Notes)
}
			
			
			