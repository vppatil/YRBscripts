#program to enter data into tblVegMetadata

#use readline() function
#for strings (prompt = )







#use scan(file='',nmax=1)
#for numbers

#to append, make a new data.frame that is blank, but with the same names as the current one.
#after you get all of the variable names
#then do a simple rbind

#first step is to set the working directory


	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	query.quote<-function(char) {paste("'",trim(char),"'",sep='')}


	readfile<-function(file.name)
	{
		cat("set the user directory? enter y/n\n")
		ask<-readLines(n=1)

		if (ask == 'y'|ask=='Y')
		{	
			#can put in a series of options later
			cat("Enter the full path, ending with a /\n")
			wdir<-trim(readLines(n=1))
		} else wdir<-''

		cat("Enter the filename\n")
		file.name<-trim(readLines(n=1))

		file.name<-filename.check(file.name)

		file.name<-paste(wdir,file.name,sep='')
		dat<-read.csv(file.name,header=TRUE,sep=',',stringsAsFactors=FALSE)

		return(list(dat,file.name))
	}

pct.cover.entry<-function()
{
	

		#get year for later use
		
		cat("What year?")
			Year<-as.numeric(readLines(n=1))
		if (Year==2010)
		{
			yrb<-odbcConnectAccess2007("C:/users/vppatil/Desktop/YRB/YRBiodiversity.accdb")
			old<-sqlFetch(yrb,'tblVegTransectPctCover')
			transects<-sqlFetch(yrb,'tblTransects')
		} else if (Year == 2011)
		{
			yrb<-odbcConnectAccess2007("C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb")
			dat<-readfile()
			old<-sqlFetch(yrb,'tblVegTransectPctCover')
		}	
	
		cat("Do you have the lock on the database?")
			lock<-readLines(n=1)
			
	
	
	library(RODBC)

#this connection should be changed to the real db if you are planning on updating properly

	#change filename here until you can implement a function for user entry
	

	meta<-sqlFetch(yrb,'tblVegTransectMetadata')
	pct.cov<-sqlFetch(yrb,'tblVegTransectPctCover')

	
	len<-length(old$TransectID)
	
	last.transect=old$TransectID[len]
	
	cat("New Row? (Y/N)\n")
	choice<-readLines(n=1)
	
	while(choice=='y'|choice=='Y')
	{
		
		vars<-c('Stratum','Lakenum','Focalnum','Transect')
		inputvals<-list()
		numvars<-length(vars)
		
		cat("Use most recent TransectID? (Y/N)\n")
		uselast<-readLines(n=1)
		
		if(uselast=='Y' | uselast=='y')
		{
			TransectID=last.transect
		} else{
		
			cat("Enter the data below, as prompted\n")
			for(i in 1:numvars)
			{
				cat(paste(vars[i],": "))
				inputvals[[i]]<-readLines(n=1)
			}

			#get complete TransectID
			TransectID<-paste(inputvals[[4]],inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			last.transect=TransectID
		}
		
		if(Year==2010)
		{
			t.check<-TransectID%in% transects$TransectID
		} else t.check=TRUE
		m.check<-TransectID%in% meta$TransectID
	
	#transect check:
		
		while	 (!(m.check)|((!t.check)&Year==2010))
		{
				cat("transect has not been entered in metadata or transect table. ")
		
			cat("Enter the data below, as prompted\n")
			for(i in 1:numvars)
			{
				cat(paste(vars[i],": "))
				inputvals[[i]]<-readLines(n=1)
			}

			#get complete TransectID
			TransectID<-paste(inputvals[[4]],inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			
		
		}
		
		cat("Habitat Type: \n")
		habs<-c('GS','LS','TS','DT','DF','MF','CF','Other')
		numvars<-length(habs)
		
		for(i in 1:numvars)
		{
			cat(paste(i,": ",habs[i],"\n"))
		}
		
		Hab.num<-readLines(n=1)
		Hab.num<-as.numeric(Hab.num)
		
		if(Hab.num==numvars)
		{HabitatType=readLines(n=1)} else { HabitatType=habs[Hab.num]}
		
		#check for notes
			cat("notes: \n")
			Notes<-readLines(n=1)
		
		cat("Start: \n")
		Start=as.numeric(readLines(n=1))
		
		cat("End: \n")
		End=as.numeric(readLines(n=1))
		
		HabitatTypeLength=End-Start
		
		cat("Percent Cover by Function type: \n")
		cat("1 2 3 4 5 t -\n")
		
		#swtich categories depending on year
		types.2011=c('Sphagnum','Non-Sphagnum','lichen','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','Equisetum','dead wood','bare ground')
		types.2010<-c('Non-Vasc','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','dead wood','bare ground')
		
	if (Year==2010) {types=types.2010} else {types=types.2011}
		
		#get cover data
		cover<-vector()
		for (i in 1:length(types))
		{
			cat(paste(types[i],": "))
			cover[i]=readLines(n=1)
		}
		
		#create dataframe for entry
		if (Year==2011)
		{
			new<-data.frame(TransectID,HabitatType,HabitatTypeLength,Start,End,Sphag=cover[1],Non_Sphag=cover[2],Lichen = cover[3],Gram=cover[4],Forb=cover[5],EG_Shrub=cover[6],Dec_Shrub=cover[7],Tree=cover[8],Eq=cover[9],DW=cover[10],Bare=cover[11],Notes,stringsAsFactors=FALSE)
			nums<-c(3:5)
			
		} else 
		{
			new=data.frame(TransectID,HabitatType,Start,End,HabitatTypeLength,HabitatDistanceNotes='',NonVasc=cover[1],Gram=cover[2],Forb=cover[3],
						EG_Shrub=cover[4],Dec_Shrub=cover[5],Tree=cover[6],DW=cover[7],Bare=cover[8],Notes,stringsAsFactors=FALSE)
			nums<-c(3:4)
		}
			
		df.names<-names(new)
		
		cat("Need to correct mistakes? (Y/N)")
		mistake<-readLines(n=1)
	
	
		
	
		if(mistake=='Y'|mistake=='y')
		{new<-edit(new)}
			
		if (Year==2011)
		{
			#write to csv
		#old<-rbind(old,new)
		#write.csv(old,file=file.name,row.names=FALSE)
		}
	#write to access2007 file
	#file is called yrb
	
	#function to stick quotes on appropriate names

	#new should be changed based on year already, and the appropriate database selected
	if (lock=='y'|lock=='Y')
			{
				
				for (i in 1:length(df.names))
				{
					if (!i%in%nums)
					{
						new[,i]=query.quote(as.character(new[,i]))
					} else {new[,i]=as.character(new[,i])}
				}
				

				
				query.pctc1<-paste("INSERT INTO tblVegTransectPctCover(",trim(paste(df.names,collapse=',')),")",collapse='')
				
				pc.vals.forquery<-paste(new[,1:length(df.names)],collapse=',')
				query<-paste(query.pctc1," VALUES(",pc.vals.forquery,")",sep='')
				
				
				q<-sqlQuery(yrb,query=query)
				q
				cat(q)
			
			} else { cat("These data will be written to the excel file, but not to access.")}
		
			
			cat("New Row? (Y/N)\n")
			choice<-readLines(n=1)
	}
	
	odbcCloseAll()
}



	filename.check<-function(name)
	{
		while(length(grep('.csv',name))<1)
		{
			print('filenames must in in .csv')
			cat("Enter the filenamentre\n")
			name<-trim(readLines(n=1))
		}
		return(name)
	}

