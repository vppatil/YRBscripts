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

	readfile<-function(file.name)
	{
		cat("set the user directory? enter y/n\n")
		ask<-readLines(n=1)

		if (ask == 'y'|ask=='Y')
		{	
			#can put in a series of options later
			cat("Use default path? (dropbox/alaska files/2011 datasheets protocols/)")
			default.choice<-readLines(n=1)
			if(default.choice=='Y'|default.choice=='y')
			{
				wdir<-'C:/users/vppatil/Dropbox/AlaskaFiles/2011 datasheets protocols/'
			} else{
			
					cat("Enter the full path, ending with a /\n")
					wdir<-trim(readLines(n=1))
				}
		} else wdir<-''

		cat("Enter the filename\n")
		file.name<-trim(readLines(n=1))

		file.name<-filename.check(file.name)

		file.name<-paste(wdir,file.name,sep='')
		dat<-read.csv(file.name,header=TRUE,sep=',',stringsAsFactors=FALSE)

		return(list(dat,file.name))
	}

entry<-function()
{
	
	dat<-readfile()
	file.name=dat[[2]]
	old<-dat[[1]]
	cat("Do you have the lock on the database?")
			lock<-readLines(n=1)
			
	
	cat("New Row? (Y/N)\n")
	choice<-readLines(n=1)
	
	library(RODBC)

#this connection should be changed to the real db if you are planning on updating properly

	cat("What Year?")
	Year<-as.numeric(readLines(n=1))
	
		test<-odbcConnectAccess2007("C:/users/vppatil/Desktop/YRB/YRBiodiversity.accdb")
	
		transects<-sqlFetch(test,'tblTransects')
		lakes<-sqlFetch(test,'tblLakeDescription')
if (Year==2011)
{
	odbcClose(test)
	test<-odbcConnectAccess2007("C:/users/vppatil/Dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb")
}	
		meta<-sqlFetch(test,'tblVegTransectMetadata')
		pct.cov<-sqlFetch(test,'tblVegTransectPctCover')

	while(choice=='y'|choice=='Y')
	{
		
		vars<-c('Stratum','Lakenum','Focalnum','Transect')
		inputvals<-list()
		numvars<-length(vars)

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
		
		#now check to see 1) if the transect id has been entered in both table transects and table transectmetadata, and 2) if the lake id is valid
		# if the lake id isn't right
		# return an error and ask to try again
		# if the transect id is in transect metadata, say that it has already been entered, loop back to entry and transect id
		# if it is not in metadata, but it is in transects, get extra entry data for tbl transects
		
		t.check<-TransectID%in% transects$TransectID
		m.check<-TransectID%in% meta$TransectID
		l.check<-LakeID%in% lakes$LakeID
		

		while	 (!l.check | m.check)
		{
				cat("Lake not found or transect has already been entered")
		
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
		}
		
		#set up the import directly into tblTransects
		if (!t.check & Year==2010)
		{		
			cat("Transect Data needs to be entered into table transects")
			
			
			if (lock=='y'|lock=='Y')
			{
				#need LakeID, TransectID, Azimuth, LakeEdgeEasting, LakeEdgeNorthing, EdgeElevation=0, VegStartEasting=0,VegStartNorthing=0, VegEndEasting,VegEndNorthing,VegBearingCenter,VegBearingEnd,Notes
				#TRansect and Lake id already entered
				cat("\nBearing to lake center")
				bearing.center<-readLines(n=1)
				
				cat("Bearing to lake edge")
				bearing.edge<-readLines(n=1)
				
				cat("Lake Edge Easting")
				lake.east<-readLines(n=1)
				
				cat("Lake Edge Northing")
				lake.north<-readLines(n=1)
				
				cat("End Easting")
				end.easting<-readLines(n=1)
				
			
				cat("End Northing")
				end.northing<-readLines(n=1)
				
				cat("tblTransect Notes")
				transect.notes<-readLines(n=1)
				
				transect.names<-names(transects)
				tt<-data.frame(LakeID,TransectID,bearing.center,transect.notes)

						cat("Need to correct mistakes? (Y/N)")
						mistake<-readLines(n=1)
	
						if(mistake=='Y'|mistake=='y')
						{edit(tt)}
				
				
				
				query.pt1<-paste("INSERT INTO tblTransects(",trim(paste(transect.names,collapse=',')),")",collapse='')
				query.quote<-function(char) {paste("'",trim(char),"'",sep='')}
				LakeID<-query.quote(tt$LakeID)
				TransectID<-query.quote(tt$TransectID)
				Azimuth<-query.quote(tt$bearing.center)
				t.notes<-query.quote(tt$transect.notes)
				
				t.vals.forquery<-paste(LakeID,TransectID,Azimuth,lake.east,lake.north,0,0,0,end.easting,end.northing,bearing.center,bearing.edge,t.notes,sep=',')
				query<-paste(query.pt1," VALUES(",t.vals.forquery,")",sep='')
				sqlQuery(test,query=query)
			} else {cat("you'll have to enter this into transects later")}
			
		}		
			
			
		
			
		
			
		#now observers
		observers<-c('Connor Jandreau','Laurenz Teuber','Alex Simon','Kristen McElligot','vppatil Patil','Katy Pieczora','Melody S. Durrett','Wesley Sarmento')
		cat("\nObserver: \n")
		for(i in 1:length(observers))
		{
			cat(paste(i,": ",observers[i],"\n",sep=''))
		}

		choice=readLines(n=1)
		Observer1<-observers[as.numeric(choice)]

		#now get the date
		vars<-c('Month','Day')
		inputvals<-list()
		numvars<-length(vars)

		cat("Enter the date below, as prompted\n")
		for(i in 1:numvars)
		{
			cat(paste(vars[i],": "))
			inputvals[[i]]<-readLines(n=1)
		}

		Date<-paste(inputvals[[1]],inputvals[[2]],Year,sep='/')
		Month<-inputvals[[1]]

		cat("Hour: ")
		Hour<-readLines(n=1)

		cat("Minutes: ")
		Minutes<-readLines(n=1)
		
		Time<-paste(Hour,Minutes,sep=':')
		
		vars<-c('Floating Mat','Emergent','Mud Flat')
		inputvals<-list()
		numvars<-length(vars)

		cat("Enter the widths of the following zones:\n")
		for(i in 1:numvars)
		{
			cat(paste(vars[i],": "))
			inputvals[[i]]<-readLines(n=1)
		}

		FM=inputvals[[1]]; EM = inputvals[[2]]; MU = inputvals[[3]]

		#check for notes
			cat("Field Notes? (Y/N)")
			ask.notes<-readLines(n=1)

			vars<-c('pests/pathogens','growth anomalies','Phenology','animal use','disturbance','drawings')
			numvars<-6
			
			if (ask.notes=='Y' | ask.notes=='y')
			{
				inputvals<-list()
				

				cat("Enter the field notes:\n")
				for(i in 1:numvars)
				{
					cat(paste(vars[i],": "))
					inputvals[[i]]<-readLines(n=1)
				}

				field.notes<-vector()
				
				for(i in 1:6)
				{
					field.notes[i]<-inputvals[[i]]
				}
				
				
			} else field.notes=c(rep('',5),'0')
			
			PestsPathogens=field.notes[1]
			GrowthAnomalies=field.notes[2]
			Phenology=field.notes[3]
			AnimalUse=field.notes[4]
			Disturbance=field.notes[5]
			Drawings=field.notes[6]

		#determine if this is a soil sampling or other transect
		if(Year!='2010')
		{
		
			cat("What kind of transect was this? ")
			cat("\n1: Soil Sampling Transect")
			cat("\n2: Other Terrestrial Transect")
			
			choice=readLines(n=1)
		} else choice='2'
		
		#same set of variables regardless
		layers=c('permafrost','fibric layer','humic layer')

		#need zeros regardless
		soilt.mat<-matrix(-1,3,3)

		#
		othert.mat<-matrix(-1,3,2)

		

		cat('Enter the layer depths, or -1 if no data\n')

		if (choice=='1')
		{
			communities<-c('Grass/Sedge ','Shrub ','Forest ')
			
			for (i in 1:3)
			{
				for (j in 1:3)
					{
						cat(paste(communities[i],layers[j],": ",collapse=''))
							
						#rows are layers, cols are communities
						soilt.mat[j,i]=readLines(n=1)
					}			
			}

		} else if (choice =='2')

			{
				options=c('Shoreline','Forest Edge')
				for (i in 1:2)
				{
					for(j in 1:3)
					{
						cat (paste(options[i],layers[j],": ",collapse=''))
						
						othert.mat[j,i]=readLines(n=1)
					}
				}
			}

			#fill in variable names regardless#fix this
			#add soil notes
			
			PF_GS = soilt.mat[1,1]; FIB_GS = soilt.mat[2,1]; HUM_GS = soilt.mat[3,1];
			PF_SH = soilt.mat[1,2]; FIB_SH = soilt.mat[2,2]; HUM_SH = soilt.mat[3,2];
			PF_FOR = soilt.mat[1,3]; FIB_FOR = soilt.mat[2,3]; HUM_FOR = soilt.mat[3,3];

			PF_DEPTH_L = othert.mat[1,1]; PF_DEPTH_FE = othert.mat[1,2];
			FIB_DEPTH_L = othert.mat[2,1]; FIB_DEPTH_FE = othert.mat[2,2];
			HUM_DEPTH_L = othert.mat[3,1]; HUM_DEPTH_FE = othert.mat[3,2];
			
			cat("Soil notes: ")
			soil.notes<-readLines(n=1)
			
			cat("other notes: ")
			Notes1<-readLines(n=1)
			
			new<-data.frame(TransectID,Observer1,Observer2='NA',TransectDate=Date,TransectTime=Time,PF_GS,FIB_GS,HUM_GS,PF_SH,FIB_SH,HUM_SH,PF_FOR,FIB_FOR,HUM_FOR,PF_DEPTH_L,FIB_DEPTH_L,HUM_DEPTH_L,PF_DEPTH_FE,FIB_DEPTH_FE,HUM_DEPTH_FE,FM,EM,MU,PestsPathogens,GrowthAnomalies,Phenology,AnimalUse,Disturbance,Drawings,PF_Peat_Notes = soil.notes,Notes1,gthan='',lthan='',stringsAsFactors=FALSE)
		
			

	#The next section creates a new data frame for an sql query, but only for 2010 data at the moment
				
		
		cat("Need to correct mistakes? (Y/N)")
		mistake<-readLines(n=1)
	
		if(mistake=='Y'|mistake=='y')
		{new<-edit(new)}
		
		
		old<-rbind(old,new)
		write.csv(old,file=file.name,row.names=FALSE)
		
			cat("open excel file? (Y/N)")
			check.xl<-readLines(n=1)
			if(check.xl=='Y'|check.xl=='y')
			shell.exec(file.name)
			
			if (Year==2010)
			{
				#for diagnostic purposes looking at the last line
				
				#new<-read.csv(file='c:/users/vppatil/Dropbox/alaskafiles/2011 datasheets protocols/2011VegTransectMetadata.csv')	
				#new=new[max(length(new$TransectID)),]	
				

				#for 2010 entry<-new[,c(1:5,15:29)
				entry.2010<-new[,c(1:5,15:16,18:19,21:31)]
				entry.2010<-entry.2010[,c(1:5,6,8,7,9,19,12,11,10,13:18,20)]
				names(entry.2010)<-names(meta)
				
				#these are the columns whose values should not have quotes around them inthe sql query
				nums<-c(6,7,8,9,11,12,13,19)
				
				for (i in 1:length(names(entry.2010)))
				{
					if (!i%in%nums)
					{
						entry.2010[,i]=query.quote(as.character(entry.2010[,i]))
					} else {entry.2010[,i]=as.character(entry.2010[,i])}
				}
				
			} else
			
			{
		
				nums=6:23
				for (i in 1:length(names(new)))
				{
					if (!i%in%nums)
					{
						new[,i]=query.quote(as.character(new[,i]))
					} else {new[,i]=as.character(new[,i])}
				}
				
				#note that this won't work with non-numeric characters for soil depths
			}
			
				if (Year==2010) {entry<-entry.2010} else {entry<-new}
				query.meta1<-paste("INSERT INTO tblVegTransectMetadata(",trim(paste(names(entry),collapse=',')),")",collapse='')
				m.vals.forquery<-paste(entry[,1:length(names(entry))],collapse=',')
				query<-paste(query.meta1," VALUES(",m.vals.forquery,")",sep='')
				
				q<-sqlQuery(test,query=query)
				q
				cat(q)
		
		
		
			
			cat("New Row? (Y/N)\n")
			choice<-readLines(n=1)
	}
	
	

	
		# [1] "TransectID"      "Observer1"       "Observer2"       "VegTransectDate" "VegTransectTime" "PF_DEPTH_L"     
		# [7] "PF_DEPTH_F"      "Peat_DEPTH_L"    "Peat_DEPTH_F"    "PF_Peat_Notes"   "MU_w"            "EM_w"           
		#[13] "FM_w"            "PestsPathogens"  "GrowthAnomalies" "Phenology"       "AnimalUse"       "Disturbance"    
		#[19] "Drawing"         "Notes1"         
	
	odbcClose(test)
}



	filename.check<-function(name)
	{
		while(length(grep('.csv',name))<1)
		{
			print('filenames must in in .csv')
			cat("Enter the filename	\n")
			name<-trim(readLines(n=1))
		}
		return(name)
	}

