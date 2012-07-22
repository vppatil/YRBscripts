	

soil.entry<-function()
{
	#file is 2011YRBiodiversity in alaskafiles/2011 datasheets protocols
	setwd('C:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols')
	
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	query.quote<-function(str) paste("'",trim(str),"'",sep='')

	#Requires sql through odbc
	library(RODBC)

	#first step is to read in the database- it should be closed
	yrb2011<-odbcConnectAccess2007('2011YRBiodiversity.accdb')

	#read in a copy of the table
	tablename<-'SoilMoisture'
	soil.m<-sqlFetch(yrb2011,tablename)
	transects<-sqlFetch(yrb2011,'tblTransects')

	time<-Sys.time()



	#time check function
	check.time<-function()
	{
		cat("\nRecord new time now?")
		choice=readLines(n=1)
		if (choice=='Y' | choice == 'y')
		{
			time<-Sys.time()
		} else time=time
		return(time)
	}
	
	
	cat("New Row? (Y/N)\n")
	choice<-readLines(n=1)

	#get new data if it is a new row
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
		#and other vals
		Stratum=inputvals[[1]]
		FocalNum=inputvals[[3]]
		
		
		t.check<-TransectID%in% transects$TransectID
		l.check<-LakeID%in% transects$LakeID
		
		#transect entry validation
		while	 (!l.check | !t.check)
		{
				if(!l.check)
					cat("Lake Not found")
				else cat("TransectID not found")
		
			cat("Enter the data below, as prompted\n")
			for(i in 1:numvars)
			{
				cat(paste(vars[i],": "))
				inputvals[[i]]<-readLines(n=1)
			}

			TransectID<-paste(inputvals[[4]],inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			LakeID<-paste(inputvals[[1]],inputvals[[2]],inputvals[[3]],sep='_')
			Stratum=inputvals[[1]]
			FocalNum=inputvals[[3]]
		
		t.check<-TransectID%in% transects$TransectID
		l.check<-LakeID%in% transects$LakeID
		
		}
	
			
	
		#now you need to figure out which situation you are in

		cat("\nWhat stage? ")
		cat("\n1: Frozen Samples")
		cat("\n2: Air-Dried Samples")
		cat("\n3: First Oven-Dry Weighing")
		cat("\n4: 2nd Oven-Dry Weighing")
 
		stage<-readLines(n=1)
			if(stage=='1')
			{
				#now Community Type
				comm=c("Grass/Sedge","Shrub","Forest")
				cat("\nCommunity Type: \n")
				for(i in 1:length(comm))
				{
					cat(paste(i,": ",comm[i],"\n",sep=''))
				}

				choice=as.numeric(readLines(n=1))
				CommunityType=comm[choice]
				
				#now Soil Type
				layer=c("Humic","Fibric","Mineral","BrownMoss","Ash","Other (see Notes)")
				cat("\nLayer Type: \n")
				for(i in 1:length(layer))
				{
					cat(paste(i,": ",layer[i],"\n",sep=''))
				}

				choice=as.numeric(readLines(n=1))
				Layer=layer[choice]
				
				cat("Now Enter the Weights:\n")
				cat("Weigh boat tare: ")
				WTareFroze<-readLines(n=1)
		
		
				
				cat("\nTotal Frozen Mass (g)")
				FrozenTotalWeight<-readLines(n=1)
				
				cat("\nNotes for frozen samples: ")
				NotesFroze=readLines(n=1)

				AirDryStart<-format(check.time(),format="%m/%d/%y %H:%M:%S")
				#will have to repeat this part for each.
				froze.df<-data.frame(TransectID,Stratum,LakeID,FocalNum,CommunityType,Layer,FrozenTotalWeight,WTareFroze,AirDryStart,NotesFroze,stringsAsFactors=FALSE)
				
				froze.char.nums<-c(1,2,3,4,5,6,9,10)
				froze.df.names<-names(froze.df)
				froze.df[,froze.char.nums]<-query.quote(froze.df[,froze.char.nums])
			
				froze.names<-paste(froze.df.names,collapse=',')
				froze.vals<-paste(froze.df,collapse=',')
				
				q<-paste("insert into SoilMoisture (",froze.names,") VALUES (",froze.vals,")",sep='')
				iq<-sqlQuery(yrb2011,q)
				print(iq)
			}
			 
	
		
			if(stage!='1')
			{
				#first we bring up the entries that meet the same criteria
				check.df<-data.frame(TransectID,stringsAsFactors=FALSE)
				names<-paste(names(check.df),collapse=',')
				check.df<-query.quote(check.df)
				vals<-paste(check.df,collapse=',')
				q=paste("Select ID,CommunityType,Layer,",names," from SoilMoisture where ",names," in (",vals,")",sep='')
				a<-sqlQuery(yrb2011,q)
				print(a)
			
				cat("\nEnter the ID to modify")
				line=as.numeric(readLines(n=1))
			
				if(stage=='2')
				#air dried
				{
					cat("\nWeigh Boat Tare")
					WTareAirDried=as.numeric(readLines(n=1))
					
					cat("\nTotalWeight")
					AirDryAll<-as.numeric(readLines(n=1))

					cat("\nSubsample?")
					sub<-readLines(n=1)	
					if(sub=='y'|sub=='Y')
					{
						AirDrySub=as.numeric(readLines(n=1))
					} else if (sub=='n'|sub=='N')
						{AirDrySub=AirDryAll}
					
					cat("\nOvenTemp?")
					OvenTemp<-as.numeric(readLines(n=1))
					
					cat("\nNotes after AirDry?")
					NotesAirDry=readLines(n=1)
					
					OvenDryStart<-check.time()

					airdry.df<-data.frame(ID=line,WTareAirDried,AirDryAll,AirDrySub,OvenTemp,NotesAirDry,OvenDryStart)
					sqlUpdate(yrb2011,tablename='SoilMoisture',dat=airdry.df,index='ID')
					
				}
				
				if(stage=='3')
				{
					cat("\n1st Oven Dry Weight: ")
					OvenDry1<-as.numeric(readLines(n=1))
			
					TimeWeigh1<-check.time()
					
					cat("\nNotes after Oven Dry?")
					NotesOven<-readLines(n=1)
					Oven1.df<-data.frame(ID=line,OvenDry1,TimeWeigh1,NotesOven)
					sqlUpdate(yrb2011,tablename='SoilMoisture',dat=Oven1.df,index='ID')
				}
					
				
				if(stage=='4')
				{
					cat("\n2nd Oven Dry Weight: ")
					OvenDry2<-as.numeric(readLines(n=1))

					TimeWeigh2<-check.time()
			
					Oven2.df<-data.frame(ID=line,OvenDry2,TimeWeigh2)
					sqlUpdate(yrb2011,tablename='SoilMoisture',dat=Oven2.df,index='ID')
				}	
			
			}
			
			
		cat("New Row? (Y/N)\n")
		choice<-readLines(n=1)

	
	
	#end while loop
	}	

odbcClose(yrb2011)
}