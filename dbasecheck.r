library(RODBC)
check<-function(choice)
{
	if (choice==1)
	{
		file<-'C:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb'
	}
	
	if (choice==2)
	{
		file<-'C:/users/vijay/desktop/yrb/YRBiodiversity.accdb'
	}
	
	shell.exec(file)
}

prev.entry.check<-function()
{
	cat("new check?")
	choice<-readLines(n=1)
	
	while(choice=='y'|choice=='Y')
	{
		file<-'C:/users/vijay/desktop/yrb/YRBiodiversity.accdb'
		check<-odbcConnectAccess2007(file)
		
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
			
			
	
			TransectID<-query.quote(TransectID)
	
		query<-paste("select TransectID,HabitatType,Start,End,HabitatTypeLength,NonVasc,Gram,Forb,EG_Shrub,Dec_Shrub,Tree,DW,Bare,PerCovAutonumber from tblVegTransectPctCover where TransectID = ",TransectID,sep='')
		
		a<-sqlQuery(check,query)
		print(a)
		
		cat("update habitat types?")
		fix.hab<-readLines(n=1)
		
		#pct cover data mostly not entered
		
		while(fix.hab=='y' | fix.hab=='Y')
		{
			
			#first enter habitat type to fix
			
			cat("row.number?")
			rownum<-as.numeric(readLines(n=1))
			
			cat("Habitat Type: \n")
			habs<-c('GS','LS','TS','DT','DF','MF','CF','Other')
			numvars<-length(habs)
			
			for(i in 1:numvars)
			{
				cat(paste(i,": ",habs[i],"\n"))
			}
			
			Hab.num<-readLines(n=1)
			Hab.num<-as.numeric(Hab.num)
			
			if (Hab.num<numvars)
			{
				HabitatType=habs[Hab.num]
			} else
				HabitatType=readLines(n=1)
				
			#next get pct cover data
			
		cat("Percent Cover by Function type: \n")
		cat("1 2 3 4 5 t -\n")
		
		#swtich categories depending on year
		types<-c('Non-Vasc','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','dead wood','bare ground')
		
		#get cover data
		cover<-list()
		for (i in 1:length(types))
		{
			cat(paste(types[i],": "))
			cover[i]=readLines(n=1)
		}
		
		#now make the df for the update statement
			new=data.frame(TransectID,HabitatType,NonVasc=cover[[1]],Gram=cover[[2]],Forb=cover[[3]],EG_Shrub=cover[[4]],Dec_Shrub=cover[[5]],Tree=cover[[6]],DW=cover[[7]],Bare=cover[[8]],PerCovAutonumber=rownum,stringsAsFactors=FALSE)
			
			cat("add row?")
			add<-readLines(n=1)
			while(add=='y'|add=='Y')
			{
							
						cat("row.number?")
						rownum<-as.numeric(readLines(n=1))
						
						#first enter habitat type to fix
						cat("Habitat Type: \n")
						habs<-c('GS','LS','TS','DT','DF','MF','CF')
						numvars<-length(habs)
						
						for(i in 1:numvars)
						{
							cat(paste(i,": ",habs[i],"\n"))
						}
						
						Hab.num<-readLines(n=1)
						Hab.num<-as.numeric(Hab.num)
						
						HabitatType=habs[Hab.num]
						
						#next get pct cover data
						
					cat("Percent Cover by Function type: \n")
					cat("1 2 3 4 5 t -\n")
					
					#swtich categories depending on year
					types<-c('Non-Vasc','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','dead wood','bare ground')
					
					#get cover data
					cover<-list()
					for (i in 1:length(types))
					{
						cat(paste(types[i],": "))
						cover[i]=readLines(n=1)
					}
					add.row<-data.frame(TransectID,HabitatType,NonVasc=cover[[1]],Gram=cover[[2]],Forb=cover[[3]],
							EG_Shrub=cover[[4]],Dec_Shrub=cover[[5]],Tree=cover[[6]],DW=cover[[7]],Bare=cover[[8]],PerCovAutonumber=rownum,stringsAsFactors=FALSE)
					new<-rbind(new,add.row)
					
					cat("add row?")
					add<-readLines(n=1)
			
			}
			
			new$TransectID<-gsub("'","",new$TransectID)
				
			sqlUpdate(check,tablename='tblVegTransectPctCover',dat=new,index='PerCovAutonumber')
				
			cat("update habitat types?")
			fix.hab<-readLines(n=1)
		}
		
		
		cat("new check?")
		choice<-readLines(n=1)
	}
	
}

query.quote<-function(char) {paste("'",trim(char),"'",sep='')}

erase.careful<-function(filename,startrow,endrow)
{
	yrb<-odbcConnectAccess2007(filename)
	for (i in startrow:endrow)
	{
		sqlQuery(yrb,query=paste("delete from tblVegTransectPctCover where PerCovAutonumber=",i,sep=''))
	}
	odbcClose(yrb)
}

prev.entry.2011<-function()
{
	cat("new check?")
	choice<-readLines(n=1)
	
	while(choice=='y'|choice=='Y')
	{
		file<-'C:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb'
		check<-odbcConnectAccess2007(file)
		
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
			
			TransectID<-query.quote(TransectID)
	
		query<-paste("select Key,TransectID,HabitatType,Start,End,HabitatTypeLength,Sphag,Non_Sphag,Lichen,Gram,Forb,EG_Shrub,Dec_Shrub,Tree,Eq,DW,Bare from tblVegTransectPctCover where TransectID = ",TransectID,sep='')
		
		a<-sqlQuery(check,query)
		print(a)
		
		cat("update habitat types?")
		fix.hab<-readLines(n=1)
		
		#pct cover data mostly not entered
		
		while(fix.hab=='y' | fix.hab=='Y')
		{
			
			#first enter habitat type to fix
			
			cat("row.number?")
			rownum<-as.numeric(readLines(n=1))
			
			cat("Habitat Type: \n")
			habs<-c('GS','LS','TS','DT','DF','MF','CF','Other')
			numvars<-length(habs)
			
			for(i in 1:numvars)
			{
				cat(paste(i,": ",habs[i],"\n"))
			}
			
			Hab.num<-readLines(n=1)
			Hab.num<-as.numeric(Hab.num)
			
			if (Hab.num<numvars)
			{
				HabitatType=habs[Hab.num]
			} else
				HabitatType=readLines(n=1)
				
			#next get pct cover data
			
		cat("Percent Cover by Function type: \n")
		cat("1 2 3 4 5 t -\n")
		
		#swtich categories depending on year
		types<-c('Sphag','Non_Sphag','Lichen','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','Equisetum','dead wood','bare ground')
		
		#get cover data
		cover<-list()
		for (i in 1:length(types))
		{
			cat(paste(types[i],": "))
			cover[i]=readLines(n=1)
		}
		
		#now make the df for the update statement
			new=data.frame(TransectID,HabitatType,Sphag=cover[[1]],Non_Sphag=cover[[2]],Lichen=cover[[3]],Gram=cover[[4]],Forb=cover[[5]],EG_Shrub=cover[[6]],Dec_Shrub=cover[[7]],Tree=cover[[8]],Eq=cover[[9]],DW=cover[[10]],Bare=cover[[11]],Key=rownum,stringsAsFactors=FALSE)
			
			cat("add row?")
			add<-readLines(n=1)
			while(add=='y'|add=='Y')
			{
							
						cat("row.number?")
						rownum<-as.numeric(readLines(n=1))
						
						#first enter habitat type to fix
						cat("Habitat Type: \n")
						habs<-c('GS','LS','TS','DT','DF','MF','CF')
						numvars<-length(habs)
						
						for(i in 1:numvars)
						{
							cat(paste(i,": ",habs[i],"\n"))
						}
						
						Hab.num<-readLines(n=1)
						Hab.num<-as.numeric(Hab.num)
						
						HabitatType=habs[Hab.num]
						
						#next get pct cover data
						
					cat("Percent Cover by Function type: \n")
					cat("1 2 3 4 5 t -\n")
					
					types<-c('Sphag','Non_Sphag','Lichen','graminoids','forbs','shrubs(evergreen)','shrubs (deciduous)','trees','Equisetum','dead wood','bare ground')
					
					#get cover data
					cover<-list()
					for (i in 1:length(types))
					{
						cat(paste(types[i],": "))
						cover[i]=readLines(n=1)
					}
					add.row<-data.frame(TransectID,HabitatType,Sphag=cover[[1]],Non_Sphag=cover[[2]],Lichen=cover[[3]],Gram=cover[[4]],Forb=cover[[5]],EG_Shrub=cover[[6]],Dec_Shrub=cover[[7]],Tree=cover[[8]],Eq=cover[[9]],DW=cover[[10]],Bare=cover[[11]],Key=rownum,stringsAsFactors=FALSE)
			
					new<-rbind(new,add.row)
					
					cat("add row?")
					add<-readLines(n=1)
			
			}
			
			new$TransectID<-gsub("'","",new$TransectID)
				
			sqlUpdate(check,tablename='tblVegTransectPctCover',dat=new,index='Key')
				
			cat("update habitat types?")
			fix.hab<-readLines(n=1)
		}
		
		
		cat("new check?")
		choice<-readLines(n=1)
	}
	
}
