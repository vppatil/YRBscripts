helper.spp<-function(choice)
{
	library(RODBC)
		if (choice==1)
		{
			con<-odbcConnectAccess2007('c:/users/vijay/documents/my dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
		}

		else if (choice ==2)
		{
			con<-odbcConnectAccess2007('c:/users/vijay/desktop/yrb/YRBiodiversity.accdb')
		}

		else
		{
			cat("Error: enter 1 or two")
			break()
		}
	
	cat("New Check? ")
	choice<-readLines(n=1)
	while(choice=='y'|choice=='Y')
	{
	
		#now make a query for the vegtransectspplist table

		s<-species.check()
		#h<-HabitatTypeSelect()
		lake<-TransectIDcheck()
		q.df<-data.frame(s,lake,stringsAsFactors=FALSE)

		sql.check(q.df,con)
	
		cat("New Check? ")
		choice<-readLines(n=1)
	}	
}

		sql.check<-function(sql.df,con)
		{
			sql.names<-names(sql.df)
			char.vals<-c(1:3)
			sql.df[,char.vals]<-query.quote(sql.df[,char.vals])	
			vals<-paste(sql.df)
			where<-paste(sql.names,vals,sep=' = ')
			where=paste(where,collapse=' AND ')
			query<-paste("select LakeID,Bearing,Species,GS,LS,TS,DF,MF,CF,EM,FM,MU,AQ from tblVegTransectSppList where ",where,collapse='')

			q<-sqlQuery(con,query)
			q
			print(q)
		}
		
		trim <- function (x) gsub("^\\s+|\\s+$", "", x)
		query.quote<-function(char) {paste("'",trim(char),"'",sep='')}

		species.check<-function(allspp)
		{
			cat("Enter species name: \n")
			spp<-readLines(n=1)
			spp<-.simpleCap(spp)
			
			name.df<-data.frame(Species=spp,stringsAsFactors=FALSE)
			return(name.df)
		}

#		HabitatTypeSelect<-function()
#		{
#			cat("Enter the number or code for the habitat type you want to search\n")
#
#			ht.n<-c('GS','LS','TS','DT','DF','MF','CF','AQ','FM','MU','EM','Other')
#			ht.c<-c('g','l','t','dt','df','mf','c','a','f','mu','em','o')
#
#			code.df<-data.frame(ht.n,ht.c)
#			print(code.df)
#			
#			codes<-scan(what='char')[1]
#			h.bool<-vector()
#			h.bool<-(ht.c %in%codes | as.character(1:length(ht.c)) %in% codes)
#			h=ht.n[h.bool]
#			
#			ht.df<-data.frame('TRUE',stringsAsFactors=FALSE)
#			ht.df[,1]=as.character(ht.df[,1])
#			names(ht.df)<-h
#			return(ht.df)
#		}
				
		TransectIDcheck<-function(transects.df,entry.df)
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
				Transect.df<-data.frame(LakeID,Bearing,stringsAsFactors=FALSE)
				return(Transect.df)
		}