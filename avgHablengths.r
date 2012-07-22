
setwd('c:/users/vppatil/desktop/yrb')
library(RODBC)

yrb<-odbcConnectAccess2007('YRBiodiversityCopy.accdb')

sqlQuery(yrb,"update tblvegtransectpctcover set habitattype = 'LS' where habitattype like 'LS%'")
sqlQuery(yrb,"update tblvegtransectpctcover set habitattype = 'GS' where habitattype like 'GS%'")
sqlQuery(yrb,"update tblvegtransectpctcover set habitattype = 'CF' where habitattype like 'CF%'")
sqlQuery(yrb,"update tblvegtransectpctcover set habitattype = NULL where habitattype like 'not%'")
sqlQuery(yrb,"update tblvegtransectpctcover set habitattype = NULL where habitattype = ''")


	q= "select lakeid,TransectID, HabitatType,sum(HabitatTypeLength) as habSum from (select tblvegtransectpctcover.TransectId,tblvegtransectpctcover.habitattype,tblvegtransectpctcover.habitattypelength,tbltransects.lakeid from tblvegtransectpctcover left join tbltransects on tblvegtransectpctcover.transectid  = tbltransects.transectid ) where habitattypelength is not null and habitattypelength < 101 and habitattype not in ('MUFL','EM','AQ','FM') group by lakeid,TransectID,HabitatType"

	
	
	avg.hablengths<-sqlQuery(yrb,q)
	avg.hablengths$habSum<-ifelse(avg.hablengths$habSum < 0 ,0 ,avg.hablengths$habSum)
	hab.t<-table(avg.hablengths$TransectID,avg.hablengths$HabitatType)
	trs<-row.names(hab.t)
	habs<-colnames(hab.t)
	for(i in 1:length(trs))
	{
		for(j in 1:length(habs))
		{
			if(hab.t[i,j] == 0)
			{
				avg.hablengths<-rbind(avg.hablengths,data.frame(lakeid =paste(strsplit(as.character(trs[i]),split='_')[[1]][2:4],collapse='_'),TransectID = trs[i],HabitatType = habs[j],habSum = 0))
			}
		}	
	}
			"lakeid"      "TransectID"  "HabitatType" "habSum"  
	
		hab.t2<-table(avg.hablengths$TransectID,avg.hablengths$HabitatType)
		hab.t3<-table(avg.hablengths$lakeid,avg.hablengths$HabitatType)

		hab.means<-tapply(avg.hablengths$habSum,list(avg.hablengths$lakeid,avg.hablengths$HabitatType),mean)
		
		setwd('c:/users/vppatil/dropbox/alaskafiles/')
			write.csv(hab.means,'avgHabLengths.csv')
	
	
	
	