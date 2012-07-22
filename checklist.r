i=i+1
sqlQuery(yrb,paste("select  distinct TransectID from tblVegTransectMetadata where TransectID like '%",lakes[i],"'",sep=''))

sqlQuery(yrb,paste("select  distinct TransectID from tblTransects where TransectID like '%",lakes[i],"'",sep=''))