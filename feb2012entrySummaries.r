library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')


#four comparisons
#get table, merge, sort unique transects by table

#Transects
t<-sqlQuery(yrb,"select LakeID,TransectID from tblTransects")
t2<-sqlQuery(old,"select LakeID,TransectID from tblTransects")

t3<-rbind(t,t2)

t3<-t
t3<-t3[!duplicated(t3$TransectID),]
t.table<-table(t$LakeID,t$TransectID)
t.table<-apply(t.table,1,sum)

weird.t<-t.table[t.table > 4|t.table < 4]

#Meta
m<-sqlQuery(yrb,"select TransectID from tblVegTransectMetadata")
mt<-sqlQuery(yrb,"select LakeID,TransectID from tblTransects where TransectID in (select distinct TransectID from tblVegTransectMetadata)")

m.old<-sqlQuery(yrb,"select TransectID from tblVegTransectMetadata")
mt.old<-sqlQuery(yrb,"select LakeID,TransectID from tblTransects where TransectID in (select distinct TransectID from tblVegTransectMetadata)")


transects=sqlQuery(yrb,"select distinct TransectID from tblTransects where LakeID = '1_53_2' and TransectID not in (select distinct TransectID from tblVegTransectMetadata where TransectID like '%1_53_2')")


mt<-rbind(mt,mt.old)
mt<-mt[!duplicated(mt$TransectID),]

m.table<-table(mt$LakeID,mt$TransectID)
m.table<-ifelse(m.table > 0,1,m.table)
m.table<-apply(m.table,1,sum)
m.table
length(m.table)

entered.lm<-unique(row.names(cbind(m.table)))

partial=m.table[m.table <4]
lakes<-as.character(sqlQuery(yrb,'select distinct LakeID from tblTransects')$LakeID)

missing<-lakes[lakes%in% entered.lm == FALSE]

missing.m<-m$TransectID[m$TransectID %in% mt$TransectID == FALSE]
cbind(as.character(missing.m))

#now from the merged copy
m.copy<-sqlQuery(copy,"select TransectID from tblVegTransectMetadata")
mt.copy<-sqlQuery(copy,"select LakeID,TransectID from tblTransects where TransectID in (select distinct TransectID from tblVegTransectMetadata)")

mt=mt.copy
mt<-mt[!duplicated(mt$TransectID),]

m.table<-table(mt$LakeID,mt$TransectID)
m.table<-ifelse(m.table > 0,1,m.table)
m.table<-apply(m.table,1,sum)
m.table
length(m.table)

missing.m<-m.copy$TransectID[m.copy$TransectID %in% mt$TransectID == FALSE]
cbind(as.character(missing.m))

lakes<-sqlQuery(yrb,"select distinct LakeID from tblTransects")
missing.lakes=lakes[lakeID %in% pt$TransectID)
pt.table<-ifelse(pt.table>0,1,pt.table)
pt.table<-apply(pt.table,1,sum)

pt.table
length(pt.table)
missing.pt=lakes[lakes$LakeID %in% pt$LakeID ==FALSE,]

#spp.list
st<-sqlQuery(yrb2011,"select LakeID,Bearing as TransectID from tblVegTransectSppList")
st$TransectID=paste(as.character(st$Bearing),as.character(st$LakeID),sep='_')
st<-subset(st,select=c('LakeID','TransectID'))
s.old<-sqlQuery(yrb,"select TransectID from tblVegTransectSppList")
st.old<-sqlQuery(yrb,"select LakeID,TransectID from tblTransects where TransectID in (select distinct TransectID from tblVegTransectSppList)")

st<-rbind(st,st.old)

st<-st[!duplicated(st$TransectID),]
st.table<-table(st$LakeID,st$TransectID)
st.table<-ifelse(st.table>0,1,st.table)
st.table<-apply(st.table,1,sum)

st.table
length(st.table)
missing.st=lakes[lakes$LakeID %in% st$LakeID ==FALSE,]

length(st.table)