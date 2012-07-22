#hab  species clusters

#first get the data
library(RODBC)
yrb=odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')
lakes<-sqlFetch(yrb,'tblLakeBaseInfo')

#load reshape package
library(reshape)

#richness grouped by habitat type, transect, and lake
#get list of habitat


#get names
names(sqlFetch(yrb,'tblVegTransectSppList'))
"TransectID"            "Species"               "GS"                   
 [4] "LS"                    "TS"                    "DT"                   
 [7] "DF"                    "MF"                    "CF"                   
[10] "AQ"                    "FM"                    "MU"                   
[13] "EM"                    "Other"                 "Notes"                
[16] "TransectSppAutonumber"


rich.query="select distinct LakeID,TransectID,Species,GS,LS,TS,DT,DF,MF,CF,AQ,FM,MU,EM,Other from (select tblTransects.LakeID,tblVegTransectSppList.* from tblTransects inner join tblVegTransectSppList on tblTransects.TransectID = tblVegTransectSppList.TransectID)"

rich.df<-sqlQuery(yrb,rich.query)
rich.melt<-melt(rich.df,id.vars=c('LakeID','TransectID','Species'),variable_name = 'HabitatType')
rich.melt=subset(rich.melt,value == 1,select = c('LakeID','TransectID','Species','HabitatType'))

#now get the richness table
rich.byhab<-count(rich.melt,vars=c('LakeID','TransectID','HabitatType'))

#but for now you really just need the rich.melt for the ordination
#make table
library(vegan)
rich.melt$hab.tid<-paste(rich.melt$HabitatType,rich.melt$TransectID,sep='_')
tab.forord<-table(rich.melt$HabitatType,rich.melt$Species)

tab.forord<-tab.forord[apply(tab.forord,1,sum) > 0,]
tab.forord<-tab.forord[,apply(tab.forord,2,sum) > 0]


rich.dec<-decorana(tab.forord,iweigh = TRUE)
plot(rich.dec,display='sites')
text(rich.dec,display='sites')

#use envfit after the fact. or cca first
habs.df<-data.frame(habitat.type = row.names(tab.forord))#that doesn't work
habs.df$simple=c('GS','s','s','f','f','f','f','a','trans','trans','trans','ot')

big.table<-table(rich.melt$hab.tid,rich.melt$TransectID)
habs= substr(row.names(big.table),0,2)
big.habdf<-data.frame(habitat.type = habs)

noaq<-tab.forord[-8,]
habs.noaq<-habs.df[-8,]
rich.smallcca<-cca(noaq~habs.noaq$simple)
rich.cca<-cca(big.table~habs)
hab.simple<-ifelse(habs %in%  c('DF','MF','CF','DT'),'F',ifelse(habs %in% c('LS','TS'),'S',ifelse(habs%in% c('FM','MU','EM'),'Litt',habs)))  
big2<-big.table[-(hab.simple == 'AQ'),]
hab.simple2<-substr(row.names(big2),0,2)
rich.cca2<-cca(big2~hab.simple2)
#for cca need a dataframe with habitat types- using the full sparse datamatrix with species for each tid/h

rich.dist<-dist(tab.forord)
rich.clust<-hclust(rich.dist)

rich.bigdist<-dist(big.table)
rich.bigclust<-hclust(rich.bigdist)

#trying the percent cover ordination again:##########################################################################

pct<-sqlQuery(yrb,"select TransectID,habitattype,start,end,nonvasc,gram,forb,eg_shrub,dec_shrub,tree,dw,bare,habitattypelength from tblVegtransectpctcover")

covers<-sqlQuery(yrb,"select nonvasc,gram,forb,eg_shrub,dec_shrub,tree,dw,bare from tblVegtransectpctcover",stringsAsFactors=FALSE)

cov.names<-names(covers)
covers[covers =='0' | covers == '-1' | covers == 'N/A'|covers == ''] = '-'
covers[covers == '6'] = '5'
covers[covers == 'sphag 2. nonsphag 1. lichen 1. eq na.' ] = '3' #need to fix this in the dase


x=c('-','t','1','2','3','4','5')
y=c(0,.5,(1+10)/2,35/2,75/2,125/2,175/2)

library(labdsv)
nona.sum<-function(x) sum(na.omit(x))
compare.vals<-vegtrans(covers,x,y)
compare.vals<-compare.vals[apply(compare.vals,1,sum)>0,]
compare.vals<-compare.vals[!is.na(compare.vals[,1]),]

habs<-as.character(pct$habitattype[as.numeric(row.names(compare.vals))])
hab.simple<-ifelse(habs %in%  c('DF','MF','CF','DT'),'F',ifelse(habs %in% c('LS','TS'),'S',ifelse(habs%in% c('FM','MU','EM'),'Litt',habs))) 
hab.simple[grep('/',hab.simple)] = 'Other' 
hab.simple[grep(' ',hab.simple)]='Other'
hab.simple
	
	
pct.dec<-decorana(compare.vals)
pct.cca<-cca(compare.vals~hab.simple)
hab.simple<-data.frame(hab=factor(hab.simple))

envfit(pct.dec~hab.simple$hab)