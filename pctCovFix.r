
library(RODBC)
setwd('c:/users/vppatil/desktop/yrb')
yrb<-odbcConnectAccess2007('yrbiodiversity.accdb')

#first part is to correct mammal veg using same system
#three non-vasc categories have to be converted, summed, and reconverted.
#equisetum should be added to forb, reconverted
#check for equisetum in notes
#add a note saying that non-vascular data and equisetum were converted

#raw table
mam.pc<-sqlFetch(yrb,'tblmammalpctcover')

nonvasc<-sqlQuery(yrb,"select mammalPctCoverNumber,trapid,[sphagnum moss],[Non-sphagnum moss],Lichen,Nonvasc,Equisetum,forb from tblmammalpctcover where lichen is not null or nonvasc is null",stringsAsFactor=FALSE)
	
q2011<-"select * from tblmammalpctcover where vegdate like '%2011%'"
test<-sqlQuery(yrb,q2011)

rem2010<-"delete from tblmammalpctcover2011 where vegdate like '%2010'"
	
library(vegan)
library(labdsv)


x = c('N/A','t','1','2','3','4','5')
y = c(0,.5,mean(c(1,10)),mean(c(10,25)),mean(c(25,50)),mean(c(50,75)),mean(c(75,100)))

cbind(x,y)
?vegtrans
#convert to conver classes
#add together here.

new=nonvasc[,3:5]
	
new<-vegtrans(new[,1:3],x,y)
new<-apply(new,1,sum)
	
newef<-nonvasc[,7:8]
newef<-vegtrans(newef,x,y)
newef[,1][is.na(newef[,1])]=0
newef<-apply(newef,1,sum)

newef<-ifelse(newef>100,100,newef)
new<-ifelse(new>100,100,new)
	
low=c(-1,0,1,10,25,50,75)
high=c(0,1,10,25,50,75,100)


valrange=cbind(low,high)
sum.class<-rep('',length(new))
sum.class2<-rep('',length(newef))

for(i in 1:7)
{
	sum.class<-ifelse(new>low[i] & new <=high[i],x[i],sum.class)
	sum.class2<-ifelse(newef>low[i] & newef <=high[i],x[i],sum.class2)

}

nonvasc$NonVasc = sum.class
for (i in 1:length(sum.class))
{
	ins<-nonvasc[i,]
	iq<-paste("update tblmammalpctcover set nonvasc = '",sum.class[i],"',notes = notes + ';nonvasc converted.' where trapid = '",ins$trapid,"'",sep='')
	sqlQuery(yrb,iq)
}	

nonvasc$ef<-sum.class2
for (i in 2:length(sum.class2))
{
	ins<-nonvasc[i,]
	iq<-paste("update tblmammalpctcover set forb = '",sum.class2[i],"',notes = notes + ';forb converted.' where trapid = '",ins$trapid,"'",sep='')
	sqlQuery(yrb,iq)
}

rem2010<-"delete from tblmammalpctcover2011 where vegdate like '%2010%'"


drop.q<-"alter table tblmammalpctcover drop column [sphagnum moss],[Non-sphagnum moss],Lichen,Equisetum"

sqlQuery(yrb,drop.q)

################################################################################################################################
#this should end the 2010-2011 mammal veg conversion issue.


#note that this does not resolve the issue of multiple habitat types with different start or end points- this will have to be a separate query.
#so fix bad dups from query below
#same tid, same hab, different starts where count of distinct rows by hab,tid is > 1

#then replace the current vals in 2011 covers with the ones from the original cover database
# then mop up.
#then you will need to sort out what to do with the non-vasculars
#then compare matches again for each functional type

#then do your own arbitrary error checking on a lake from 2010- see if those got entered correctly.


	select * from tblvegtransectpctcover where transectid in (select transectid from (select transectid,habitattype,count(start) as numEntries from (select distinct transectid, habitattype, start,end from tblvegtransectpctcover) group by transectid,habitattype) where numEntries > 1) and habitattype in (select habitattype from (select transectid,habitattype,count(start) as numEntries from (select distinct transectid, habitattype, start,end from tblvegtransectpctcover) group by transectid,habitattype) where numEntries > 1) order by transectid, habitattype,start;


egs<-sqlQuery(yrb,"select pkey,tid,hab,start,oldeg_shrub,neweg_shrub from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.eg_shrub as oldeg_shrub,cov2011.eg_shrub as neweg_shrub from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldeg_shrub not like neweg_shrub")

gram<-sqlQuery(yrb,"select pkey,tid,hab,start,oldgram,newgram from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.gram as oldgram,cov2011.gram as newgram from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldgram not like newgram")

forb<-sqlQuery(yrb,"select pkey,tid,hab,start,oldforb,newforb from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.forb as oldforb,cov2011.forb as newforb from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldforb not like newforb")

decs<-sqlQuery(yrb,"select pkey,tid,hab,start,olddec_shrub,newdec_shrub from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.dec_shrub as olddec_shrub,cov2011.dec_shrub as newdec_shrub from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where olddec_shrub not like newdec_shrub")

bare<-sqlQuery(yrb,"select pkey,tid,hab,start,oldbare,newbare from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.bare as oldbare,cov2011.bare as newbare from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldbare not like newbare")

dw<-sqlQuery(yrb,"select pkey,tid,hab,start,olddw,newdw from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.dw as olddw,cov2011.dw as newdw from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where olddw not like newdw")

tree<-sqlQuery(yrb,"select pkey,tid,hab,start,oldtree,newtree from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.tree as oldtree,cov2011.tree as newtree from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldtree not like newtree")

eq<-sqlQuery(yrb,"select pkey,tid,hab,start,oldeq,neweq from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.eq as oldeq,cov2011.eq as neweq from (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)) where oldeq not like neweq")

fixes<-list(egs,decs,gram,forb,bare,dw,eq,tree)
names<-c('eg_shrub','dec_shrub','gram','forb','bare','dw','eq','tree')

	for(i in 1:dim(egs)[1])
	{
		df<-egs[i,]
		pk = df$pkey
		newshrub = df$neweg_shrub
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set eg_shrub = '",newshrub,"' where percovautonumber = ",pk,sep=''))
	}
	for(i in 1:dim(decs)[1])
	{
		df<-decs[i,]
		pk = df$pkey
		new = df$newdec_shrub
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set dec_shrub = '",new,"' where percovautonumber = ",pk,sep=''))
	}
	
	for(i in 1:dim(gram)[1])
	{
		df<-gram[i,]
		pk = df$pkey
		new = df$newgram
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set gram = '",new,"' where percovautonumber = ",pk,sep=''))
	}
	
	for(i in 1:dim(forb)[1])
	{
		df<-forb[i,]
		pk = df$pkey
		new = df$newforb
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set forb = '",new,"' where percovautonumber = ",pk,sep=''))
	}
	
	for(i in 1:dim(tree)[1])
	{
		df<-tree[i,]
		pk = df$pkey
		new = df$newtree
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set tree = '",new,"' where percovautonumber = ",pk,sep=''))
	}
	
		for(i in 1:dim(bare)[1])
	{
		df<-bare[i,]
		pk = df$pkey
		new = df$newbare
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set bare = '",new,"' where percovautonumber = ",pk,sep=''))
	}
for(i in 1:dim(dw)[1])
	{
		df<-dw[i,]
		pk = df$pkey
		new = df$newdw
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set dw = '",new,"' where percovautonumber = ",pk,sep=''))
	}

library(vegan)
library(labdsv)

#what are the different kinds of diversity again?

x = c('N/A','t','1','2','3','4','5')
y = c(0,.5,mean(c(1,10)),mean(c(10,25)),mean(c(25,50)),mean(c(50,75)),mean(c(75,100)))

cbind(x,y)
?vegtrans

nonvasc.df<-sqlQuery(yrb,"select tblvegtransectpctcover.percovautonumber,cov2011.transectid,cov2011.start,cov2011.habitattype,cov2011.sphag,cov2011.non_sphag,cov2011.lichen from  (tblvegtransectpctcover inner join cov2011 on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start and tblvegtransectpctcover.end = cov2011.end)",stringsAsFactors=FALSE)

nonvasc.df[is.na(nonvasc.df)] = 'N/A'
nonvasc.df[nonvasc.df == '-+'] = 'N/A'

nums<-vegtrans(nonvasc.df[,5:7],x,y)
nonvasc.sum<-apply(nums,1,sum)
nonvasc.sum<-ifelse(nonvasc.sum > 100,100,nonvasc.sum)
hist(nonvasc.sum)

low=c(-1,0,1,10,25,50,75)
high=c(0,1,10,25,50,75,100)

valrange=cbind(low,high)
sum.class<-rep('',length(nonvasc.sum))
for(i in 1:7)
{
	sum.class<-ifelse(nonvasc.sum>low[i] & nonvasc.sum <=high[i],x[i],sum.class)
}
nonvasc.df$NonVasc = sum.class

pc<-sqlFetch(yrb,'tblvegtransectpctcover')
 test<-pc[match(nonvasc.df$percovautonumber,pc$PerCovAutonumber),] 
test$newnv=nonvasc.df$NonVasc
 old=as.character(test$NonVasc)
 new=nonvasc.df$NonVasc
fix = nonvasc.df[old!=new,]

for(i in 1:dim(fix)[1])
	{
		df<-fix[i,]
		pk = df$percovautonumber
		new = df$NonVasc
		
		sqlQuery(yrb,paste("update tblvegtransectpctcover set NonVasc = '",new,"' where percovautonumber = ",pk,sep=''))
	}
