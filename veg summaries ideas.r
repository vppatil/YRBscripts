
setwd('C:/Users/Vijay/Documents')
#create a standard error function to use later.
se<-function(x) sd(x)/sqrt(length(x))

#read in the data with the read.table function
pct.cover<-read.table(file='tblVegTransectPctCover.csv',header=TRUE,sep=',')
spp.list<-read.table(file='tblVegTransectSppList.txt',header=TRUE,sep=',')

#use var HabitatTypeLength
#table by Habitat Type

#means- need to break out by transect w/in lake, by lake
#or sum across all transects within lake

#habitat type names for graphs
hab.names=c('Coniferous Forest','Deciduous Forest','Dwarf Tree','Emergent','Float','Grass Sedge','Low Shrub','Mixed Forest','Mud','Tall Shrub')
spp.list.names<-c('Grass Sedge','Low Shrub','Tall Shrub','Dwarf Tree','Deciduous Forest','Mixed Forest','Coniferous Forest','Aquatic','Floating Mat','Mudflat','Emergent','Other')

#stratum 0 = west, 1 = south/east

#load error bar function
source(file='C:/Users/Vijay/Documents/My Dropbox/thesis stuff/r scripts/superpose.eb.r')


#need to strsplit transectid by '_'
#strsplit stands for 'string split'- splitting a character string into chunks
#first number is transect id
#second is stratum
#third is lake id w/in stratum
#fourth is focal or not

pct.cover<-subset(pct.cover,pct.cover$TransectID!='')
ids<-strsplit(as.character(pct.cover$TransectID),split='_')
ids<-t(sapply(ids,cbind))


ids<-data.frame(ids)
names(ids)<-c('transect','stratum','lake.id','Focal')
#now I make a fifth column, recombining the lake id and focal columns to get a complete lake id, which I call lake.id2
ids$lake.id2<-paste(ids$lake.id,ids$Focal,sep='_')

#now add the identifier columns to the original percent cover dataset
pct.cover<-cbind(ids,pct.cover)

#remove species entries that aren't associated with a lake/transect
spp.list<-subset(spp.list,spp.list$TransectID!='')

#add new id columns to the species richness data
spp.list.ids<-strsplit(as.character(spp.list$TransectID),split='_')
spp.list.ids<-t(sapply(spp.list.ids,cbind))
spp.list.ids<-data.frame(spp.list.ids)
names(spp.list.ids)<-c('transect','stratum','lake.id','Focal')
spp.list.ids$lake.id2<-paste(spp.list.ids$lake.id,ids$Focal,sep='_')
spp.list<-cbind(spp.list.ids,spp.list)

#spp.presence=a partial data frame created with just the checkboxes for community type
spp.presence=array(0,dim=c(length(unique(spp.list$Species)),12,2))

#this part looks confusing, but basically counts the number of unique species found in each community type
for(i in 1:12) {
spp.presence[,i,]=table(spp.list$Species,spp.list[,i+7],spp.list$stratum)[,2,]}
spp.presence<-ifelse(spp.presence>0,1,spp.presence)
spp.richness<-apply(spp.presence,c(2,3),sum)

#spp.richness has total species richness by plant community type and by stratum
row.names(spp.richness)<-spp.list.names
barplot(t(spp.richness),beside=TRUE,legend.text=c('West','South'),ylab='Total Species Richness',xlab='Plant Community Type')
barplot(spp.richness,names.arg=c('West','South'),beside=TRUE,main='Total richness by community type',ylab='Number of Species',ylim=c(0,150),xlab='Stratum',col=rainbow(12),legend=TRUE,args.legend=c(x=10,y=150))

#now, how many lakes are there in each sample?
lake.table<-table(spp.list$lake.id2,spp.list$stratum)
lake.table<-ifelse(lake.table>0,1,lake.table)
richness.n<-apply(lake.table,2,sum)




#mean species richness by lake, stratum




rich.table<-table(spp.list$Species,spp.list$lake.id2,spp.list$stratum)
rich.table<-ifelse(rich.table>0,1,rich.table)
rich.table.sum<-apply(rich.table,c(2,3),mean)

rich.table.forsum<-table(spp.list$Species,spp.list$lake.id2,spp.list$stratum)
rich.table.forsum<-ifelse(rich.table.forsum>0,1,rich.table.forsum)
rich.table.sum<-apply(rich.table.forsum,c(2,3),sum)

no.zero.mean<-function(x) mean(x[x>0])
no.zero.se<-function(x) se(x[x>0])

rich.table.mean<-apply(rich.table.sum,2,no.zero.mean)
rich.table.se<-apply(rich.table.sum,2,no.zero.se)


rich.mean<-barplot(rich.table.mean,names.arg=c('West','South'),ylim=c(0,60),ylab='mean species richness',main='Mean Species Richness by stratum')
superpose.eb(rich.mean,rich.table.mean,rich.table.se)
barplot(rich.table.sum,names.arg=c('West','South'),ylab='species richness',main='Total Species Richness by stratum')
	


#hab.lake is the number of lakes with each community type

hab.lake<-table(pct.cover$HabitatType,pct.cover$lake.id2,pct.cover$stratum)
hab.lake<-ifelse(hab.lake>0,1,hab.lake)
hab.lake<-apply(hab.lake,c(1,3),sum)

num.lakes.t<-table(pct.cover$lake.id2,pct.cover$stratum)
num.lakes.t<-ifelse(num.lakes.t>0,1,num.lakes.t)
num.lakes<-apply(num.lakes.t,2,sum)

#hab.proportions is the proportion of lakes containing each community type
hab.proportions<-cbind(hab.lake[,1]/num.lakes[1],hab.lake[,2]/num.lakes[2])
hab.names2<-row.names(table(pct.cover$HabitatType))
barplot(t(hab.proportions),names.arg=hab.names2,beside=TRUE,ylab='Proportion of Lakes where present',xlab='Habitat Type',legend.text=c('West','South'))

#now get the average size of each community zone, summed across all transects per lake
hab.length<-xtabs(HabitatTypeLength~lake.id2+stratum+HabitatType,data=pct.cover)
hab.length.west<-hab.length[num.lakes.t[,1]==1,1,]
hab.length.south<-hab.length[num.lakes.t[,2]==1,2,]
good.hab.length<-list(hab.length.west,hab.length.south)

temp.func<-function(x) apply(x,2,mean)
temp.func2<-function(x) apply(x,2,se)


hab.length.mean<-sapply(good.hab.length,temp.func)
hab.length.se<-sapply(good.hab.length,temp.func2)
hab.names2<-row.names(hab.length.mean)

length.plot<-barplot(t(hab.length.mean),beside=TRUE,main='mean community zone width, by lake',ylab='Mean habitat zone width',xlab='Habitat Type',legend.text=c('West','South'))
superpose.eb(length.plot,t(hab.length.mean),t(hab.length.se))

hab.length.mean.noz<-apply(hab.length,c(2,3),no.zero.mean)
hab.length.se.noz<-apply(hab.length,c(2,3),no.zero.se)

length.plot2<-barplot(hab.length.mean.noz,names.arg=hab.names2,beside=TRUE,main='mean community zone width, where present',ylab='Mean habitat zone width',xlab='Habitat Type',legend.text=c('West','South'))
superpose.eb(length.plot2,hab.length.mean.noz,hab.length.se.noz)

#shannon diversity index:
#formula = sum from i to S of p i*ln(p i) where p i is the proportional abundance of species i (ni/N) where N is total number of individuals (sum of distances)
#make a column of index values for each lake

#need to find all the non-zero length values, get their sum, then calc the index.

#accept a vector of all ni
Shannon.func<-function(ni.vec){
	ni<-ni.vec[ni.vec>0]
	pi.vec<-ni/sum(ni)
	shannon<-sum(pi.vec*log(pi.vec))*-1
	return(shannon)}

shannon.sapply<-function(hab){apply(hab,1,Shannon.func)}

shannon.stratum<-sapply(good.hab.length,shannon.sapply)
shannon.mean<-sapply(shannon.stratum,mean)
shannon.se<-sapply(shannon.stratum,se)
shannon.plot<-barplot(shannon.mean,main='Mean Shannon Diversity Index, by stratum',ylim=c(0,max(shannon.mean)*1.3),ylab='H',xlab='stratum',names.arg=c('West','South'))
superpose.eb(shannon.plot,shannon.mean,shannon.se)
shannon.t<-t.test(shannon.stratum[[1]],shannon.stratum[[2]])
t<-shannon.t$statistic
p<-shannon.t$p.value
legend(.3,max(shannon.mean)*1.2,c(paste('t = ',round(t,2),sep=''),paste('p = ',round(p,2),sep='')),bty='n')
	
#mean number of community types per lake, by stratum
#using the pct.cover dataframe

num.hab<-function(x) {length(unique(x))}
tapply(pct.cover$HabitatType,list(pct.cover$lake.id2,pct.cover$stratum),num.hab)

hab.table<-table(pct.cover$HabitatType,pct.cover$lake.id2,pct.cover$stratum)
hab.table<-ifelse(hab.table>0,1,hab.table)
hab.table<-apply(hab.table,c(2,3),sum)
hab.num.mean<-apply(hab.table,2,no.zero.mean)
hab.num.se<-apply(hab.table,2,no.zero.se)
hab.num<-barplot(hab.num.mean,names.arg=c('West','South'),ylim=c(0,7),main='Mean number of community types per stratum',ylab='Mean number of community types',xlab='Stratum')
superpose.eb(hab.num,hab.num.mean,hab.num.se)


