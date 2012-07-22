#lakes

#by subject
#year, day of summer regression
#extract residuals
#look to see if log transformation necessary

setwd('c:/users/sam/desktop/yrb_gis')
library(foreign)

rover<-read.csv('Rover_all_eqns.csv',header=TRUE)
rover.grts<-read.dbf('rover_near_grts.dbf')	 	
rov.join<-read.dbf('yrb_sample_roverjoin.dbf')$dbf

#year names:
areas<-c('.1979_5','.1981_8','.1983_5','.1985_9','.1986_5','.1986_6','.1986_7','.1994_5','.1994_9','.1999_7','.1999_8','.2000_6','.2000_7','.2002_7','.2003_5','.2005_6','.2007_8','.2008_5','.2009_5','.2009_7')

names(rov.join)[19:38]=areas
test<-rov.join[,c(4,19:38)]
rov.stack<-stack(test,-LakeID)
rov.stack<-cbind(LakeID = rep(rov.join$LakeID,20),rov.stack)
rov.stack.small<-rov.stack
rov.join.small<-rov.join

#for all rover
names(rover)<-gsub('X','.',names(rover))
names(rover)[6:25]<-areas
names(rover)[1]='LakeID'
s<-sample(1:dim(rover)[1],1000,replace=FALSE)
rov.sub<-rover[s,]

rov.join<-rover
#subset for pooled variance
#rov.join<-rov.sub

test<-rov.join[,c(1,6:25)]


#for rover.grts
#names(rover.grts)<-gsub('X','.',names(rover.grts))
#names(rover.grts)[6:25]<-areas
#names(rover.grts)[1]='LakeID'
#test<-rover.grts[,c(1,6:25)]
#rov.join<-rover.grts

lakes<-unique(rov.join$LakeID)

rov.stack<-stack(test,-LakeID)
#change this line
rov.stack<-cbind(LakeID = rep(rov.join$LakeID,20),rov.stack)

#rov.stack can be used- split ind into year and month
rov.stack.small$year<-as.numeric(substr(rov.stack.small$ind,2,5))
rov.stack.small$month<-as.numeric(substr(rov.stack.small$ind,7,7))
rov.stack.small$values[rov.stack.small$values<0]=NA

rov.stack$year<-as.numeric(substr(rov.stack$ind,2,5))
rov.stack$month<-as.numeric(substr(rov.stack$ind,7,7))
rov.stack$values[rov.stack$values<0]=NA


#rov.stack summaries
range.func<-function(x) max(x[x>0])-min(x[x>0])
perdif<-function(df) #calculate percent change in area from earliest to last measurement for all lakes
{
	pd<-vector()
	lakes=unique(df$LakeID)
	for(i in 1:length(lakes))
	{
	
		s<-subset(df,df$LakeID==lakes[i])
		areas=s$values
		years=s$year
		new.area=mean(areas[years==max(years[!is.na(areas)&areas>0])])
		old.area=mean(areas[years==min(years[!is.na(areas)&areas>0])])
		pdiff=(new.area-old.area)/old.area
		pd<-c(pd,pdiff)
	}

	return(pd)
}

	
year.range<-tapply(rov.stack$year,rov.stack$LakeID,range.func)
rov.join$year.range<-year.range[match(names(year.range),rov.join$LakeID)]

#pdiff<-perdif(rov.stack)
pdiff<-perdif(rov.stack.small)

#
lakes<-rov.join.small$LakeID

#run glms
rov.stack$values[rov.stack$values == -1] = NA
rov.stack.small$values[rov.stack.small$values == -1] = NA

rov.stack$logarea=log(rov.stack$values+1)
rov.stack.small$logarea=log(rov.stack.small$values+1)
rov.stack$LakeID<-factor(rov.stack$LakeID)

lake.glm<-function(x,formula) return(glm(formula,data=x,na.action=na.omit))
sub=subset(rov.stack,rov.stack$LakeID == '1_28_1')
rov.glms<-by(rov.stack.small,rov.stack.small$LakeID,lake.glm,logarea~year+month)
rov.summaries<-lapply(rov.glms,summary)



slopes<-sapply(rov.glms,function(x) return(x$coefficients[2]))
ps<-sapply(rov.glms,function(x) {if(dim(summary(x)$coefficients)[1]==1) return(NA) else(return((summary(x)$coefficients)[2,4]))})
mse<-sapply(rov.glms,function(x) return(mean(resid(x)^2)))
rmse<-sqrt(mse)
#mean.logarea<-tapply(rov.stack$logarea,rov.stack$LakeID,function(x) mean(na.omit(x)))
mean.logarea<-tapply(rov.stack.small$logarea,rov.stack.small$LakeID,function(x) mean(na.omit(x)))



#change between rov.join, rov.join.small
rov.join.small$pdiff<-pdiff[match(lakes,rov.join.small$LakeID)]
rov.join.small$p<-ps[match(lakes,rov.join.small$LakeID)]
rov.join.small$slopes<-slopes[match(lakes,rov.join.small$LakeID)]
rov.join.small$mse<-mse[match(lakes,rov.join.small$LakeID)]
rov.join.small$rmse<-rmse[match(lakes,rov.join.small$LakeID)]
rov.join.small$mean.logarea<-mean.logarea[match(lakes,rov.join.small$LakeID)]
rov.join.small$mse.cv<-rov.join.small$rmse/rov.join.small$mean.logarea

shrinks=rov.join.small$LakeID[rov.join.small$pdiff<=-.3]
sigshrinks<-rov.join.small$LakeID[rov.join.small$p <=0.05 & rov.join.small$slopes<0]
shrink.x<-ifelse(rov.join$LakeID %in% shrinks, 1,0)
sigshrinks.x<-ifelse(rov.join$LakeID %in% sigshrinks,1,0)

quantile(rov.join.small$mse.cv)
bigfluct<-rov.join.small$LakeID[rov.join.small$mse.cv > 0.07]
smallfluct<-rov.join.small$LakeID[rov.join.small$mse.cv > 0.03]

bigfluct.x<-ifelse(rov.join$LakeID %in% bigfluct,1,0)
smallfluct.x<-ifelse(rov.join$LakeID %in% smallfluct,1,0)

sloperange<-quantile(slopes)
biggerslopes<-rov.join.small$LakeID[rov.join.small$slopes < -.08]

hist(biggerslopes
biggerslopes[biggerslopes %in% shrinks]

 
lake.table<-data.frame(id = rov.join.small$LakeID,shrink=shrink.x,sigshrink=sigshrinks.x,bigfluct=bigfluct.x,smallfluct=smallfluct.x)

stable<-ifelse(shrink.x+sigshrinks.x+smallfluct.x==0,1,0)
fluctuating<-ifelse(bigfluct.x == 1 & shrink.x+sigshrinks.x == 0,1,0)
fullshrink<-ifelse(shrink.x==1 & sigshrinks.x==1,1,0)
shrinkfluct<-ifelse(shrink.x==1 & bigfluct.x==1,1,0)
shrinkfluct2<-ifelse(shrink.x==1 & smallfluct.x==1,1,0)
shrinkfluct3<-ifelse(sigshrinks.x==1 & smallfluct.x==1,1,0)
lake.table<-cbind(lake.table,stable,fluctuating,fullshrink,shrinkfluct2)

#noshrink,nofluct noshrink,fluct shrink,nofluct shrink,fluct
#1 not in shrinks,sigshrinks or smallfluct
#2 in bigfluct, nothingelse
#3 in sigshrinks and/or shrinks
#4 #in shrinks and bigfluct

stable<-



#write.csv(rov.join,'rover_vijest.csv')
#export roverdata out so you don't have to do it again

#pooled variance estimate
#try first with subset

small.glm<-by(rov.stack.small,rov.stack.small$LakeID,lake.glm,logarea~year+month)


pooled1<-glm(logarea~-1+ LakeID * (year+month),data=rov.stack.small,na.action=na.omit)


torm<-ls()[ls()!='rov.stack']
rm(list=c(torm,'torm'))
pooled2<-glm(logarea~-1+ LakeID * (year+month),data=rov.stack,na.action=na.omit)

setwd('c:/users/sam/desktop/yrb_gis')
rov.all<-read.csv('rover_vijest.csv')

attach(rov.all)
diffs<-rov.all$pdiff
negs<-diffs[diffs<0 & !is.na(diffs)]
quantile(negs,c(.25,.75,.05,.95))
quantile(diffs[!is.na(diffs)],c(.25,.75))

negslopes<-slopes[slopes<0]
quantile(negslopes)

lakes<-unique(rov.stack$LakeID)

rov.resid<-sapply(rov.summaries,resid)
ps<-sapply(rov.summaries,function(x) return (x$coefficients[2,4])
shrinks<-lakes[ps<0.05]
shrink.data<-subset(rov.stack,rov.stack$LakeID %in% shrinks)

shrink.data$LakeID<-factor(shrink.data$LakeID)
par(mfcol=c(3,3))
by(shrink.data,shrink.data$LakeID,function(x)plot(x$year,x$values))

cbind(shrinks,ps)[ps<0.05,]
#still need to extract residuals

sub<-subset(rov.stack,rov.stack$LakeID == '1_27_1')
sub.glm<-lake.glm(sub,values~month+year)


