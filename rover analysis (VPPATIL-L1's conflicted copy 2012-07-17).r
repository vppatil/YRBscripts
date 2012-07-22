#lakes

#by subject
#year, day of summer regression
#extract residuals
#look to see if log transformation necessary

setwd('c:/users/sam/desktop/yrb_gis')
library(foreign)
library(shapefiles)

#read data in

#list of okay lakes
setwd('c:/users/sam/desktop/yrb_gis')
mark.okay<-read.dbf('mark_okay.dbf')$dbf #7
roach.okay<-read.dbf('roach_okay.dbf')$dbf
yrb.okay<-read.dbf('yrb_okay.dbf')$dbf

roach.okay[,3]=c(6677,6561,6605)
okay.lakes<-c(as.character(mark.okay[,7]),as.character(roach.okay[,3]),as.character(yrb.okay[,4]))

marklakes<-read.csv('c:/users/sam/desktop/studylakes_wgs84.csv')
marklakes.table<-data.frame(ID = marklakes$ID,X = marklakes$POINT_X, Y = marklakes$POINT_Y)
mark.shp<-convert.to.shapefile(marklakes,marklakes.table,'ID',1)
write.shapefile(mark.shp,'c:/users/sam/desktop/yrb_gis/marklakes')

rover<-read.csv('Rover_all_eqns.csv',header=TRUE)
rover.grts<-read.dbf('rover_near_grts.dbf')	 	
rov.join<-read.dbf('yrb_sample_roverjoin.dbf')$dbf
roach.rov<-read.dbf('roach_rover.dbf')$dbf
rover.2<-read.dbf('Rover_2strata_noMSS.dbf')$dbf

mark.join<-read.dbf('mark_rover.dbf')$dbf

#remove bad lake names
names(mark.join)[7] = names(roach.rov)[11] = names(rov.join[4]) = 'LakeID'
mark.join<-subset(mark.join,mark.join$LakeID %in% okay.lakes)
rov.join<-subset(rov.join,rov.join$LakeID %in% okay.lakes)
roach.rov<-subset(roach.rov,roach.rov$LakeID %in% okay.lakes)

mark.join$LakeID<-factor(mark.join$LakeID)
rov.join$LakeID<-factor(rov.join$LakeID)
roach.rov$LakeID<-factor(roach.rov$LakeID)

#header
#stack.func<-function(rov.join,year.data,idcol) #df for stacking,data cols, idcol
#lakeglm.wrap<-function(joinstack.list,form)
#lakeglm returns 1 join, 2 stack, 3 glms, 4 glms summary

#stacks
rov.all<-stack.func(rover,6:25,1)
mark.all<-stack.func(mark.join,12:31,7)
roach.all<-stack.func(roach.rov,16:35,11)
yrb.all<-stack.func(rov.join,19:38,4)

#glms
f=logarea~year+month

mark.glms<-lakeglm.wrap(mark.all,f)
roach.glms<-lakeglm.wrap(roach.all,f)
yrb.glms<-lakeglm.wrap(yrb.all,f)

rov.stack2<-rov.stack
rov.stack2$values<-ifelse(rov.stack2$values ==0,NA,rov.stack2$values)
rov.glms<-lakeglm.wrap(rov.all,f)

#find cutoffs based on rover all
all.subsets<-list(rov.glms,mark.glms,roach.glms,yrb.glms)
cuts<-lapply(all.subsets,cutoffs)
#horizontal<- 1: all, 2: mark, 3: roach, 4: yrb8
#2: pdiff, 3:mse, 4: slope, 5: norm.slope
#pdiff
a<-sapply(cuts,function(x) return(x[[2]]))
#mse
b<-sapply(cuts,function(x) return(x[[3]]))
#slope
c<-sapply(cuts,function(x) return(x[[4]]))
#norm.slope
d<-sapply(cuts,function(x) return(x[[5]]))

colnames(a)<-colnames(b)<-colnames(c)<-colnames(d)<-c('rov','mark','roach','yrb')
cuts<-list(pdiff=a,mse=b,slope=c,norm.slope=d)

#cutoffs-modified in line above lake. table function

#cutoffs were (50% quantile of negative pdiffs,75% quantile of mse for year+month, 50% quantile of slope, 50% quantile of norm.slope)


#laketable<-function(joinstack,cutoffs)
#make laketables
mark.table<-laketable(mark.glms,cs)
roach.table<-laketable(roach.glms,cs)
yrb.table<-laketable(yrb.glms,cs)
rov.table<-laketable(rov.glms,cs)

	#create coord tables for shapefiles
	
	mark.xy<-data.frame(LakeID = mark.table$LakeID,X = mark.glms[[1]]$X,Y=mark.glms[[1]]$Y)
	roach.xy<-data.frame(LakeID = roach.table$LakeID,X = roach.glms[[1]]$utmx[match(roach.table$LakeID,roach.glms[[1]]$LakeID)],Y = roach.glms[[1]]$utmy[match(roach.table$LakeID,roach.glms[[1]]$LakeID)])
	
	yrb.glms[[1]]$X = rover.2$utmx[match(yrb.glms[[1]]$OBJECTID,rover.2$OBJECTID)]
	yrb.glms[[1]]$Y = rover.2$utmy[match(yrb.glms[[1]]$OBJECTID,rover.2$OBJECTID)]

	yrb.xy<-data.frame(LakeID = yrb.table$LakeID,X = yrb.glms[[1]]$X[match(yrb.table$LakeID,yrb.glms[[1]]$LakeID)],Y = yrb.glms[[1]]$Y[match(yrb.table$LakeID,yrb.glms[[1]]$LakeID)])

	
	#combine
	bigtable<-rbind(mark.table,roach.table,yrb.table)
	big.xy<-rbind(mark.xy,roach.xy,yrb.xy)

	#write shapefiles
	bigselect.shp<-convert.to.shapefile(big.xy,bigtable,'LakeID',1)
	write.shapefile(bigselect.shp,'c:/users/sam/desktop/yrb_gis/bigselect')



#pooled variance estimate
#try first with subset
pooled1<-glm(logarea~-1+ LakeID * (year+month),data=rov.stack.small,na.action=na.omit)
rov.all<-read.csv('rover_vijest.csv')

######################################################3summary functions
fix.order<-function(df) {return(df[order(df$LakeID),])}
cs<-c(-.3,.09,-.206,-.053)
laketable<-function(joinstack,cutoffs)
{
	pd=-.3#cs[1]
	p=.05
	slope=cs[3]
	norm.slope=cs[4]
	mse=cs[2]

	join=joinstack[[1]]
	shrinks=join$LakeID[join$pdiff<=pd]
	sigshrinks<-join$LakeID[join$ps <=p & join$slopes < 0]
	sigexpand<-join$LakeID[join$ps <=p & join$slopes > 0]
	slopeshrinks<-join$LakeID[join$slopes<=slope]
	ns.shrinks<-join$LakeID[join$slope.norm<=norm.slope]
	bigfluct<-join$LakeID[join$mse.cv >=mse]
	 
	lakes<-unique(join$LakeID)
	lake.col<-function(group) return(ifelse(lakes%in%group,1,0))

	lake.df<-data.frame(LakeID=lakes,shrinks=lake.col(shrinks),sigshrinks=lake.col(sigshrinks),slopeshrinks=lake.col(slopeshrinks),ns.shrinks=lake.col(ns.shrinks),bigfluct=lake.col(bigfluct))
	
	#replace ns.shrinks with slopeshrinks if you decide to use unnormalized sigshrinks
	lake.df$lake.category<-'other'
	lake.df$lake.category<-ifelse(lake.df$sigshrinks==1 & lake.df$bigfluct == 0,'shrink',lake.df$lake.category)
	lake.df$lake.category<-ifelse(lake.df$sigshrinks==1 & lake.df$bigfluct == 0 & lake.df$shrinks == 1,'bigshrink',lake.df$lake.category)	
	lake.df$lake.category<-ifelse(lake.df$sigshrinks==0 & lake.df$bigfluct == 0,'stable',lake.df$lake.category)
	lake.df$lake.category<-ifelse(lake.df$sigshrinks==1 & lake.df$bigfluct == 1,'shrinkfluct',lake.df$lake.category)
	lake.df$lake.category<-ifelse(lake.df$sigshrinks==0 & lake.df$bigfluct == 1,'fluct',lake.df$lake.category)

	return(lake.df)
}

	
 
stack.func<-function(rov.join,year.data,idcol) #df for stacking,data cols, idcol
{
	fix.order<-function(df) {return(df[order(df$LakeID),])}
	
	areas<-c('.1979_5','.1981_8','.1983_5','.1985_9','.1986_5','.1986_6','.1986_7','.1994_5','.1994_9','.1999_7','.1999_8','.2000_6','.2000_7','.2002_7','.2003_5','.2005_6','.2007_8','.2008_5','.2009_5','.2009_7')

	names(rov.join)[year.data]=areas
	names(rov.join)[idcol]='LakeID'
	rov.join<-fix.order(rov.join)
	test<-rov.join[,c(idcol,year.data)]
	rov.stack<-stack(rov.join[,year.data])
	rov.stack<-cbind(LakeID = rep(rov.join[,idcol],length(areas)),rov.stack)

	rov.stack$year<-as.numeric(substr(rov.stack$ind,2,5))
	rov.stack$month<-as.numeric(substr(rov.stack$ind,7,7))
	rov.stack$values[rov.stack$values<0]=NA

	
	rov.join$year.range<-tapply(rov.stack$year,rov.stack$LakeID,range.func)
	rov.join$pdiff<-perdif(rov.stack)
	rov.join$meanarea<-tapply(rov.stack$values,rov.stack$LakeID,function(x) mean(na.omit(x)))
	rov.stack$logarea<-log(rov.stack$values+1)
	rov.join$mean.logarea<-tapply(rov.stack$logarea,rov.stack$LakeID,function(x) mean(na.omit(x)))
	
	return(list(rov.join,rov.stack))
	
}


lakeglm.wrap<-function(joinstack.list,form)
{
	stacked=joinstack.list[[2]]
	join=joinstack.list[[1]]
	
	res<-by(stacked,stacked$LakeID,lake.glm,form)
	res.summ<-lapply(res,summary)
	join$slopes<-sapply(res,function(x) if(length(na.omit(x))>0) {return(x$coefficients[2])} else {return(NA)})
	join$slope.norm<-join$slopes/join$mean.logarea
	join$ps<-sapply(res,function(x) {if(length(na.omit(x))==0) return(NA) else(return((summary(x)$coefficients)[2,4]))})
	join$mse<-sapply(res,function(x) if(length(na.omit(x))>0) return(mean(resid(x)^2)) else return(NA))
	join$rmse<-sqrt(join$mse)
	join$mse.cv<-join$rmse/join$mean.logarea
	cat("list contains 1: data, 2: stacked data, 3: glms by lake, 4: glm summaries)")
	return(list(join,stacked,res,res.summ))
	
}

cutoffs<-function(joinstack)
{
	#pdiff,slopes now based on negs only
	join=joinstack[[1]]
	pdiff<-na.omit(join$pdiff)
	mse.cv<-join$mse.cv[!is.na(join$mse.cv)]
	slopes=na.omit(join$slopes)
	s.norm=join$slope.norm[!is.na(join$slope.norm)]
	
	pd=pdiff[pdiff<0 ]
	slopes<-slopes[slopes < 0]
	s.norm<-s.norm[s.norm < 0]
	
	pd.q<-quantile(pd,na.rm=TRUE,c(0,.25,.5,.75,.9,1))
	mse.q<-quantile(mse.cv,na.rm=TRUE,c(0,.25,.5,.75,.9,1))
	slope.q<-quantile(slopes,na.rm=TRUE,c(0,.25,.5,.75,.9,1))
	s.norm.q<-quantile(s.norm,na.rm=TRUE,c(0,.25,.5,.75,.9,1))
	return(list(quantiles<-c('pdiff','mse.cv','slope','norm.slope'),pd.q,mse.q,slope.q,s.norm.q))
}

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
	
lake.glm<-function(x,formula){
	check=length(na.omit(x$logarea))
	if(check<=1) {return(NA)} else {
	return(glm(formula,data=x,na.action=na.omit))}
}
