se<-function(x) sd(x)/sqrt(length(x))
		
superpose.eb <-
function (x, y, ebl, ebu = ebl, length = 0.08, ...)
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
    length = length, ...)


#lake classes
#setwd('c:/users/sam/dropbox')
setwd('c:/users/sam/dropbox')
		
lake.class<-read.csv('lake_class.csv',header=TRUE)
lake.class$Lake<-as.factor(paste('L',lake.class$Lake,sep=''))		


#soil temperature
	#remember to account for date somehow
	setwd('c:/users/sam/dropbox/alaskafiles/2011 datasheets protocols')

	oldtemp<-read.csv('Old temp.csv',header=TRUE,stringsAsFactor=FALSE)
	newtemp<-read.csv('NewTemp.csv',header=TRUE,stringsAsFactor=FALSE)

	#no overlap in lake names

	ampm<-rep(0,length(oldtemp$Notes))
	ampm[grep('am',oldtemp$Notes)] = 1
	oldtemp$Notes<-gsub('pm','am',oldtemp$Notes)
	oldtimes<-vector()
	for(i in 1:length(oldtemp$Notes))
	{
	oldtimes[i]<-ifelse(i %in%  grep('am',oldtemp$Notes),strsplit(oldtemp$Notes[i],split= 'am')[[1]],NA)
	}

	oldtimes<-as.numeric(ifelse(oldtimes=='the ',NA,oldtimes))
	oldtimes<-ifelse(ampm==0,oldtimes+1200,oldtimes)

	oldtemp<-cbind(Time=oldtimes,oldtemp)

	meantemp<-ifelse(is.na(newtemp$Temp2),newtemp$Temp1,(newtemp$Temp1+newtemp$Temp2+newtemp$Temp3)/3)
	newtemp$meantemp<-meantemp
	newtemp<-subset(newtemp,select=c('Time','Date','LakeID','Distance','meantemp'))
	oldtemp<-subset(oldtemp,select=c('Time','Date','Lake','Distance','Temp'))
	names(oldtemp)[3] = 'LakeID'
	names(newtemp)[5]='Temp'
	temp<-rbind(oldtemp,newtemp)
	temp<-subset(temp,!is.na(temp$Temp))

	#now the two are merged, but what about comparing temps across lakes

	#first get the naive curve for all
	allmean<-tapply(newtemp$Temp,newtemp$Distance,function(x) mean(na.omit(x)))
	allse<-tapply(newtemp$Temp,newtemp$Distance,function(x) sd(na.omit(x))/sqrt(length(na.omit(x))))
	#plot(seq(0,100,by=10),allmean,type='l')
	#superpose.eb(seq(0,100,by=10),allmean,allse)

	#now select out the tem lakes
	tem.temp<-subset(temp,temp$LakeID %in% lake.class$LakeID)
	tem.temp$Temp<-ifelse(tem.temp$Temp > 30,(tem.temp$Temp -32)*(5/9),tem.temp$Temp)
	tem.mat<-reshape(tem.temp,v.names='Temp',timevar = 'Distance',idvar='LakeID',direction='wide')
	ts.plot(t(tem.mat[,4:14]),col=rainbow(8))
	legend(6,14,legend=tem.mat$LakeID,col=rainbow(8),lty=1)
	
	tem.mean<-tapply(tem.temp$Temp,tem.temp$Distance,mean)
	
	#now to merge the temperature data with habitat type, could maybe do it by dominant habitat type as well.
	library(RODBC)
	yrb=odbcConnectAccess2007('c:/users/sam/desktop/yrb/yrbiodiversity.accdb')
	yrb2011=odbcConnectAccess2007('c:/users/sam/dropbox/alaskafiles/2011 datasheets protocols/2011yrbiodiversity.accdb')
	#get Lake,Habitattype,Start,End
	#only select transectID where soil temp was collected? or just take mean
	
	q="select transectid,habitattype,start,end from tblvegtransectpctcover where transectid in (select transectid from tblvegtransectmetadata where pf_depth_l = -1 or pf_depth_l is null) and start is not null and end is not null"
	q2="select transectid,habitattype,start,end from tblvegtransectpctcover where start is not null and end is not null and transectid in (select transectid from tblvegtransectmetadata where pf_depth_l = -1 and pf_depth_fe = -1)"
	
	tem.habs<-sqlQuery(yrb,q,stringsAsFactor=FALSE)
	tem.habs2<-sqlQuery(yrb2011,q2,stringsAsFactor=FALSE)
				
	sqlQuery(yrb2011,"select * from tblvegtransectpctcover where start <=0 and end >= 0 and transectid like '%0_19_1'")
	
	LakeID<-vector()
	for (i in 1:dim(tem.habs)[1])
	{
		LakeID[i]<-paste(strsplit(tem.habs$transectid[i],split='_')[[1]][2:4],collapse='_')
	}
	tem.habs<-cbind(LakeID,tem.habs)
	
	tempHab<-vector()
	
	for(i in 1:dim(temp)[1])
	{
			
			sub.temp<-subset(tem.habs,select=c('LakeID','habitattype'),tem.habs$LakeID == temp$LakeID[i]  & tem.habs$start <= temp$Distance[i] & tem.habs$end >= temp$Distance[i])	
			
			sub.temp
	#	if(dim(sub)	[1] > 1) sub=sub[1,]
	#	if(dim(sub)[1] > 0)
	#	{
			tempHab<-c(tempHab,sub.temp$habitattype[1])
			tempHab
			l=length(tempHab)
			if(l < i) tempHab<-c(tempHab,NA)
	#	} else tempHab<-c(tempHab,NA)
	}		
	
	#leave out lake 0_15_1
	#0_2_1 ends short
	#would have to make a script that averages for lakes where there isn't a soil transect marked
	#like 1_90_4
	#0_28_5 exclude
	#so mostly okay, but remove nas
	
	temp$habitatType<-tempHab
	
	temp<-temp[!is.na(temp$habitatType),]
	
	#now take the average
	temp.mean<-tapply(temp$Temp,list(temp$LakeID,temp$habitatType),mean)
	temp.habmean<-tapply(temp$Temp,temp$habitatType,mean)
	temp.habse<-tapply(temp$Temp,temp$habitatType,se)
	temp.plot<-barplot(temp.habmean)
	superpose.eb(temp.plot,temp.habmean,temp.habse)	
	
	#do average temp by distance within gs
	temp.gs<-subset(temp,temp$habitatType == 'GS')
	gs.mean<-tapply(temp.gs$Temp,temp.gs$Distance,mean)
	gs.se<-tapply(temp.gs$Temp,temp.gs$Distance,se)
	gs.plot<-barplot(gs.mean)
	superpose.eb(gs.plot,gs.mean,gs.se)
	

	#do average distances from tem.habs by habitattype

	se.q<-"select habitattype,avg(mid) as avgMid,count(mid) as l,stdev(mid)/sqr(l) as se from (select transectid,habitattype,(end + start)/2 as mid from tblvegtransectpctcover where start is not null and end is not null and habitattype in ('GS','LS','TS','MF','DF','DT','CF')) group by habitattype order by 2"
	
	hab.midpoints<-sqlQuery(yrb,se.q)
	hab.locs<-barplot(hab.midpoints$avgMid,names.arg=hab.midpoints$habitattype)
	superpose.eb(hab.locs,hab.midpoints$avgMid,hab.midpoints$se)

#so the habs do conform to expectations for the most part.
#could do hab midpoints across lakes rather than amounts, that might show it more clearly.
	#for tem lakes, that is.
	
#shrub harvests

	setwd('c:/users/sam/desktop')
	shrubs<-read.csv('shrub_harvest.csv',header=TRUE)

	setwd('c:/users/sam/dropbox/datasheetsforivan')
	containers<-read.csv('containermass.csv',header=TRUE)
	mean.container<-tapply(containers$mass,containers$container,mean)

	#subtract container masses from raw masses
	mass.switch<-function(x,container){
		switch(container,heavyLunch = x-mean.container[1],lightLunch = x-mean.container[2],smCoin = x-mean.container[3],lgCoin = x-mean.container[4])
		}

	shrubs$new.mass<-0
	for (i in 1:dim(shrubs)[1])
	{
		shrubs$new.mass[i]=mass.switch(shrubs$Mass[i],shrubs$Container[i])
	}

	tapply(shrubs$new.mass,list(shrubs$LakeID,shrubs$Community,shrubs$Part,shrubs$New.Old),mean)
	#so gs had a larger new/old stem ratio than the shrubs.

https://dl-web.dropbox.com/get/AlaskaFiles/rscripts/agb%20summary.r?w=1e510cb3
#shrub stem densities from waldrop et al. still need to be entered
#shrub productivity stuff could be calculated now if you had that paper set up.
		
#enter cwd as well
		
#mark's soil moisture data
	setwd('c:/users/sam/dropbox/alaskafiles/2011 datasheets protocols/waldrop data/')
	soilpH<-read.csv('soilpHeC.csv',header=TRUE,stringsAsFactor=FALSE)

	#fix comm names
	soilpH$Site<-ifelse(soilpH$Site =='FOREST','F',soilpH$Site)
	soilpH$Site<-ifelse(soilpH$Site =='SCREEN G','G',soilpH$Site)
	soilpH$Site<-ifelse(soilpH$Site =='SHRUB','S',soilpH$Site)

	soilpH<-subset(soilpH,soilpH$Site !='UNKNOWN' )

	soilpH$Horizon[grep('MESIC',soilpH$Horizon)] = 'MES'
	soilpH$Horizon[grep('MIN',soilpH$Horizon)] = 'MIN'
	soilpH$Horizon[grep('M',soilpH$Horizon)] = 'MIN'

	soilpH$Horizon[grep('F',soilpH$Horizon)] = 'FIB'
	soilpH$Horizon[grep('A',soilpH$Horizon)] = 'A'
	soilpH$Horizon[grep('Live moss',soilpH$Horizon)] = 'LI'
	soilpH$Horizon[grep('H',soilpH$Horizon)] = 'H'

	nona.mean<-function(x) mean(na.omit(x))
	nona.se<-function(x) 
	{
		nona.x<-na.omit(x)
		s<-sd(nona.x)
		l<-length(nona.x)
		se<-s/sqrt(l)
		return(se)
	}

	soilpH<-merge(soilpH,lake.class,by='Lake')

	a<-glm(pH~(Site+Horizon),data=soilpH)
	gs<-subset(soilpH,soilpH$Site == 'G')

	b<-glm(pH~Horizon*trend,data=soilpH)

	allmean<-tapply(soilpH$pH,list(soilpH$Lake,soilpH$Site,soilpH$Horizon),nona.mean)
	sitemean<-tapply(soilpH$pH,list(soilpH$Site,soilpH$Horizon),nona.mean)

	fib<-allmean[,,2]

	tapply(soilpH$pH,list(soilpH$Lake,soilpH$Site,soilpH$Horizon),nona.se)
	tapply(soilpH$pH,list(soilpH$Site,soilpH$Horizon),nona.se)



	#so soil acidity is higher in forests

#now look at EC
	EC<-as.numeric(soilpH$EC)
	EC<-ifelse(soilpH$X !='mS',EC/1000,EC)
		
	logEC<-log(EC) #non normal

	ECmean<-tapply(logEC,list(soilpH$Lake,soilpH$Site,soilpH$Horizon),nona.mean)
	ECsitemean<-tapply(logEC,list(soilpH$Site,soilpH$Horizon),nona.mean)

	ECfib<-ECmean[,,2]

	c<-glm(EC~soilpH$Site+soilpH$expansion)
	d<-glm(EC~soilpH$Site)

#definite lower EC in forests, apparently higher in GS- would need to examine more to confirm this.


    F   FOREST        G        S SCREEN G    SHRUB  UNKNOWN 

#on desktop
setwd('c:/users/sam/dropbox/alaskafiles/2011datasheets protocols')

#Soil moisture

	#file is TEMyrbSoilMoisture.csv
	moist<-read.csv('TEMyrbSoilMoisture.csv',header=TRUE,stringsAsFactors=FALSE)

	#ivan moisture data
	ivan.m<-read.csv('c:/users/sam/desktop/SoilMoisture.csv',header=TRUE,sep=',')

	ivan.tem=subset(ivan.m,ivan.m$LakeID %in% c('0_9_1','0_9_2','1_58_1','1_27_1','1_99_1','1_99_2','0_2_1','0_2_2'))
	ivan.58=ivan.m[grep('1_58_1',ivan.m$LakeID),]
	ivan.58$LakeID = '1_58_1'
	ivan.tem=rbind(ivan.tem,ivan.58)
	ivan.tem$LakeID<-factor(ivan.tem$LakeID)

	moist<-merge(moist,lake.class,by='Lake')

	str(moist)
	cbind(names(moist))

	#response is h20.gram.soil
	#predictor =  Lake, Site, horizon

	#different names for habs
	moist$Site[moist$Site == 'F']='FOREST'
	moist$Site[moist$Site == 'SCREEN G'] = 'G'
	moist$Site[moist$Site == 'S']='SHRUB'
	moist$Site[moist$Site == 'G']='GRASS/SEDGE'
	moist<-subset(moist,moist$Site !='UNKNOWN')
	moist$Site<-factor(moist$Site)
	moist$Lake<-factor(moist$Lake)
	summary(moist$Lake)

	#now deal with layer types
	cbind(row.names(table(moist$horizon)))
	moist$horizon[moist$horizon == 'M']='MINERAL'
	moist$horizon[grep('^F',moist$horizon)]='F'
	moist$horizon[grep('^A',moist$horizon)]='MINERAL'
	moist$horizon[grep('^H',moist$horizon)]='H'
	moist$horizon[c(grep('LIVE MOSS',moist$horizon),grep('live moss',moist$horizon))]='MOSS'
	moist$horizon[grep('MINERAL',moist$horizon)]='MINERAL'
	moist$horizon[c(moist$horizon == 'L',moist$horizon == 'LITTER',grep('L/D',moist$horizon))]='LITTER'
	moist$horizon[moist$horizon == 'L']='LITTER'
	moist$horizon[c(grep('DEAD MOSS',moist$horizon),moist$horizon == 'D')]='DEAD MOSS'
	moist$horizon[grep('MESIC',moist$horizon)]='MESIC'
	moist$horizon[moist$horizon == 'D']='DEAD MOSS'

	moist<-data.frame(moist,stringsAsFactors=TRUE)
	moist$Rep<-as.numeric(moist$Rep)
	moist$basal.depth..cm<-as.numeric(moist$basal.depth..cm)

	#average depths by lake and layer
	#basal.depth..cm
	nona.mean<-function(x) mean(na.omit(x))
	nona.count<-function(x) length(na.omit(x))
	se<-function(x) sd(x)/sqrt(length(x))
	nona.se<-function(x) se(na.omit(x))


	tapply(moist$basal.depth..cm,list(moist$Site,moist$horizon),nona.mean)

	#crap

	#just look at fibric layer for now
	#average moisture content by lake and layer
	moistforplot=tapply(moist$h20.gram.soil,moist$LakeID,nona.mean)[c(2,3,1)]

	fibric.mean<-tapply(moist$h20.gram.soil,list(moist$expansion,moist$Site,moist$horizon),nona.mean)[,c(2,3,1),2]
	fibric.mean2<-tapply(moist$h20.gram.soil,list(moist$Lake,moist$expansion,moist$Site,moist$horizon),nona.mean)[,,c(2,3,1),2]
	fibric.supermean<-apply(fibric.mean,c(2,3),nona.mean)

	tapply(moist$h20.gram.soil,list(moist$expansion,moist$Site,moist$horizon),nona.count)[,c(2,3,1),c(2)]
	fibric.se<-tapply(moist$h20.gram.soil,list(moist$expansion,moist$Site,moist$horizon),nona.se)[,c(2,3,1),c(2)]
	fibric.superse<-apply(fibric.mean,c(2,3),nona.se)


	f2<-barplot(fibric.supermean,beside=TRUE,legend=TRUE)
	superpose.eb(f2,fibric.supermean,fibric.superse)

	row.names(fibric.mean)=c('High Flood','Low Flood')

	tiff('fibricmoist.tif',height=9,width=9,units='in',res=300)
	colnames(fibric.mean)=c('Grass/Sedge','Shrub','Forest')
	f.plot<-barplot(fibric.mean,beside=TRUE,col=c('Blue','Red'),cex.names=1.5,cex.axis=1.5,cex.lab=1.5,args.legend=c(cex=2),legend=TRUE,ylim=c(0,7),ylab=c('g H20 / g Soil'))
	superpose.eb(f.plot,fibric.mean,fibric.se)
	dev.off()
	fibric.data<-subset(moist,moist$horizon=='F')
	fibric.lm<-lm(h20.gram.soil~expansion*Site,data=fibric.data)
	fibric.lme<-lmer(h20.gram.soil~expansion*Site + (1|Lake),data=fibric.data)


	Analysis of Variance Table

	Response: h20.gram.soil
				   Df Sum Sq Mean Sq F value    Pr(>F)    
	expansion       1 14.924 14.9236 10.3202  0.002327 ** 
	Site            2 38.898 19.4488 13.4496 2.208e-05 ***
	expansion:Site  2 10.132  5.0662  3.5035  0.037834 *  
	Residuals      49 70.856  1.4460                      




