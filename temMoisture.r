#mark's soil moisture data


superpose.eb <-
function (x, y, ebl, ebu = ebl, length = 0.08, ...)
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
    length = length, ...)


#on desktop
setwd('c:/users/vppatil/dropbox/alaskafiles/2011datasheets protocols')

#file is TEMyrbSoilMoisture.csv
moist<-read.csv('TEMyrbSoilMoisture.csv',header=TRUE,stringsAsFactors=FALSE)

#ivan moisture data
ivan.m<-read.csv('c:/users/vppatil/desktop/SoilMoisture.csv',header=TRUE,sep=',')

ivan.tem=subset(ivan.m,ivan.m$LakeID %in% c('0_9_1','0_9_2','1_58_1','1_27_1','1_99_1','1_99_2','0_2_1','0_2_2'))
ivan.58=ivan.m[grep('1_58_1',ivan.m$LakeID),]
ivan.58$LakeID = '1_58_1'
ivan.tem=rbind(ivan.tem,ivan.58)
ivan.tem$LakeID<-factor(ivan.tem$LakeID)

#lake classes
setwd('c:/users/vppatil/dropbox')
lake.class<-read.csv('lake_class.csv',header=TRUE)
lake.class$Lake<-as.factor(paste('L',lake.class$Lake,sep=''))		

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




