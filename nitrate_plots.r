##read in nitrate table (nitrate.csv)
setwd('c:/users/vppatil/dropbox/')
n03<-read.csv(file='c:/users/vppatil/dropbox/nitrate.csv',header=TRUE)
nh4<-read.csv(file='c:/users/vppatil/dropbox/nh4.csv',header=TRUE)

superpose.eb <-
function (x, y, ebl, ebu = ebl, length = 0.08, ...)
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
    length = length, ...)

lake.class<-read.csv('lake_class.csv',header=TRUE)
n03<-merge(n03,lake.class,by='Lake')
names(n03)[13]='n03'
nh4<-merge(nh4,lake.class,by='Lake')
names(nh4)[13]='nh4'
nh4$Vegetation<-factor(nh4$Vegetation)

#now calculate means by vegetation type
n03.means<-tapply(n03$n03,n03$Vegetation,mean)[c(2,3,1)]
nh4.means<-tapply(nh4$nh4,nh4$Vegetation,mean)[c(2,3,1)]

se=function(x) sd(x)/sqrt(length(x))

n03.ses<-tapply(n03$n03,n03$Vegetation,se)[c(2,3,1)]
nh4.ses<-tapply(nh4$nh4,nh4$Vegetation,se)[c(2,3,1)]

means<-cbind(n03.means,nh4.means)
ses<-cbind(n03.ses,nh4.ses)

n.plot<-barplot(means,beside=TRUE,legend=TRUE,args.legend=c(x=2),ylab='ng * cm ^-2 * d^-1')
superpose.eb(n.plot,means,ses)

n03.lm1<-lm(n03~Vegetation,data=n03)
summary(n03.lm1)
nh4.lm1<-lm(nh4~Vegetation,data=nh4)
summary(nh4.lm1)

n03.mean2<-t(tapply(n03$n03,list(n03$Vegetation,n03$expansion),mean)[c(2,3,1),])
colnames(n03.mean2)<-c('Grass/Sedge','Shrub','Forest')
row.names(n03.mean2)=c('High Flood','Low Flood')

n03.se2<-t(tapply(n03$n03,list(n03$Vegetation,n03$expansion),se)[c(2,3,1),])
tiff('nitrate.tif',height=9,width=9,units='in',res=300)
n03.lake<-barplot(n03.mean2,beside=TRUE,cex.axis=1.5,cex.names=1.5,cex.lab=1.5,col=c('blue','red'),ylab='ng N03 * cm ^-2 * d^-1',legend=TRUE,ylim=c(0,60),args.legend=c(cex=2))
superpose.eb(n03.lake,n03.mean2,n03.se2)
dev.off()
n03.lm2<-lm(n03~Vegetation*expansion,data=n03)