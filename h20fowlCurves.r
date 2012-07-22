#install nonlinear model tools
detach(h20fowl)
#install.packages('nlstools')
library(nlstools)


#why is this acting weird?


#get data- change the working directory
setwd('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/')
h20fowl<-read.csv('h20fowl.csv',header=TRUE)
h20fowl<-na.omit(h20fowl)
attach(h20fowl)


#basic plot
plot(Area.ha.,CombinedWaterfowlRichness)
plot(log(Area.ha.),log(CombinedWaterfowlRichness))

test<-glm(log(CombinedWaterfowlRichness)~log(Area.ha.),data=h20fowl)

#functions for creating points using each of the two models
exp.plot<-function(a,b,x)
	{y = a * log(x) +b
		return(y)}
		
negexp.plot<-function(a,b,x)
{
	y = a * (1- exp(-b * x))
	return(y)
}

#sequence of numbers 0:400 for plotting
x = 0:400

#overlay of log and neg exponential models.
#you can play around with the a and be values to see which work the best
lines(x,exp.plot(2.65,3,x))
lines(x,negexp.plot(25.8557420 ,0.7525614,x))

#Generalized additive model
#install.packages('gam')
library(gam)
h20fowl.gam<-gam(CombinedWaterfowlRichness~s(Area.ha.),data=h20fowl)
plot(h20fowl.gam,resid=TRUE,se=TRUE)


#log fit
log.fit<-nls(CombinedWaterfowlRichness~a * log(Area.ha.) + b + dummy,data=h20fowl,start = list(a=2,b=2.6) )
log.fit2<-gnls(CombinedWaterfowlRichness~a * log(Area.ha.) + b + dummy,data=h20fowl,params=CombinedWaterfowlRichness~log(Area.ha.)+dummy,start = list(a=2,b=2.6) )


#have to merge lake areas in first.

h20fowl$dummy=1
log.rand<-nlme(CombinedWaterfowlRichness~a * log(Area.ha.) + b,data=h20fowl,fixed = a+b~1,random = ~a+b|dummy,start = list(a=2,b=2.6),method='ML')

h20fowl$log.fit = fitted(log.fit)
log.a=coef(log.fit)[1] #extract coefficients
log.b=coef(log.fit)[2]
#type name of fit to get summary
log.fit
overview(log.fit)

#negexp fit
negexp.fit<-nls(CombinedWaterfowlRichness~a * (1- exp(-b * Area.ha.)),data=h20fowl,start = list(a=12,b=.05))
negexp.fit
overview(negexp.fit)
negexp.a=coef(negexp.fit)[1]
negexp.b=coef(negexp.fit)[2]	#negexp.b is the asymptote

#pseudoR2 function
pseudoR2<-function(nlsfit)
{	SS=sum((CombinedWaterfowlRichness-mean(CombinedWaterfowlRichness))^2)
	r2=1-(SS-986)/SS
return(r2)}

#overview plot
plot(gs.rich$area_ha,gs.rich$avgRich,main = 'GS plant SAR curve',xlab='Area (ha)',ylab = 'Richness')
lines(x,exp.plot(log.a,log.b,x),col='red',lty=2)	#plot curve fit using function I wrote
lines(x,negexp.plot(negexp.a,negexp.b,x),col='blue',lty=2)	#plot curve fit using function I wrote
text(200,5,paste('log paramaters: a=',round(log.a,2),', b = ',round(log.b,2)))
text(250,4,paste('Negative exponential paramaters: a=',round(negexp.a,2),', b = ',round(negexp.b,2)))
legend(210,18,bty='n',lty=c(2,2),col=c('red','blue'),legend=c('Log model','Negative Exponential Model'))

#overview plot
plot(gs.rich$area_ha,gs.rich$avgRich,main = 'GS plant SAR curve',xlab='Area (ha)',ylab = 'Richness')
lines(x,exp.plot(log.a,log.b,x),col='red',lty=2)	#plot curve fit using function I wrote
lines(x,negexp.plot(negexp.a,negexp.b,x),col='blue',lty=2)	#plot curve fit using function I wrote
text(200,5,paste('log paramaters: a=',round(log.a,2),', b = ',round(log.b,2)))
text(250,4,paste('Negative exponential paramaters: a=',round(negexp.a,2),', b = ',round(negexp.b,2)))
legend(210,18,bty='n',lty=c(2,2),col=c('red','blue'),legend=c('Log model','Negative Exponential Model'))

#compare models
AIC(log.fit,negexp.fit)
#AIC shows substantially more support for negexp.fit
#but AIC is potentially problematic
#residual mean squared error is similar from overview

library(mmSAR)
#remove 0
rem0<-function(df)
{
	return(subset(df,df$CombinedWaterfowlRichness > 0 & df$Area.ha. > 0))
}

create.mmsardf<-function(df,nm)
{
	df<-subset(df,select = c('Area.ha.','CombinedWaterfowlRichness'))
	names(df)<-c('a','s')
	df<-list(name = nm,data=df)
	return(df)
}

h20fowl<-rem0(h20fowl)
h20fowl.mmSAR<-create.mmsardf(h20fowl,'waterfowl')


data(negexpo)
data(expo)
data(power)

mods=c('power','expo','negexpo')
#for ci- use the rth and nth (25th, 975th value from 1000 bootstrap samples)
h20.multiSAR<-multiSAR(c('expo','negexpo'),h20fowl.mmSAR)
h20fowl<-h20fowl[,1:3]
hfowl.bootci<-t(apply(h20.multiSAR$bootMatrix,2,quantile,c(.025,.975)))
h20fowl$model.average<-h20.multiSAR$averaged #model average estimates

h20fowl<-cbind(h20fowl,hfowl.bootci)#add ci onto data set
names(h20fowl)[5:6] = c('lowerci','upperci')
h20fowl<-cbind(h20fowl,t(h20.multiSAR$calculated))
h20fowl<-h20fowl[order(h20fowl$Area.ha.),]

pred<-subset(h20fowl,select = c('Area.ha.','model.average'))
pred<-pred[order(pred$Area.ha.),]

#for shading
poly.x<-c(h20fowl$Area.ha,rev(h20fowl$Area.ha))
poly.y<-c(h20fowl$upperci,rev(h20fowl$lowerci))


plot(h20fowl$Area.ha.,h20fowl$CombinedWaterfowlRichness,ylim=c(0,25),type='n',main = 'waterfowl species area curves',xlab = 'area ha',ylab = 'richness')
polygon(poly.x,poly.y,col='grey',fillOddEven =FALSE)
points(h20fowl$Area.ha.,h20fowl$CombinedWaterfowlRichness)

#draw model averaged fit
lines(pred,lty = 2,lwd = 2)
lines(h20fowl$Area.ha.,h20fowl$expo,col = 'blue')
lines(h20fowl$Area.ha.,h20fowl$negexpo,col = 'red')

text(250,3,'circles are data points, grey shading = 95% CI.\nBlue = log model,Red = neg Exp,black = model-averaged')