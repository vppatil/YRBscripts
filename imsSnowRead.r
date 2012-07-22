filename<-dir()[8]
test<-read.table(filename,skip=30,colClasses='character',strings)[,1]

snow<-matrix(0,1024,1024)
for(i in 1:1024)
{
	snow[i,]<-as.numeric(strsplit(test[i],split='')[[1]])
}

snow<-snow[1024:1,]


snow.sub<-snow[512:640,336:436]
snow.i<-image(tmpSnow,col=c('darkblue','sandybrown','white','lightblue'))
abline(h=.5,v=.5,col='red')
#so centered on the pole.

#now arrange extractions for 1 year for the part you are interested in.

years=c(1997,1998,2000:2011)
for (k in 1:length(years))
{
	tempdir<-paste('~/Documents/noaa snow/',years[k],sep='')
	setwd(tempdir)
	
	names<-dir()
	numnames<-length(names)

	snowlist.temp<-list()
	#outer loop to go through each year
	for(i in 1:numnames)
	{
		filename<-dir()[i]
		filenum<-paste(1999,i,sep='_')
		temp<-read.table(gzfile(filename),skip=30,colClasses='character')[,1]

	
		tmpSnow<-matrix(0,1024,1024)
		for(j in 1:1024)
		{
			tmpSnow[j,]<-as.numeric(strsplit(temp[j],split='')[[1]])
		}

		tmpSnow<-tmpSnow[1024:1,]


		#now dump into list
		snowlist.temp[[i]]<-list(filenum,tmpSnow.sub)
	}
	imagename<-paste('NOAAsnow',years[k],sep='')
	save.image(imagename)
}

#now for the animation
snow.animate<-function(range,snowlist)
{
 for(i in range)
	 { 
		image(snowlist[[i]][[2]],col=c  ('darkblue','sandybrown','white','lightblue'),axes=FALSE,main=snowlist[[i]][[1]])
	 choice=readline(prompt='Enter to continue\n')
	 if (choice =='n'|choice=='N') break()
	 }

}
