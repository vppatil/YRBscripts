dm.conv<-function(deg,min,sec=0)
{
  #convert degrees minutes to dec.deg
  sec.dec<-sec/60
  min=min+sec.dec
  dec<-min/60
  dd<-deg+dec
  return (dd)
}

#lat lon km distance conversion function

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  long1<-deg2rad(long1)
  long2<-deg2rad(long2)
  lat1<-deg2rad(lat1)
  lat2<-deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}


#pika camp is at 138 16' W, 61d 13' N

#north pole is at 0 W,90N

lat1<-90
lon1<-0

lat2<-dm.conv(61,13)
lon2<-dm.conv(138,16)

pika.np<-gcd.slc(lon1,lat1,lon2,lat2)
#now sort out dist 

#coords<-read.csv('C:/users/vijay/documents/my dropbox/alaskafiles/cmcLatLon.csv')
coords<-read.csv('~/documents/cmcLatLon.csv')


euclid<-function(lat1,lon1,lat2,lon2)
{
	x = lat1-lat2
	y = lon1-lon2
	dist = sqrt((x^2)+(y^2))
	return (dist)
}

lats<-coords$Lat[1]
lons<-coords$Lon[1]

dist<-euclid(lats,lat2,lons,lon2)

lat2=61.2667
lon2=138.2667

lat.dist<-coords$Lat>=61&coords$Lat<=62
lon.dist<-coords$Lon>=138&coords$Lon<=139

range=coords[lat.dist&lon.dist,]

coord.dist<-gcd.slc(range$Lon,range$Lat,lon2,lat2)
good.range<-range[coord.dist<25,]
good.range<-cbind(good.range,coord.dist=coord.dist[coord.dist<25])

#points 1 and 2 are closest
#i 253,474
#i 254,474

#now read in 1 year of snow depth data in documents/snowdepth on linux
setwd('~/Documents/snow depth/')
docs<-dir()
yrs<-1998:2010

pc.snowdepth<-list()
for (yr in 1:length(docs))
{
	#need to read in whole file, not factors, 
	#then extract

	test<-read.table(docs[i],fill=TRUE,blank.lines.skip=TRUE)
	year.rows<-1+707*0:12
	months<-test[year.rows,2]
	months<-months[is.na(months)==FALSE]
	nummonths<-length(months)

	snow.array<-list()
	for(i in 1:nummonths)
	{
	    startrow<-2+707*(i-1)
	    endrow<-707*i
	    snow.array[[i]]<-list(months[i],test[startrow:endrow,1:706])
	}

	snow.array[[i]][[2]]<-snow.array[[i]][[2]][253:254,473:474]
	#for(i in 1:nummonths)
	#pikacamp.mat[,,i]=as.matrix(snow.array[[i]][[2]][253:254,473:474],2,2)

	pc.snowdepth[[yr]]<-list(yrs[yr],snow.array)
}
	
#first dim = yar
#second dim = month
#3rd index 1=month,2=array

mean.snow.month<-list()
for(yr in 1:length(docs))
{
	mean.snow.month[[yr]]<-data.frame(year = pc.snowdepth[[yr]][[1]],snow.depth=mean(pc.snowdepth[[yr]][[2]]))}    	






























