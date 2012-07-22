


m.q<-"select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapChecks.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID"

test<-sqlQuery(yrb,m.q)

#next is to get species richness for each trap

mrich<-"select GridID,TrapID,HabitatType1,count(VegSpecies) as sppCount from (select distinct VegSpecies,GridID,TrapID,HabitatType1 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapChecks.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID))"

test2<-"select distinct VegSpecies,GridID,TrapID,HabitatType1 from (select tblMammalVegSppList.TrapID,tblMammalVegSppList.VegSpecies,tblMammalTrapTraits.GridID,tblMammalTrapTraits.HabitatType1 from tblMammalVegSppList inner join tblMammalTrapTraits on tblMammalVegSppList.TrapID = tblMammalTrapTraits.TrapID)"

a<-sqlQuery(yrb,test2)

#now you know if each species has shown up in the plot

#just loop it.
allcurves<-vector()
for(l in 1:length(unique(a$GridID)))
{

	plot.table<-table(a$TrapID,a$VegSpecies,a$GridID)[,,l]
	plot.table<-plot.table[,apply(plot.table,2,sum) > 0]
	plot.table<-plot.table[apply(plot.table,1,sum) > 0,]
	accum<-plot.table[1,]
	for (i in 2:dim(plot.table)[1])
	{
		accum.vec = apply(plot.table[1:i,],2,sum)
		accum.vec<-ifelse(accum.vec > 0,1,accum.vec)
		accum<-rbind(accum,accum.vec)
	}

	forcurve<-apply(accum,1,sum)

	if (length(forcurve) == 25)
	allcurves<-cbind(allcurves,forcurve)
}

 ts.plot(allcurves,col=rainbow(54))
 m<-apply(allcurves,1,mean)
 plot(m,type='l')
 se<-function(x) {sd(x)/sqrt(length(x)) }
 lines(m+apply(allcurves,1,se),col='red')
 lines(m-apply(allcurves,1,se),col='red')