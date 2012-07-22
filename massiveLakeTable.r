#massive lake comparison table (eventually)


library(RODBC)
yrb<-odbcConnectAccess2007('C:/users/sam/desktop/yrb/YRBiodiversity.accdb')

pc.q<-"select lake.lakeid,pc.* from tbltransects as lake inner join (select transectid,habitattype,habitattypelength,Start,End from tblvegtransectpctcover where start is not null and end is not null) as pc on lake.transectid = pc.transectid"
habAll<-sqlQuery(yrb,pc.q,stringsAsFactor=FALSE)

#get num habs by transect
trs.summ<-sqlQuery(yrb,"select lakeID,transectid,count(HabitatType) as HabNum from (select lake.lakeid,pc.* from tbltransects as lake inner join (select transectid,habitattype,habitattypelength,Start,End from tblvegtransectpctcover where start is not null and end is not null) as pc on lake.transectid = pc.transectid) group by transectid,lakeID")

#get shannon index by transect
Shannon.func<-function(hab.df)
{
	length.vector<-hab.df$habitattypelength
	pi.vec<-length.vector/sum(length.vector)
	shannon<-sum(pi.vec*log(pi.vec))*-1
	return(shannon)
}

shannon<-sapply(by(habAll,habAll$transectid,Shannon.func),cbind)
shannon<-data.frame(shannon,transectid=names(shannon))

habs<-unique(habAll$habitattype)

hablength<-sqlQuery(yrb,"select transectid,habitattype,sum(habitattypelengths) as totallengths from habAlltemp group by transectid,habitattype")
#loop to get lengths for all hab types
hab.length.df<-data.frame()


#get hab lengths for all Habs for each transect

xtabs_2_dataframe <- function(aa){
	# figure out column names that are originally used in the xtabs call
	nm_tmp <- attributes(dimnames(aa))$names
	if(length(nm_tmp) != 2){
	# this function only handles 2 dimension xtabs object
	cat('Error: the input xtabs object has to have at most 2 dimensions!\n')
	#return(NULL)
	} else {
	bb <- as.data.frame.matrix(aa)

	# playing aroun d
	colnames(bb) <- paste(nm_tmp[2], colnames(aa), sep='_')
	bb$newcol <- dimnames(bb)[[1]]
	colnames(bb)[ncol(bb)] <- nm_tmp[1]
	rownames(bb) <- NULL

	cc=data.frame(bb[, ncol(bb)], bb[, 1:(ncol(bb)-1) ], stringsAsFactors = F)
	colnames(cc)[1]=nm_tmp[1]
	return(cc)
} } 

hablengths.tab<-xtabs(habitattypelength~transectid+habitattype,data=habAll)
hablengths.df<-xtabs_2_dataframe(hablengths.tab)
names(hablengths.df)<-c('transectid',as.character(habs))

shorehab<-sqlQuery(yrb,"select transectid,habitattype from (select transectid,habitattype,Start from tblvegtransectpctcover where Start=0)")

merge.func<-function(x,y) {if(sum(!(names(y) %in% names(x))) >0)
x<-merge(x,y,by='transectid')}
trs.summ<-merge(trs.summ,shannon)
trs.summ<-merge(trs.summ,hablengths.df)
trs.summ<-merge(trs.summ,shorehab)

sqlQuery(yrb,'drop table habAlltemp')
sqlSave(yrb,habAll,'habAlltemp')
