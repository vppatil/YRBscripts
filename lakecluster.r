#lake cluster analysis
library(RODBC)
yrb=odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/yrbiodiversity.accdb')
lakes<-sqlFetch(yrb,'tblLakeBaseInfo')

#get data file
lakechem<-sqlQuery(yrb,"select pHlake,condlake,cl,na,alk,k,mg,ca,d180,ch4,dic,doc from Lakechemcharacteristics") 
lakelist<-sqlQuery(yrb,"select LakeID from Lakechemcharacteristics")

#make distance matrix
for (i in 1:dim(lakechem)[2])
{
	temp  = lakechem[,i]
	temp[is.na(temp)]=mean(na.omit(temp))
	lakechem[,i]=temp
}

library(vegan)

chemdist<-vegdist(lakechem,method='euclid')
a<-hclust(chemdist)

chem.pc<-prcomp(~.,data=lakechem)
chem.pc<-prcomp(~pHlake+condlake+cl+ca,data=lakechem)

#a few lakes with much higher conductivity than others- 
#1_34_1,1_19_1,1_19_2

