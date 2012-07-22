#veg spp ordination by lakes for tem

library(RODBC)
yrb<-odbcConnectAccess2007('c:/users/vppatil/dropbox/alaskafiles/2011 datasheets protocols/2011YRBiodiversity.accdb')
old<-odbcConnectAccess2007('c:/users/vppatil/desktop/yrb/YRBiodiversity.accdb')

names(sqlFetch(yrb,'tblVegTransectSppList'))
#first get veg spp list
spp<-sqlQuery(yrb,'select * from tblVegTransectSppList where LakeID in ('1_99_1','1_99_2','0_2_1','0_9_1','1_58_1','1_27_1')')