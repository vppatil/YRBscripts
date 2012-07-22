setwd('c:/users/sam/desktop')
agb<-read.csv('AGB_YRB2011.csv')

agb.t<-table(agb$X.,agb$LakeID,agb$Community.type)
agb.t<-ifelse(agb.t>0,1,agb.t)
agb.t
apply(agb.t,c(2,3),sum)
sum(agb.t)

library(RODBC)
setwd('c:/users/vppatil/desktop/yrb')
yrb<-odbcConnectAccess2007('yrbiodiversity.accdb')
