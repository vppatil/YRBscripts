spl<-function(str,start,end)
{
 spl.list<-strsplit(str,split='_')[[1]]
 mer<-paste(spl.list[start:end],collapse='_')
return(mer)
 }