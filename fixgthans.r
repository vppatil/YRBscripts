problem- > symbols in depth measurements

solution

	1) find columns where they occurred
	2) record corresponding column names
	3) paste into additional column called approximate depths (character
		have col names, separated by commas
		requires indexing appropriate col. names
	4) replace > with nothing, convert to numeric
	
	meta<-read.csv('VegTransectMetadata.csv',header=TRUE)
	meta<-meta[,-33]
	
	replace.greaterthan<-function(df)
	{
		df.names<-names(df)
		cols<-length(df.names)
		rows<-length(row.names(df))
		approx<-vector()
		approx2<-vector()

		for (i in 1:rows)
		{
			
			test<-vector()
			for (j in 1:cols)
			{
				test<-c(test,length(grep('>',df[i,j])>0))

				if(is.character(df[i,j]))
				{
					df[i,j]<-gsub('>','',df[i,j])
				}
				
			}
			g.than.cols<-df.names[test==1]
			if(length(g.than.cols)<1) {g.than=''} else
			{ g.than<-paste(g.than.cols,collapse=',') }
			
			approx[i]=g.than
		}
	
		
		for (i in 1:rows)
		{
			
			test<-vector()
			for (j in 1:cols)
			{
				test<-c(test,length(grep('<',df[i,j])>0))

				if(is.character(df[i,j]))
				{
					df[i,j]<-gsub('<','',df[i,j])
				}
				
			}
			l.than.cols<-df.names[test==1]
			if(length(l.than.cols)<1) {l.than=''} else
			{ l.than<-paste(l.than.cols,collapse=',') }
			
			approx2[i]=l.than
		}
		
		approx.df<-data.frame(gthan=approx,lthan=approx2)
		
		return(approx.df)
	}
	
	
	meta<-cbind(meta,replace.greaterthan(meta))
	write.csv(meta,'VegTransectMetadata.csv')
		