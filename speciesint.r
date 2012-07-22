## attempt to recreate mutualist/nonmut interaction model of travis et al.

#parameters
it = 50			#number of iterations
x = y = 200; 		#grid boundaries
rmax = .3; rmin = 0 	#max and min reproductive rate, zero may not work.
d = .07			#probability of death	
bval = .3		#benefit of occuring with a mutualist
cval = 0.02		#cost to mutualist of occuring with someone else.
Am = .4			#proportion of species B that is mutualist
Bm = .1			#proportion of species B that is mutualist

Ae = .5			#proportion of cells that start out with no B
Be = .5			#proportion of cells that start out with no B

Amat = matrix(0,x,y)	#lattice for a occupation
Bmat = matrix(0,x,y)	#lattice for b occupation

Ar = matrix(0,x,y)
Br = matrix(0,x,y)

A.df<-data.frame()	#populate these at each timestep
B.df<-data.frame()

#neighbor function
neighbor<-function(choice,x,y)
	{
		if(choice[x,y] == 1)
			{x.new = x;y.new = y+1}	
		else if(choice[x,y] ==2)
			{x.new = x+1;y.new = y}
		else if(choice[x,y] == 3)
			{x.new = x;y.new=y-1}
		else 
			{x.new = x-1;y.new = y}
		x.new<-ifelse(x.new == 0,200,x.new)
		x.new<-ifelse(x.new == 201,1,x.new)
		y.new<-ifelse(y.new == 0,200,y.new)
		y.new<-ifelse(y.new == 201,1,y.new) #hack, need to make generic for different board sizes
		new.coords = c(x.new,y.new)
		return(new.coords)
	}

#create environmental gradient
xvals = 1:200
rvals = rmax*(xvals/200)#rvals are gradient

for (i in 1:x)
{
	for (j in 1:y)
	{		
		rands = runif(2)		#0 is empty, 1 is mut, 2 is non
		Amat[i,j]<-ifelse(rands[1] < Am,1,ifelse(rands[1] < Am + Ae,0,2))
		Bmat[i,j]<-ifelse(rands[2] < Bm,1,ifelse(rands[2] < Bm + Be,0,2))
	}
}


#for each mutualist and non mutualist, calc r.

Ares.mat<-array(0,dim=c(x,y,it))
Bres.mat<-array(0,dim=c(x,y,it))
for (i in 1:it)#10 it
{
	#reproduction values
	for(xval in 1:x)
	{	
		Br.temp = Br[,xval]
		Ar.temp = Ar[,xval]; # for empty cells
		Br.temp[(Bmat[xval,] == 1 | Bmat[xval,] == 2) & Amat[xval,] == 0] = rvals[xval]
		Br.temp[Bmat[xval,] == 1 & Amat[xval,] == 1] = rvals[xval] + bval - cval
		Br.temp[Bmat[xval,] == 1 & Amat[xval,] == 2] = rvals[xval] - cval	
		Br.temp[Bmat[xval,] == 2 & Amat[xval,] == 1] = rvals[xval] + bval	

		Ar.temp[(Amat[xval,] == 1 | Amat[xval,] == 2) & Bmat[xval,] == 0] = rvals[xval]
		Ar.temp[Amat[xval,] == 1 & Bmat[xval,] == 1] = rvals[xval] + bval - cval
		Ar.temp[Amat[xval,] == 1 & Bmat[xval,] == 2] = rvals[xval] - cval	
		Ar.temp[Amat[xval,] == 2 & Bmat[xval,] == 1] = rvals[xval] + bval

		Br[,xval] = Br.temp
		Ar[,xval] = Ar.temp		
	}
	

	cell = 1:4
	choice = matrix(sample(cell,x*y,replace=TRUE),x,y)
	choice.b = matrix(sample(cell,x*y,replace=TRUE),x,y)	

	

	for(xval in 1:x)
	{
		for(yval in 1:y)
		{
			if(Amat[xval,yval] > 0 | Bmat[xval,yval] > 0)
			{			
				new.x = neighbor(choice,xval,yval)[1]
				new.y = neighbor(choice,xval,yval)[2]
				if(Amat[xval,yval] >0 & Amat[new.x,new.y] == 0)#cell is occupied and neighbor is not
					Amat[new.x,new.y]<-ifelse(runif(1) < Ar[xval,yval],Amat[xval,yval],0)
				
				new.x = neighbor(choice.b,i,j)[1]
				new.y = neighbor(choice.b,i,j)[2]
				if(Bmat[xval,yval] >0 & Bmat[new.x,new.y] == 0)#cell is occupied and neighbor is not
					Bmat[new.x,new.y]<-ifelse(runif(1) < Br[xval,yval],Bmat[xval,yval],0)
			}			
		}
	}		
	
	
	#death
	death.func<-function(Mat)
	{
		if(Mat == 1 | Mat == 2)
			Mat<-ifelse(runif(1) < d,0,Mat)
		return(Mat)
	}
	
	Amat<-apply(Amat,c(1,2),death.func)
	Bmat<-apply(Bmat,c(1,2),death.func)


	#output number of cells with each val	
	am = sum(Amat == 1)
	an = sum(Amat == 2)
	ae = sum(Amat == 0)

	bm = sum(Bmat == 1)
	bn = sum(Bmat == 2)
	be = sum(Bmat == 0)
	
	A.df<-rbind(A.df,c(am,an,ae))
	B.df<-rbind(B.df,c(bm,bn,be))
	
	Ares.mat[,,i]=Amat
	Bres.mat[,,i]=Bmat
}





 








