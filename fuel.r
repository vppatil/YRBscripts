#fuel tank area calculator
#L = tank length
#r = tank radius
#h = dip height
#A=sector area


fuel<-function(h){
L = 5*12
r = 38.5/2
d = r-h
theta = (2*acos(d/r)) #theta = angle of circle sector
A = ((r^2)/2)*(theta-sin(theta))
gallons=A*L/231 #231 cubic inches per gallon
return(gallons)
}
