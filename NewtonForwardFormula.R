# Newton’s Forward Interpolation Formula
NFF<-function(x,y,x0){ #x0 for which, y-value (yx) will be interpolated
  l<-length(x)         
  h<-x[2]-x[1]	  # difference between 2nd and 1st values of x 
  u<-(x0-x[1])/h   
  sum<-0
  for(i in 1:(l-1)){
    del<-diff.default(y, lag=1, differences=i)  # ith differences of y 
    sum<-sum+choose(u, i)*del[1]	
  }
  return(y[1]+sum)
}
# An example
x<-seq(320, 325, 1)
y<-c(2.505150, 2.506505, 2.507856, 2.509203, 2.510545, 2.511883)
options(digits = 10)
NFF(x, y, 320.4)		#interpolation (begaining)
# R output
1] 	2.505692611
NFF(x,y,319.5)		#extrapolation (begaining)
# R output
[1] 	2.50446998
