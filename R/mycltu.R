
#' Estimating the CLT using a normal distribution
#'
#' @param n number of values in each iteration
#' @param iter the number of times the distribution will be created
#' @param a the lower number for the uniform distribution
#' @param b the upper number for the uniform distribution
#'
#' @importFrom stats runif density dnorm dunif
#' @importFrom graphics hist lines curve
#'
#' @return a plot showing the sample mean vs the relative density of each mean
#' @export
#'
#' @examples mycltu(20,10,4,8)
mycltu=function(n,iter,a=0,b=10){

  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  x<-NULL
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
