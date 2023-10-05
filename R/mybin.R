#' a binomial distribution where user gets to select the number of iterations ran
#'
#' @param iter number of times the function will run
#' @param n the number of in a trial
#' @param p the probablity of getting a success
#'
#' @return a boxplot of the data distribution as well as a table of probablities from the run
#' @export
#'
#' @examples
#' mybin(iter=100, n = 10, p= 0.7)
mybin=function(iter,n, p){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
