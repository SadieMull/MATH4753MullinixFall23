#' A function that will return a 95\% mean confidence interval of one data set
#'
#' @param x vector used
#'
#' @importFrom stats qt
#' @return the 95\% confidence interval for the mean
#' @export
#'
#' @examples myci(c(1,4,5,2,4))
myci=function(x){

  len <- length(x)
  t=qt(0.975,len-1)
  ci=c(NA,NA)
  ci[1]=mean(x)-t*sd(x)/sqrt(len)
  ci[2]=mean(x)+t*sd(x)/sqrt(len)
  return(ci)
}
