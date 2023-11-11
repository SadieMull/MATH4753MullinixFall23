#' A normal curve where P(x<=a)
#'
#' @param a Probablitiy that x = a, where P(X<=a)
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#'
#' @importFrom graphics polygon layout
#' @importFrom stats pnorm pbinom
#'
#' @return a plot showing the normal curve, with the probability of x <= a in red
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(a,mu,sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  x<- NULL
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")


  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)

  text(mu,0, paste("Area = ", prob, sep=""))
}
