#' A normal curve where P(x<=a)
#'
#' @param a Probablitiy that x = a, where P(X<=a)
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#'
#' @return a plot showing the normal curve, with the probability of x <= a in red
#' @export
#'
#' @examples
#'
myncurve = function(a,mu,sigma){
  curve(dnorm(x,mean = mu,sd = sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,5,length=1000)
  ycurve=dnorm(curve,mean=mu,sd=sigma)

  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")


  prob=pnorm(a,mean=0,sd=1)
  prob=round(prob,4)

  text(mu,0, paste("Area = ", prob, sep=""))  # issues with the locator
}
