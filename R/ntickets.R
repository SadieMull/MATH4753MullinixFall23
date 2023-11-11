
#' Number of tickets to solve an airline overbooking problem
#'
#' @param N Number of seats on the plane.
#' @param gamma Pain threshold deemed suitable by the airline
#' @param p probablility a person shows
#'
#' @return two plots, one showing the discrete number of seats sold and one showing the continuous number of seats sold, as well as list containing useful data from the function
#' @export
#'
#' @examples ntickets(N=400,gamma = 0.02, p = 0.95)
#'
ntickets = function(N = 200, gamma = .02, p = .94){
  rangediscrete <- c(N:(N*1.1))
  binomal <- 1-gamma-pbinom(N,rangediscrete,p)
  nd <- which.min(abs(binomal))
  nd <- rangediscrete[nd]

  rangedCont <- seq(N, N*1.1, by = .000005)
  cont <- -pnorm(N+.5, rangedCont*p, sd = sqrt(rangedCont*p*(1-p)))+1-gamma
  nc <-which.min(abs(cont))
  nc <- rangedCont[nc]

  discreteTitle <- paste("(", toString(nd), ") gamma = ", toString(gamma), " N = ", toString(N), " Discrete")
  contTitle <- paste("(", toString(nc), ") gamma = ", toString(gamma), " N = ", toString(N), " Continuous")



  layout(matrix(1:2, nrow=2,ncol=1))

  plot(rangediscrete, binomal, type = 'b', main =  paste('Objective vs. n to find optimal number of tickets sold
      ', discreteTitle), xlab = "n", ylab = "objective")
  abline(h = 0, col = 'red', lwd= 2)
  abline(v=nd, col = 'red', lwd = 2)

  plot(rangedCont, cont, type= 'l', main = paste('Objective vs. n to find optimal number of tickets sold
    ', contTitle), xlab = "n", ylab = "objective")
  abline(h = 0, col = 'blue', lwd= 2)
  abline(v=nc, col = 'blue', lwd = 2)

  lists <-  list('nd' = nd,'nc' = nc, 'N' = N, 'p' = p, 'gamma' = gamma)
  print(lists)
}
