#' Piecewise function at x = 18
#'
#' @param x the variable in the equation
#' @param coef a coefficient from the lm summary
#'
#' @return a one line piecewise function where the break is at x = 18
#' @export
#'
#' @examples
#' \dontrun{piecewise(x,coef=tmp$coefficients[,"Estimate"])}
piecewise = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)

}
