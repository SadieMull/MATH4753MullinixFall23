#' The quadratic line of best fit of Breast Height Diameters to tree heights
#'
#' @param x the variable of Breast Height Diameters of Spruce Trees
#'
#' @return the estimated height of the Spruce Tree
#' @export
#'
#' @examples
#' myquadplot(19)
myquadplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
