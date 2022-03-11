#' Title
#'
#' @param y Quantile
#' @param r Num success
#' @param p Probability of success
#'
#' @return Probabilty of falling within the lower tail of the quantile
#' @export
#'
#' @examples
#' \dontrun{mynbin(10, 3, 0.4)}
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
