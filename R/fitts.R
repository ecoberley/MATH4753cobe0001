#' Title
#'
#' @param x1
#' @param y1
#' @param x2
#' @param y2
#' @param width
#'
#' @return
#' @export
#'
#' @examples
fitts = function(x1, y1, x2, y2, width){
  A = sqrt((x2-x1)^2 + (y2-y1)^2)
  W = width
  ID = log2(A/W + 1)
  MT = 50 + 150*ID
  sprintf("MT = %g ms, A = %g px, W = %g px", MT, A, W)
}
