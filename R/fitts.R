#' Fitt's Distance
#'
#' Calculate the Fitt's distance from one element to another. For use in UX design.
#'
#' @param x1 The x-coordinate of the first object
#' @param y1 The y-coordinate of the first object
#' @param x2 The x-coordinate of the second object
#' @param y2 The y-coordinate of the second object
#' @param width The width of the second object
#'
#' @return Formatted string of Fitts's distance, showing Movement Time, the regression coefficient, and the width of the element.
#' @export
#'
#' @examples fitts(x1 = 10, y1 = 15, x2 = 50, y2 = 76, width = 120)
fitts = function(x1, y1, x2, y2, width){
  A = sqrt((x2-x1)^2 + (y2-y1)^2)
  W = width
  ID = log2(A/W + 1)
  MT = 50 + 150*ID
  sprintf("MT = %g ms, A = %g px, W = %g px", MT, A, W)
}
