#' Title
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-7*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mu,sigma)
  polygon(c(mu-7*sigma,xcurve,a),c(0,ycurve,0),col="Red")


  pnorm(a, mu, sigma)
}
