#' myboot2
#'
#' Creates a Bootstrap sample from sample x and plots its statistics
#'
#' @param iter Number of iterations to be made
#' @param x A sample to create a bootstrap from
#' @param fun The function to perform on the dataframe
#' @param alpha The alpha variable, using to find the confidence interval
#' @param cx The amount by which plotting text and symbols should be scaled relative to the default
#' @param ...
#'
#' @return A list of the confidence interval, the function, the sample, the experimental dataframe created and formatted according to the function, and the experimental dataframe created.
#' @export
#'
#' @examples myboot2(iter = 10000,x = c(0,1,3,), fun = "mean", alpha = 0.05, cx = 1.5)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))


  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)


  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)


  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)


  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat=xstat,rs.mat=rs.mat))
}
