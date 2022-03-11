#' Title
#'
#' @param csv The name of a csv file
#' @param dird The directory the file is in
#'
#' @return The data frame inside the csv file
#' @export
#'
#' @examples
#' \dontrun{mpg.df=myread("EPAGAS.csv")}
myread=function(csv,dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
