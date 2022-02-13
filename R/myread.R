#' Title
#'
#' @param csv The path to a csv file
#'
#' @return The data frame inside the csv file
#' @export
#'
#' @examples
#' \dontrun{mpg.df=myread("EPAGAS.csv")}
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}