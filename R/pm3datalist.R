#'@title  pm3datalist
#'@description Identification and formatting of data.
#'@param data A data entry is required.
#'@param x The 3 categorical variables that you make matches for.
#'@param y Your result variable.
#'@param covs Enter the relevant covariates.
#'@param factor Define categorical variables. You can leave this blank, but if you do, fill in at least 2.
#'@return A data.
#'
#'
#'
pm3datalist<-function(data,x,y,covs,factor=NULL){
  data<-as.data.frame(data)
  data<-na.omit(data)
  if (!missing(factor)) {fac<-factor}
  data[,x]<-as.numeric(as.factor(data[,x]))
  a<-unique(data[,x]);a<-sort(a);b<-c(1,2,3)
  if (identical(a,b) !=TRUE)  {stop("X must be 3 categories.")}
  if (!missing(factor)) {data[,c(fac)] <- lapply(data[,c(fac)], factor)}
  data
}
