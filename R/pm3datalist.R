#'@title  pm3datalist
#'@description Identification and formatting of data.
#'@param data A data entry is required.
#'@param x The 3 categorical variables that you make matches for.
#'@param y Your result variable.
#'@param covs Enter the relevant covariates.
#'@param factor Define categorical variables.
#'@return A data.
#'
#'
#'
pm3datalist<-function(data,x,y,covs,factor=NULL){
  data<-as.data.frame(data)
  data<-na.omit(data)
  fac<-factor
  data[,x]<-as.numeric(as.factor(data[,x]))
  a<-unique(data[,x]);b<-c(1,2,3)
  if (identical(a,b) !=TRUE)  {stop("X must be 3 categories.")}
  if (!missing(factor)) {
    for(i in factor){
      data[,i] <- as.factor(data[,i])
    }
  }
  data
}
