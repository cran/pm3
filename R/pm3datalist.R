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
  data<-data;covs<-covs
  psid<-seq(1,dim(data)[1])
  var<-c(x,y,covs)
  if (!missing(factor)) {factor<-factor}
  if (!missing(factor)) {
    for(i in factor){
      data[,i] <- as.factor(data[,i])
    }
  }
  macdata<-data[,var];
  macdata$psid<-psid
  macdata<-as.data.frame(macdata)
  macdata<-na.omit(macdata)
  if (!length(levels(factor(macdata[,x])))==3) {stop("X must be 3 categories.")}
  macdata[,x]<-as.numeric(factor(macdata[,x],labels = c(1,2,3)))
  a<-unique(macdata[,x]);a<-sort(a);b<-c(1,2,3)
  if (identical(a,b) !=TRUE)  {stop("X must be 3 categories.")}
  macdata[,x]<-as.factor(macdata[,x])
  macdata
}
