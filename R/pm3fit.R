#'@title  pm3fit
#'@name   pm3fit
#'@description Generate propensity scores and generate the data to be matched.
#'@param data A data entry is required.
#'@param x The 3 categorical variables that you make matches for.
#'@param y Your result variable.
#'@param covs Enter the relevant covariates.
#'@param factor Define categorical variables. You can leave this blank, but if you do, fill in at least 2.
#'@importFrom "stats" "predict"
#'@return A list with data.
#'
#'
utils::globalVariables(c('predict'
))

pm3fit<-function(data,x,y,covs,factor) {
  if (missing(covs)) {stop("covs is miss.")}
  bc<-data
  df12<-subset(bc,data[,x]==1|data[,x]==2)
  df13<-subset(bc,data[,x]==1|data[,x]==3)
  df23<-subset(bc,data[,x]==2|data[,x]==3)
  formula12 <- paste0("glm(",y,"~", paste0(covs, collapse=" + "),",data = df12",",family = binomial(link = 'logit'))",sep="")
  fit12<-eval(parse(text=formula12))
  formula13 <- paste0("glm(",y,"~", paste0(covs, collapse=" + "),",data = df13",",family = binomial(link = 'logit'))",sep="")
  fit13<-eval(parse(text=formula13))
  formula23 <- paste0("glm(",y,"~", paste0(covs, collapse=" + "),",data = df23",",family = binomial(link = 'logit'))",sep="")
  fit23<-eval(parse(text=formula23))
  ######ѡ??????
  d<-datach(bc,x) #????????ɸѡ
  newDF_2<-d[["newDF_2"]] ##??????????
  newDF_1<-d[["newDF_1"]] ##??????????
  newDF_3<-d[["newDF_3"]] ##?м???????
  ############newDF_1????ps?��?
  newDF_1$ps12<-predict(fit12,newdata =newDF_1)
  newDF_1$ps13<-predict(fit13,newdata =newDF_1)
  newDF_1$ps23<-predict(fit23,newdata =newDF_1)
  ############newDF_2????ps?��?
  newDF_2$ps12<-predict(fit12,newdata =newDF_2)
  newDF_2$ps13<-predict(fit13,newdata =newDF_2)
  newDF_2$ps23<-predict(fit23,newdata =newDF_2)
  ############newDF_3????ps?��?
  newDF_3$ps12<-predict(fit12,newdata =newDF_3)
  newDF_3$ps13<-predict(fit13,newdata =newDF_3)
  newDF_3$ps23<-predict(fit23,newdata =newDF_3)
  d<-list(newDF_1=newDF_1,newDF_2=newDF_2,newDF_3=newDF_3)
  d
}
