#'@title  pm3
#'@name  pm3
#'@description  Propensity score matching for unordered 3-group data
#'
#'@details  You can use this program for 3 sets of categorical data for propensity score matching.
#'          Assume that the data has 3 different categorical variables. You can use it to perform propensity matching of baseline indicator groupings.
#'          The matching will make the differences in the baseline data smaller.
#'
#'
#'@param data need a dataframe
#'@param x Enter the 3 categorical variables to be matched.If x is a number, it must be of type 1,2,3.
#'@param y Enter the outcome variable for your study.
#'@param covs Covariates. Usually the other fitted variables of the model.This is also usually the baseline variable you need to match.
#'@param factor Define the categorical variables in your data.
#'@param CALIP The number used to match. Usually you don't need to change it. The default is 0.5.
#'@importFrom "tableone" "CreateTableOne"
#'
#'
#'
#'
#'
#'@return A list with data.

#'@format NULL
#'@usage NULL
#'@export
#'@examples
#'bc<-prematurity
#'#####Generate data lists and extract data
#'g<-pm3(data=bc,x="race",y="low",covs=c("age","lwt","ptl"),
#'factor=c("ui","low","smoke"))
#'mbc<-g[["mbc"]]
#'####Compare before and after matching
#'library(tableone)
#'allVars <-c("age", "lwt", "ptl")
#'fvars<-c("ht")
#'tab2 <- CreateTableOne(vars = allVars, strata = "race" ,
#'data = bc, factorVars=fvars,addOverall = TRUE )
#'print(tab2,smd = TRUE)
#'tab1 <- CreateTableOne(vars = allVars, strata = "race" ,
#'data = mbc, factorVars=fvars,addOverall = TRUE )
#'print(tab1,smd = TRUE)
#'


utils::globalVariables(c('pm3datalist',
                         'pm3fit',
                         'na.omit'
))



pm3<-function(data,x,y,covs,factor=NULL,CALIP) {
  if (missing(data)) {stop("data is miss.")}
  if (missing(x)) {stop("x is miss.")}
  if (missing(y)) {stop("y is miss.")}
  if (missing(covs)) {stop("covs is miss.")}
  psid<-seq(1,dim(data)[1]);
  ysdata<-data;ysdata$psid<-psid  #ID
  data<-pm3datalist(data=data,x=x,y=y,covs=covs,factor=factor)
  dat1<-pm3fit(data=data,x=x,y=y,covs=covs)
  newDF_1<-dat1[["newDF_1"]]
  newDF_2<-dat1[["newDF_2"]]
  newDF_3<-dat1[["newDF_3"]]
  jGet<-0
  mtchDf_1<-NULL
  mtchDf_2<-NULL
  mtchDf_3<-NULL
  fMtchDf_1<-NULL
  fMtchDf_2<-NULL
  fMtchDf_3<-NULL
  rowsNum_2<-dim(newDF_2)[1]
  jGet<-0
  mindis<-999.99
  if (missing(CALIP)) {CALIP<-0.5} else {CALIP<-CALIP}
  ##########
  for(i in 1:rowsNum_2){ # Ƕ??ѭ????Ϊ a ???еĸ????? b ????Ѱ??ƥ??????
    rowsNum_3<-dim(newDF_3)[1]
    for(j in 1: rowsNum_3){
      absDist<-abs(newDF_2$ps23[i]-newDF_3$ps23[j]) # ?????????��־???
      deno<-abs(newDF_2$ps23[i]) + abs(newDF_3$ps23[j]) # ?????????��־???
      relDist<-absDist/deno
      if(relDist<= CALIP){
        if(absDist<mindis){
          mindis<-absDist
          jGet<-j
        }
      }
    }
    if(jGet>0){
      tempDf<-newDF_2[i,]
      mtchDf_2<- rbind(mtchDf_2,tempDf) # ƥ???????��δ????µ? a ?????ݿ???
      tempDf<-newDF_3[jGet,]
      mtchDf_3<-rbind(mtchDf_3,tempDf) # ƥ???????��δ????µ? b ?????ݿ???
      tempDf<-NULL
      newDF_3 <- newDF_3[-jGet,] # ??ԭ b ?????ݿ???ɾ????ƥ???????ݣ??????´?ѭ???ظ?ƥ??
    }
  }
  ########
  rowsNum_mtch23<-dim(mtchDf_2)[1]
  for(i in 1 : rowsNum_mtch23){    #Ƕ??ѭ????Ϊƥ???? a??b ???????ݶԣ??? c ????Ѱ??ƥ??????
    jGet<-0
    mindis<-999.99
    rowsNum_1<-dim(newDF_1)[1]
    for(j in 1 : rowsNum_1){
      absDist12<-abs(newDF_1$ps12[j]-mtchDf_2$ps12[i]) # ʹ?á?ps ac ?????? a??c ?????????��־???
      absDist13<-abs(newDF_1$ps13[j]-mtchDf_3$ps13[i]) # ʹ?á?ps bc ?????? b??c ?????????��־???
      deno<-abs(newDF_1$ps12[j])+abs(mtchDf_2$ps12[i]) + abs(newDF_1$ps13[j]) + abs(mtchDf_3$ps13[i])
      relDist<-(absDist12 + absDist13)/deno # ?????????��־???
      if(relDist<-CALIP){
        if((absDist12 + absDist13)<mindis){
          mindis<-absDist12 + absDist13
          jGet<-j
        }
      }
    }
    if(jGet>0){
      tempDf<-newDF_1[jGet,]
      fMtchDf_1<-rbind(fMtchDf_1,tempDf) # ƥ???????��δ????µ? c ???????ݿ???
      tempDf<-mtchDf_2[i,]
      fMtchDf_2<-rbind(fMtchDf_2,tempDf) # ƥ???????��δ????µ? a ???????ݿ???
      tempDf<-mtchDf_3[i,]
      fMtchDf_3<-rbind(fMtchDf_3,tempDf) # ƥ???????��δ????µ? b ???????ݿ???
      tempDf<-NULL
      newDF_1<-newDF_1[-jGet,] # ??ԭ c ?????ݿ???ɾ????ƥ???????ݣ??????´?ѭ???ظ?ƥ??
    }
  }
  mbc<-rbind(fMtchDf_1,fMtchDf_2,fMtchDf_3)
  mbc<-ysdata[match(mbc$psid,ysdata$psid),]
  fMtchDf_1<-ysdata[match(fMtchDf_1$psid,ysdata$psid),]
  fMtchDf_2<-ysdata[match(fMtchDf_2$psid,ysdata$psid),]
  fMtchDf_3<-ysdata[match(fMtchDf_3$psid,ysdata$psid),]
  out<-list(mbc=mbc,fMtchDf_1=fMtchDf_1,fMtchDf_2=fMtchDf_2,fMtchDf_3=fMtchDf_3)
  out
}
