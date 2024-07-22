#'@title  datach
#'@description Generate new data and define the data.
#'@param data A data entry is required.
#'@param x The 3 categorical variables that you make matches for.
#'@return A list with data.
#'
#'
datach<-function(data,x){
  A<-subset(data,data[,x]==1) ##??????????
  B<-subset(data,data[,x]==2)##??????????
  C<-subset(data,data[,x]==3) ##?м???????
  dtN <- c("S", "M", "L")
  Rk <- rank(c(dim(A)[1], dim(B)[1], dim(C)[1]),ties.method="last")
  #????list
  filelist <- list(A = A, B = B, C = C)
  newlist <- list(L = data.frame(NULL),
                  M = data.frame(NULL),
                  S = data.frame(NULL))
  newlist[dtN[Rk[1:3]]] <- filelist
  #ת??dt
  L <- as.data.frame.list(newlist[1])
  M <- as.data.frame.list(newlist[2])
  S <- as.data.frame.list(newlist[3])
  #?Ļر?��??
  names(L) <- names(newlist[[1]])
  names(M) <- names(newlist[[2]])
  names(S) <- names(newlist[[3]])
  #newDF_1<-L;newDF_3<-M;newDF_2<-S
  dat1<-list(newDF_1=L,newDF_3=M ,newDF_2=S)
  dat1
}
