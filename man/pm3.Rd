% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pm3.R
\name{pm3}
\alias{pm3}
\title{pm3}
\arguments{
\item{data}{need a dataframe}

\item{x}{Enter the 3 categorical variables to be matched.If x is a number, it must be of type 1,2,3.}

\item{y}{Enter the outcome variable for your study.}

\item{covs}{Covariates. Usually the other fitted variables of the model.This is also usually the baseline variable you need to match.}

\item{factor}{Define the categorical variables in your data.}

\item{CALIP}{The number used to match. Usually you don't need to change it. The default is 0.5.}
}
\value{
A list with data.
}
\description{
Propensity score matching for unordered 3-group data
}
\details{
You can use this program for 3 sets of categorical data for propensity score matching.
         Assume that the data has 3 different categorical variables. You can use it to perform propensity matching of baseline indicator groupings.
         The matching will make the differences in the baseline data smaller.
}
\examples{
bc<-prematurity
#####Generate data lists and extract data
g<-pm3(data=bc,x="race",y="low",covs=c("age","lwt","ptl"),
factor=c("ui","low","smoke"))
mbc<-g[["mbc"]]
####Compare before and after matching
library(tableone)
allVars <-c("age", "lwt", "ptl")
fvars<-c("ht")
tab2 <- CreateTableOne(vars = allVars, strata = "race" ,
data = bc, factorVars=fvars,addOverall = TRUE )
print(tab2,smd = TRUE)
tab1 <- CreateTableOne(vars = allVars, strata = "race" ,
data = mbc, factorVars=fvars,addOverall = TRUE )
print(tab1,smd = TRUE)

}
