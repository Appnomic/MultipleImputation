---
title: "MI"
author: "Jaisri"
date: "April 15, 2016"
output: html_document
---

library(RMySQL)
library(doParallel)
library(RODBC)
library(timeSeries)
library(forecast)
library(timeDate)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(Amelia)
library(mice)

avgdf<-function(df,cols,method,window,no_of_impute){
  t1<-Sys.time()
  match<-length(intersect(cols,colnames(df)))
  if(length(cols)==match)
  {
    types<-as.vector(sapply(df,class))
    char<-length(grep("chara",types))
    if(char == 0)
    {
      datasets=multiple_imputation(df,cols,method,window,no_of_impute)
      result<-Reduce("+",datasets)/no_of_impute
      cat("Total Time:" ,(Sys.time()-t1))
      cat("\n")
      return(result)
    }
    else
    {
      print("The provided data frame has character columns, Please provide the data frame which has only numeric/interger columns")
    }
  }
  else
  {
  print("The provided column names doesn't match with column names of dataframe")
  }
}



multiple_imputation<-function(x,cols,method,n,times){
  foreach (f=1:times) %do% 
  {
    MI(x,cols,method,n)
    }
}




MI<-function(x,cols,method,n,times)
{
  t1<-Sys.time()
  rc<-nrow(x)
  cc<-length(cols)
  y=do(x,cols,method,n)
  y<-y[,cols]
  temp<-x[,cols]
  for (j in cols)
  {	
    naCount<-sum(is.na(x[,j]))
    mean<-mean(x[,j],na.rm = T)
    sd<-sd(x[,j],na.rm = T) 
    if(is.na(sd)=="TRUE")
    {sd<-1}
    error<-rnorm(naCount,mean=mean,sd=sd)
    k<-0
    for (i in 1:rc)
    { 
      if(is.na(temp[i,j])=="TRUE")
      { 
        colno<-grep(j,colnames(temp))
        count<-length(which(is.na(temp[i,-colno])))
        k<-k+1
        if(count >= 1)
        { 
          fit<-lm(temp[,j] ~ .,data=y[,-colno], na.action = na.omit)  
          x[i,j]<-predict(fit,y[i,-colno]) + error[k]
        }
        else
        {
          colno<-grep(j,colnames(x))
          fit<-lm(temp[,j] ~ .,data=x[,-colno], na.action = na.omit)
          x[i,j]<-predict(fit,x[i,-colno])+ error[k]
        }
        }
    }
  temp[,j]<-x[,j]
    }
  #cat("Total Time:" ,(Sys.time()-t1))
  return(x)
} 
