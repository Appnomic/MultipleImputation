---
title: "mean"
author: "Jaisri"
date: "April 1, 2016"
output: html_document
---

dopar<-function(x,cols,method,n)
{ Time1<-Sys.time()
  foreach (f=cols) %dopar% 
    { 
      x[,f]<-methods(x,f,method,n)
    }
      Execution<-(Sys.time()-Time1)
      #cat("Total Time:" ,Execution)
      return(x)
  }

do<-function(x,cols,method,n)
{ Time1<-Sys.time()
  foreach (f=cols) %do% 
  {
    x[,f]<-methods(x,f,method,n)
  }
    Execution<-(Sys.time()-Time1)
     # cat("Total Time:" ,Execution)
    return(x)
}


methods<- function(x,f,method,n)
{   
  count<-nrow(x)
  ## Checks if the first element is NA if yes then replace with 0
  if(is.na(x[1,f])=="TRUE")
  {x[1,f]<-0
  }
  if(count>n)
  {
    ### Method is SMA
    if(method=="SMA")
    { 
      ## For loop starts if the values are less than n
      for (i in 2:count)
      { 
        if(is.na(x[i,f])=="TRUE")
        {
          if(i<=n)
          {
            x[i,f]<-mean(x[1:(i-1),f])
          }
          ### if the values are greater than n
          else
          {
            x[i,f]<-mean(x[(i-n):(i-1),f])
          }
        }
      }  
    }
    
    ### Method is EMA	
    else
    {
      mul<-(2/(n+1))
      ## For loop starts if the values are less than n
      for (i in 2:count)
      { 
        if(is.na(x[i,f])=="TRUE")
        {
          if(i<=n)
          {
            x[i,f]<-mean(x[1:(i-1),f])
          }
          ### if the values are greater than n
          else
          {
            if(i==n+1)
            {	avg<-mean(x[(i-n):(i-1),f])
            x[i,f]<-mul*(x[i-1,f]-avg)+avg
            }
            else
            {
              x[i,f]<-(mul*(x[i-1,f]-x[i-2,f])+x[i-2,f])
            }
          }
        }
      }
    }
  }
  x[,f]<-x[,f]
}


