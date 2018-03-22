library(RJSONIO)

library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)
library(dplyr)
library(data.table)

# A script to generate functions that allow us to construct the 16 separate
# pieces of the SVI for all census tracts.

# Each function has 2 inputs:

# 1. A state
# 2. a year
# 3. an API key

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

api_call.fn <- function(year,series,key){
  
  
  if(year %in% c(2009,2010)){
    call <- paste('https://api.census.gov/data/',year,'/acs5?key=',key,'&get=',series,',NAME&for=state:*',
                  sep="")
  }else if(year %in% c(2011,2012,2013,2014)){

    call <- paste('https://api.census.gov/data/',year,'/acs5?get=NAME,',series,'&for=state:*&key=',
                  key,sep="")
  }else{

    call <- paste('https://api.census.gov/data/',year,'/acs/acs5?get=NAME,',series,'&for=state:*&key=',
                  key,sep="")
    
  }
  
  return(call)  
}
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

data.function <- function(year,series,series.name,key){
  call <- api_call.fn(year=year,key=key,series=series)
  
  df <- fromJSON(call)
  df <- data.frame(rbindlist(lapply(df,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],data_series=series,data_name=series.name))
  })
  )) %>% filter(row_number() > 1)
  return(df)
}
