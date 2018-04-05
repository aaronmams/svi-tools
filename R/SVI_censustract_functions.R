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

api_call.fn <- function(year,state,series,key){
  
  
if(year %in% c(2009,2010)){
  call <- paste('https://api.census.gov/data/',year,'/acs5?key=',key,'&get=',series,',NAME&for=tract:*&in=state:',state,
                 sep="")
}else if(year %in% c(2011,2012,2013,2014)){
  call <- paste('https://api.census.gov/data/',year,'/acs5?get=NAME,',series,'&for=tract:*&in=state:',state,'&key=',
                 key,sep="")
}else{
  call <- paste('https://api.census.gov/data/',year,'/acs/acs5?get=NAME,',series,'&for=tract:*&in=state:',state,'&key=',
                 key,sep="")
  
}

return(call)  
}
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################


# this get data function helps us pull together a metric when
# it is comprised of multiple different series

get.data.15 <- function(series,state,key){
  resURL <- paste('https://api.census.gov/data/2015/acs/acs5?get=NAME,',
                  series,'&for=tract:*&in=state:',state,'&key=',
                  key,sep="")
  ljson<-fromJSON(resURL)
  df <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],
                      series=series.df$label[which(series.df$series==series)]))
  })))) %>% filter(row_number() >1)
  return(df)  
}

get.data.11_14 <- function(s,year,state,key){
  resURL <- paste('https://api.census.gov/data/',year,'/acs5?get=NAME,',
                  s,'&for=tract:*&in=state:',state,'&key=',
                  key,sep="")
  ljson<-fromJSON(resURL)
  df <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],
                      series=series.df$label[which(series.df$series==s)]))
  })))) %>% filter(row_number() >1)
  return(df)  
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

total_households.fn <- function(state,year,key){
  
  series1 <- 'B11001_001E'

  call <- api_call.fn(year=year,state=state,key=key,series='B11001_001E')
  
  total_households <- fromJSON(call)
  total_households <- data.frame(rbindlist(lapply(total_households,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],total_households=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() > 1)
return(total_households)
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

total_pop.fn <- function(year,state,key){

# total population: 'B01001_001E'
  call <- api_call.fn(year=year,state=state,key=key,series='B01001_001E')
  
  total_pop <- fromJSON(call)
  total_pop <- data.frame(rbindlist(lapply(total_pop,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],total_pop=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() > 1)

    return(total_pop)

}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# population by age and sex

# males
pop_age_male.fn <- function(year,state,key){
series.df <- data.frame(series=c('B01001_003E','B01001_004E','B01001_005E','B01001_006E','B01001_007E','B01001_008E',
                                 'B01001_009E','B01001_010E','B01001_011E','B01001_012E','B01001_013E',
                                 'B01001_014E','B01001_015E','B01001_016E','B01001_017E','B01001_018E','B01001_019E',
                                 'B01001_020E','B01001_021E','B01001_022E','B01001_023E','B01001_024E','B01001_025E'),
                        label=c('M_5','M5_9','M10_14','M15_17','M18_19','M20','M21','M22_24','M25_29',
                                'M30_34','M35_39','M40_44','M45_49','M50_54','M55_59','M60_61','M62_64',
                                'M65_66','M67_69','M70_74','M75_79','M80_84','M85'))
series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

pop.age.male <- list()
for(i in 1:nrow(series.df)){

  call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])
  df <- fromJSON(call) 
  df <- tbl_df(data.frame(rbindlist(lapply(df,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],
                      series=series.df$label[i]))
  })))) %>% filter(row_number() >1)
  pop.age.male[[i]] <- df
}  

return(data.frame(rbindlist(pop.age.male)))
}


pop_age_female.fn <- function(year,state,key){
  
  series.df <- data.frame(series=c(                                 
    'B01001_027E','B01001_028E','B01001_029E','B01001_030E','B01001_031E',
    'B01001_032E','B01001_033E','B01001_034E','B01001_035E','B01001_036E',
    'B01001_037E','B01001_038E','B01001_039E','B01001_040E','B01001_041E','B01001_042E',
    'B01001_043E','B01001_044E','B01001_045E','B01001_046E','B01001_047E','B01001_048E',
    'B01001_049E'),
    label=c('F_5','F5_9','F10_14','F15_17','F18_19','F20','F21','F22_24','F25_29',
            'F30_34','F35_39','F40_44','F45_49','F50_54','F55_59','F60_61','F62_64',
            'F65_66','F67_69','F70_74','F75_79','F80_84','F85'))
  
  series.df$series <- as.character(series.df$series)
  series.df$label <- as.character(series.df$label)

pop.age.female <- list()
for(i in 1:nrow(series.df)){
  
  call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])
  df <- fromJSON(call) 
  df <- tbl_df(data.frame(rbindlist(lapply(df,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],
                      series=series.df$label[i]))
  })))) %>% filter(row_number() >1)
  pop.age.female[[i]] <- df
}  

return(data.frame(rbindlist(pop.age.female)))
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# A function to get percent of households below the poverty line from the ACS 5 year
# estimates for a particular year

#------------------------------------------------------------------------------
# population for which poverty status is determined B17001_001E
# population with income below the poverty line in the last 12 months B17001_002E

below_poverty.fn <- function(state,year,key){

series1 <- 'B17001_001E'
series2 <- 'B17001_002E'

call1 <- api_call.fn(year=year,state=state,key=key,series=series1)
call2 <- api_call.fn(year=year,state=state,key=key,series=series2)


# population for whom poverty status is determined
pov.pop <- fromJSON(call1)
pov.pop <- data.frame(rbindlist(lapply(pov.pop,function(x){
  x<-unlist(x)
  return(data.frame(name=x[1],pov.pop=x[2],state=x[3],county=x[4],tract=x[5]))
})
)) %>% filter(row_number() > 1)


# income in the past 12 months below poverty level
pov <- fromJSON(call2)
pov <- data.frame(rbindlist(lapply(pov,function(x){
  x<-unlist(x)
  return(data.frame(name=x[1],pov=x[2],state=x[3],county=x[4],tract=x[5]))
})
)) %>% filter(row_number() >1)

pov <- tbl_df(pov) %>% inner_join(pov.pop,by=c('name','state','county','tract')) %>%
  mutate(pov.pct = as.numeric(as.character(pov))/as.numeric(as.character(pov.pop)))

return(pov)
}
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

unemp.function <- function(year,state,key){
  
#---------------------------------------------------------------------------------
# unemployment age 16 and up looking for work
# B23001_008E - total male 16-19, in labor force, civilian, unemployed
# B23001_015E - total male 20-21, in labor force, civilian, unemployed
# B23001_022E - total male 22-24, in labor force, civilian, unemployed
# B23001_029E - total male 25-29, in labor force, civilian, unemployed
# B23001_036E - total male 30-34, in labor force, civilian, unemployed
# B23001_043E - total male 35-44, in labor force, civilian, unemployed
# B23001_050E - total male 45-54, in labor force, civilian, unemployed
# B23001_057E - total male 55-59, in labor force, civilian, unemployed
# B23001_064E - total male 60-61, in labor force, civilian, unemployed
# B23001_071E - total male 62-64, in labor force, civilian, unemployed

# B23001_094E - total female 16-19, in labor force, civilian, unemployed
# B23001_101E - total female 20-21, in labor force, civilian, unemployed
# B23001_108E - total female 22-24, in labor force, civilian, unemployed
# B23001_115E - total female 25-29, in labor force, civilian, unemployed
# B23001_122E - total female 30-34, in labor force, civilian, unemployed
# B23001_129E - total female 35-44, in labor force, civilian, unemployed
# B23001_136E - total female 45-54, in labor force, civilian, unemployed
# B23001_143E - total female 55-59, in labor force, civilian, unemployed
# B23001_150E - total female 60-61, in labor force, civilian, unemployed
# B23001_157E - total female 62-64, in labor force, civilian, unemployed


  
  series.df <- data.frame(series=c(
  'B23001_008E','B23001_015E','B23001_022E','B23001_029E','B23001_036E','B23001_043E','B23001_050E','B23001_057E',
  'B23001_064E','B23001_071E','B23001_094E','B23001_101E','B23001_108E','B23001_115E','B23001_122E','B23001_129E',
  'B23001_136E','B23001_143E','B23001_150E','B23001_157E'),
  labels=c('m16_19','m20_21','m22_24','m25_29','m30_34','m35_44','m45_54','m55_59','m60_61','m62_64',
           'f16_19','f20_21','f22_24','f25_29','f30_34','f35_44','f45_54','f55_59','f60_61','f62_64'))

unemp <- list()
for(i in 1:nrow(series.df)){

  call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])  

df <- fromJSON(call)
df <- data.frame(rbindlist(lapply(df,function(x){
  x<-unlist(x)
  return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$labels[i]))
})
)) %>% filter(row_number() > 1)

unemp[[i]] <- df    
}  

unemp<-tbl_df(data.frame(rbindlist(unemp)))  %>%
        group_by(name,state,county,tract) %>%
        summarise(unemp=sum(as.numeric(as.character(value)),na.rm=T))
  

#labor force population
# B23001_004E - total male 16-19, in labor force
# B23001_011E - total male 20-21, in labor force
# B23001_018E - total male 22-24, in labor force
# B23001_025E - total male 25-29, in labor force
# B23001_032E - total male 30-34, in labor force
# B23001_039E - total male 35-44, in labor force
# B23001_046E - total male 45-54, in labor force
# B23001_053E - total male 55-59, in labor force
# B23001_060E - total male 60-61, in labor force
# B23001_067E - total male 62-64, in labor force

# B23001_090E - total female 16-19, in labor force
# B23001_097E - total female 20-21, in labor force
# B23001_104E - total female 22-24, in labor force
# B23001_111E - total female 25-29, in labor force
# B23001_118E - total female 30-34, in labor force
# B23001_125E - total female 35-44, in labor force
# B23001_132E - total female 45-54, in labor force
# B23001_139E - total female 55-59, in labor force
# B23001_146E - total female 60-61, in labor force
# B23001_153E - total female 62-64, in labor force

series.df <- data.frame(series=c(
  'B23001_004E','B23001_011E','B23001_018E','B23001_025E','B23001_032E','B23001_039E','B23001_046E','B23001_053E',
  'B23001_060E','B23001_067E','B23001_090E','B23001_097E','B23001_104E','B23001_111E','B23001_118E','B23001_125E',
  'B23001_132E','B23001_139E','B23001_146E','B23001_153E'),
  labels=c('m16_19','m20_21','m22_24','m25_29','m30_34','m35_44','m45_54','m55_59','m60_61','m62_64',
           'f16_19','f20_21','f22_24','f25_29','f30_34','f35_44','f45_54','f55_59','f60_61','f62_64'))

labor.force <- list()
for(i in 1:nrow(series.df)){
  call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])  
  
  df <- fromJSON(call)
  df <- data.frame(rbindlist(lapply(df,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$labels[i]))
  })
  )) %>% filter(row_number() > 1)
  
  labor.force[[i]] <- df    
}  

labor.force<-tbl_df(data.frame(rbindlist(labor.force))) %>%
              group_by(name,state,county,tract) %>%
              summarise(labor.force=sum(as.numeric(as.character(value)),na.rm=T))


unemp <- unemp %>% inner_join(labor.force,by=c('name','state','county','tract')) %>%
  mutate(unemp.rate=as.numeric(as.character(unemp))/as.numeric(as.character(labor.force)))

return(unemp)
}

#---------------------------------------------------------------------------------
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

#--------------------------------------------------------------------------------
# education - population 25 and over with less than a high school degree
# S0502_C01_047E - estimated population 25 and over
# S0502_C01_048E - population 25 and over with less than HS degree

edu.fn<-function(year,state,key){

  series.df <- data.frame(series=c(
    'B15001_013E','B15001_021E',
    'B15001_029E','B15001_037E',
    'B15001_054E','B15001_062E',
    'B15001_070E','B15001_078E'),
    labels=c('m_25_34_nodiploma','m_35_44_nodiploma',
             'm_45_64_nodiploma','m_65_plus_nodiploma',
             'f_25_34_nodiploma','f_35_44_nodiploma',
             'f_45_64_nodiploma','f_65_plus_nodiploma'))

    counts.df <- data.frame(series=c('B15001_011E','B15001_019E','B15001_027E','B15001_035E',
                                   'B15001_052E','B15001_060E','B15001_068E','B15001_076E'),
                          labels=c('m_25_34','m_35_44','m_45_64','m_65_plus',
                                   'f_25_34','f_35_44','f_45_64','f_65_plus'))
tmp.df <- list()
for(i in 1:nrow(series.df)){
  call1 <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])
  call2 <- api_call.fn(year=year,state=state,key=key,series=counts.df$series[i])

  total.age <- fromJSON(call2)
  total.age <- data.frame(rbindlist(lapply(total.age,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],pop.age=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() > 1)

  edu.age <- fromJSON(call1)  
  edu.age <- data.frame(rbindlist(lapply(edu.age,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],edu.age=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() > 1)

  z <- edu.age %>% inner_join(total.age,by=c('name','state','county','tract')) %>%
         mutate(age.range=as.character(counts.df$labels[i]))
tmp.df[[i]] <- z        
}  

edu<- data.frame(rbindlist(tmp.df)) %>%
      group_by(name,state,county,tract) %>%
      summarise(no_diploma=sum(as.numeric(as.character(edu.age)),na.rm=T)
                ,pop=sum(as.numeric(as.character(pop.age)),na.rm=T)) %>%
     mutate(pct_no_diploma=no_diploma/pop)

return(edu)
}
#--------------------------------------------------------------------------------------------

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

#--------------------------------------------------------------------------------------------
# per capital income

pci.fn <- function(state,year,key){
# B19301_001E - estimated per capital income in the past 12 months

  
series1 <- 'B19301_001E'
call <- api_call.fn(year=year,state=state,key=key,series=series1)

ljson<-fromJSON(call)  

pci <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
  x <- unlist(x)
  return(data.frame(name=x[1],pci=x[2],state=x[3],county=x[4],tract=x[5]))
})))) %>% filter(row_number() >1)

return(pci)
}
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


#-------------------------------------------------------------------------------------------
# health insurance

health_ins.fn <- function(state,year,key){

# B27001_
# 005E, 008E, 011E, 014E, 017E, 020E, 023E -males without health insurance
# 033E, 036E, 039E, 042E, 045E, 048E, 051E -females without health insurance
  
  series.df <- data.frame(series=c(
    'B27001_005E','B27001_008E','B27001_011E','B27001_014E','B27001_017E','B27001_020E','B27001_023E',
    'B27001_033E','B27001_036E','B27001_039E','B27001_042E','B27001_045E','B27001_048E','B27001_051E'),
    labels=c('m6','m6_17','m18_24','m25_34','m35_44','m45-54','m55_64',
             'f6','f6_17','f18_24','f25_34','f35_44','f45_54','f55_64'))
  
  ins <- list()
  for(i in 1:nrow(series.df)){
    call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])

    df <- fromJSON(call)
    df <- data.frame(rbindlist(lapply(df,function(x){
      x<-unlist(x)
      return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$labels[i]))
    })
    )) %>% filter(row_number() > 1)
    
    ins[[i]] <- df    
  }  
  
  ins<-tbl_df(data.frame(rbindlist(ins)))  %>%
    group_by(name,state,county,tract) %>%
    summarise(no_health_insurance=sum(as.numeric(as.character(value)),na.rm=T))

  return(ins)
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


#---------------------------------------------------------------------------------------------------
# Age 65 or less with a disability

disability.fn <- function(state,year,key){

  
  # get the list of data series
  series.df <- data.frame(series=c('B18101_004E','B18101_007E','B18101_010E','B18101_013E',
                                   'B18101_023E','B18101_026E','B18101_029E','B18101_032E'),
                          label=c('dis_m_5','dis_m_5_17','dis_m_18_34','dis_m_35_64',
                                  'dis_f_5','dis_f_5_17','dis_f_18_34','dis_f_35_64'))
  
  disable <- list()
  for(i in 1:nrow(series.df)){
    call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])
    df <- fromJSON(call)
    df <- data.frame(rbindlist(lapply(df,function(x){
      x<-unlist(x)
      return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$label[i]))
    })
    )) %>% filter(row_number() > 1)
    
    disable[[i]] <- df    
  }  
  
  disable<-tbl_df(data.frame(rbindlist(disable)))  %>%
    group_by(name,state,county,tract) %>%
    summarise(disability=sum(as.numeric(as.character(value)),na.rm=T))

return(disable)
}
#---------------------------------------------------------------------------------------------------
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# Single parent household, female head of household, no spouse present, as a 
# % of households

single_mom.fn <- function(state,year,key){
# not sure which exact series people generally use for this but I'm going to use:
# B11001_006E - estimate!total!family households!other family!female householder, no husband present

# female head of household no spouse present: 'B11001_006E'
  call <- api_call.fn(year=year,state=state,key=key,series='B11001_006E')
  ljson<-fromJSON(call)  
  
  single.mom <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],single.mom=x[2],state=x[3],county=x[4],tract=x[5]))
  })))) %>% filter(row_number() >1)

  return(single.mom)  
}


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

total_white.fn <- function(state,year,key){

# B01001A_001E - total white population
  call <- api_call.fn(year=year,state=state,key=key,series='B01001A_001E')
  
  ljson<-fromJSON(call)  
  white_alone <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],white_alone=x[2],state=x[3],county=x[4],tract=x[5]))
  })))) %>% filter(row_number() >1)
  
  return(white_alone)  
  
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

limited_english.fn <- function(state,year,key){

#B16002_004E, _007E, _010E, _013E
# Spanish, Other Indo-European, Asian and Pacific Islander, Other languages
# household language by household limited english speaking status
  
  # get the list of data series
  series.df <- data.frame(series=c('B16002_004E','B16002_007E','B16002_010E','B16002_013E'),
                          label=c('limited_eng_span','limited_eng_IE','limited_english_AP','limited_english_oth'))
  
  limited_eng <- list()
  for(i in 1:nrow(series.df)){
    call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])
    df <- fromJSON(call)
    df <- data.frame(rbindlist(lapply(df,function(x){
      x<-unlist(x)
      return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$label[i]))
    })
    )) %>% filter(row_number() > 1)
    
    limited_eng[[i]] <- df    
  }  
  
  limited_eng<-tbl_df(data.frame(rbindlist(limited_eng)))  %>%
    group_by(name,state,county,tract) %>%
    summarise(limited_eng_hh=sum(as.numeric(as.character(value)),na.rm=T))
  
  return(limited_eng)

}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Total Housing Units

total_hu.fn <- function(state,year,key){


# B25001_001E - total housing units
  call <- api_call.fn(year=year,state=state,key=key,series='B25001_001E')
  
  
  ljson<-fromJSON(call)  
  
  total_housing <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
    x <- unlist(x)
    return(data.frame(name=x[1],total_housing_units=x[2],state=x[3],county=x[4],tract=x[5]))
  })))) %>% filter(row_number() >1)
  
  return(total_housing)  
  

}
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# Housing in structures with 10 or more units per structure

housing_structures.fn <- function(state,year,key){
  
  #B25032_008E - total owner occupied housing units with 10-19 units in structure
  #B25032_009E - total owner occupied housing units with 20-49 units in structure
  #B25032_010E - total owner occupied housing units with 50 or more units in structure
  
  #B25032_019E - total renter occupied housing units with 10 - 19 units in structure
  #B25032_020E - total renter occupied housing units with 20 - 49 units in structure
  #B25032_021E - total renter occupied housing units with 50 or more units in structure
  
  # get the list of data series
  series.df <- data.frame(series=c('B25032_008E','B25032_009E','B25032_010E','B25032_019E',
                                   'B25032_020E','B25032_021E'),
                          label=c('units10_19_oo','units_20_49_oo','units_50_oo','units_10_19_ro',
                                  'units_20_49_ro','units_50_ro'))
  
  units_per_structure <- list()
  for(i in 1:nrow(series.df)){
    call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])

    df <- fromJSON(call)
    df <- data.frame(rbindlist(lapply(df,function(x){
      x<-unlist(x)
      return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$label[i]))
    })
    )) %>% filter(row_number() > 1)
    
    units_per_structure[[i]] <- df    
  }  
  
  units_per_structure <-tbl_df(data.frame(rbindlist(units_per_structure)))  %>%
    group_by(name,state,county,tract) %>%
    summarise(more_than_10_units=sum(as.numeric(as.character(value)),na.rm=T))
  
  return(units_per_structure)
  
}


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Mobile Homes

#Mobile homes as a percent of the housing stock

# B25032_022E - renter occupied mobile homes
# B25032_011E - owner occupied mobile homes


mobile_homes.fn <- function(state,year,key){
  series1 <- 'B25032_022E'
  series2 <- 'B25032_011E'
  
  call1 <- api_call.fn(year=year,state=state,key=key,series=series1)
  call2 <- api_call.fn(year=year,state=state,key=key,series=series2)
  # renter occupied mobile homes
  mobile_ro <- fromJSON(call1)
  mobile_ro <- data.frame(rbindlist(lapply(mobile_ro,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],mobile_ro=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() > 1)
  
  
  # owner occupied mobile homes
  mobile_oo <- fromJSON(call2)
  mobile_oo <- data.frame(rbindlist(lapply(mobile_oo,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],mobile_oo=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() >1)

mobile <- tbl_df(mobile_ro) %>%
          inner_join(mobile_oo,by=c('name','state','county','tract')) %>%
          mutate(total_mobile=as.numeric(as.character(mobile_ro))+as.numeric(as.character(mobile_oo)))
return(mobile)    
  
}  


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

#Occupants per room

# summary table:
# B25014_005E - owner occupied 1.01 - 1.5 occupants per room
# B25014_006E - owner occupied 1.51 - 2 occupants per room
# B25014_007E - owner occupied 2.01 or more occupants per room

# B25014_011E - renter occupied 1.01 - 1.5 occupants per room
# B25014_012E - renter occupied 1.51 - 2 occupants per room
# B25014_013E - renter occupied 2.01 or more occupants per room

# B25001# - total housing units
people_per_room.fn <- function(state,year,key){

  # get the list of data series
  series.df <- data.frame(series=c('B25014_005E','B25014_006E','B25014_007E','B25014_011E',
                                   'B25014_012E','B25014_013E'),
                          label=c('oo_1_1.5','oo_1.5_2','oo_2','ro_1_1.5',
                                  'ro_1.5_2','ro_2'))
  
  people_per_room <- list()
  for(i in 1:nrow(series.df)){
    call <- api_call.fn(year=year,state=state,key=key,series=series.df$series[i])

    df <- fromJSON(call)
    df <- data.frame(rbindlist(lapply(df,function(x){
      x<-unlist(x)
      return(data.frame(name=x[1],value=x[2],state=x[3],county=x[4],tract=x[5],data_series=series.df$label[i]))
    })
    )) %>% filter(row_number() > 1)
    
    people_per_room[[i]] <- df    
  }  
  
  people_per_room <-tbl_df(data.frame(rbindlist(people_per_room)))  %>%
    group_by(name,state,county,tract) %>%
    summarise(gt_1_per_room=sum(as.numeric(as.character(value)),na.rm=T))
  
  return(people_per_room)
  
}



###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


#-------------------------------------------------------------------------------------------------
# fraction of households with no vehicle avaiable

#B08201_002E - total households with no vehicle available

no_vehicles.fn <- function(state,year,key){

# B08201_002E
  call <- api_call.fn(year=year,state=state,key=key,series='B08201_002E')
  

  no_vehicle <- fromJSON(call)
  no_vehicle <- data.frame(rbindlist(lapply(no_vehicle,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],no_vehicle=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() >1)
  
  return(no_vehicle)    
  
}  

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################



#-------------------------------------------------------------------------------------------------
# percent of population living in group quarters
#B09019_038E - total in group quarters

group_quarters.fn <- function(state,year,key){
# 'B09019_038E'
  call <- api_call.fn(year=year,state=state,key=key,series='B09019_038E')

  group_quarters <- fromJSON(call)
  group_quarters <- data.frame(rbindlist(lapply(group_quarters,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],group_pop=x[2],state=x[3],county=x[4],tract=x[5]))
  })
  )) %>% filter(row_number() >1)
  
  return(group_quarters)    
  
}  


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

#Wrapper function to pull all the SVI data together

svi_data_censustract.fn <- function(yr,key){
  
  #------------------------------------------------------------
  #Basic Counts:
  
  # 1. total households by census tract
  
  total_households <- rbind(total_households.fn(state='06',year=yr,key=key),
                            total_households.fn(state='41',year=yr,key=key),
                            total_households.fn(state='53',year=yr,key=key))
  
  
  # 2. total population
  pop <- rbind(total_pop.fn(year=yr,state='06',key=key),
               total_pop.fn(year=yr,state='41',key=key),
               total_pop.fn(year=yr,state='53',key=key))

  
  # 3. population by age and sex
  
  pop.age.male <- rbind(pop_age_male.fn(year=yr,state='06',key=key),
                        pop_age_male.fn(year=yr,state='41',key=key),
                        pop_age_male.fn(year=yr,state='53',key=key))
  
  pop.age.female <- rbind(pop_age_female.fn(year=yr,state='06',key=key),
                          pop_age_female.fn(year=yr,state='41',key=key),
                          pop_age_female.fn(year=yr,state='53',key=key))
  #--------------------------------------------------------------
  
  #--------------------------------------------------------------
  # Socio-economic series
  
  # Socio-Economic Metrics
  
  # 1. population below the poverty line
  # 2. unemployment age 16 and up in the labor force
  # 3. per capital income
  # 4. age 25 and up w/o high school diploma
  # 5. age 65 or less w/o health insurance
  
  
  
  #------------------------------------------------------------------------------
  # households below 100% of the poverty line
  #'S0501_C01_104E'
  
  poverty <- tbl_df(rbind(below_poverty.fn(year=yr,state='06',key=key),
                   below_poverty.fn(year=yr,state='41',key=key),
                   below_poverty.fn(year=yr,state='53',key=key))) %>%
              select(name,state,county,tract,pov.pct)
  
  #--------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------
  # unemployment age 16 and up looking for work
  
  unemp <- tbl_df(rbind(unemp.function(year=yr,state='06',key=key),
                        unemp.function(year=yr,state='41',key=key),
                        unemp.function(year=yr,state='53',key=key))) %>%
            select(name,state,county,tract,unemp.rate)
  
  
  #---------------------------------------------------------------------------------
  
  
  #--------------------------------------------------------------------------------
  # education - population 25 and over with less than a high school degree
  # S0502_C01_047E - estimated population 25 and over
  # S0502_C01_048E - population 25 and over with less than HS degree
  
  edu <- tbl_df(rbind(edu.fn(state='06',year=yr,key=key),
                      edu.fn(state='41',year=yr,key=key),
                      edu.fn(state='53',year=yr,key=key)))
  
  
  #--------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------
  # per capital income
  
  pci <- rbind(pci.fn(state='06',year=yr,key=key),
               pci.fn(state='41',year=yr,key=key),
               pci.fn(state='53',year=yr,key=key))
  #--------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # health insurance
  
  #only available for 2012 onward
  
  #total population under 65 w/o health insurance
  
  ins <- rbind(health_ins.fn(state='06',year=yr,key=key),
               health_ins.fn(state='41',year=yr,key=key),
               health_ins.fn(state='53',year=yr,key=key))
  
  # now normalize by population 65 and under
  p <- tbl_df(rbind(pop.age.male,pop.age.female)) %>%
    filter(!series %in% c('M65_66','M67_69','M70_74','M75_79','M80_84','M85',
                          'F65_66','F67_69','F70_74','F75_79','F80_84','F85')) %>%
    group_by(name,state,county,tract) %>%
    summarise(pop_under_65=sum(as.numeric(as.character(value)),na.rm=T))
  
  ins <- ins %>% inner_join(p,by=c('name','state','county','tract')) %>%
    mutate(pct_no_healthins=no_health_insurance/pop_under_65)
  
  #--------------------------------------------------------------
  
  # Population metrics
  # population less than 18
  # population age 65 and older
  # age 5 or more with disability
  # % of single parent households
  # hispanic or non-white race
  # age 5 and older speaking english less than well
  
  #----------------------------------------------------------------------------------------------------
  #population under 18 and population over 65
  
  
  pop_under_18 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
    filter(series %in% c('M_5','M5_9','M10_14','M15_17','M18_19',
                         'F_5','F5_9','F10_14','F15_17','F18_19')) %>%
    group_by(name,state,county,tract) %>%
    summarise(pop_under_18=sum(as.numeric(as.character(value))))
  
  pop_over_65 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
    filter(series %in% c('M_65_66','M67_69','M70_74','M75_79','M80_84','M85',
                         'F_65_66','F67_69','F70_74','F75_79','F80_84','F85')) %>%
    group_by(name,state,county,tract) %>%
    summarise(pop_over_65=sum(as.numeric(as.character(value))))
  
  
  #---------------------------------------------------------------------------------------------------
  
  
  #---------------------------------------------------------------------------------------------------
  # Age 65 or less with a disability
  
  disable <- rbind(disability.fn(state='06',year=yr,key=key),
                   disability.fn(state='41',year=yr,key=key),
                   disability.fn(state='53',year=yr,key=key))
  
  #merge with population figure to get % 
  pop_under_65 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
    filter(!series %in% c('M65_66','M67_69','M70_74','M75_79','M80_85','M85',
                          'F65_66','F67_69','F70_74','F75_79','F80_85','F85')) %>%
    group_by(name,state,county,tract) %>%
    summarise(pop_under_65=sum(as.numeric(as.character(value))))
  
  disable <- disable %>% inner_join(pop_under_65,by=c('name','state','county','tract')) %>%
    mutate(disable_pct=disability/pop_under_65) %>%
    select(name,state,county,tract,disable_pct)
  
  
  
  #---------------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------------------------
  # single parent
  
  # not sure which exact series people generally use for this but I'm going to use:
  # B11001_006E - estimate!total!family households!other family!female householder, no husband present
  
  single.mom <- rbind(single_mom.fn(state='06',year=yr,key=key),
                      single_mom.fn(state='41',year=yr,key=key),
                      single_mom.fn(state='53',year=yr,key=key))
  
  
  # merge with total households to normalize
  single.mom <- single.mom %>% inner_join(total_households,by=c('name','state','county','tract')) %>%
    mutate(single_mom_pct = as.numeric(as.character(single.mom))/as.numeric(as.character(total_households))) %>%
     select(name,state,county,tract,single_mom_pct)
  
  #---------------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------------------------
  #Minority (hispanic and non-white race)
  
  white_alone <- rbind(total_white.fn(state='06',year=yr,key=key),
                       total_white.fn(state='41',year=yr,key=key),
                       total_white.fn(state='53',year=yr,key=key))
  
  white_alone <- white_alone %>% inner_join(pop,by=c('name','state','county','tract')) %>%
    mutate(pct.nonwhite=1-(as.numeric(as.character(white_alone))/as.numeric(as.character(total_pop)))) %>%
     select(name,state,county,tract,pct.nonwhite)
  
  
  #---------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------------
  # speaks english less than well
  
  
  limited_eng_hh <- rbind(limited_english.fn(state='06',year=yr,key=key),
                          limited_english.fn(state='41',year=yr,key=key),
                          limited_english.fn(state='53',year=yr,key=key)) %>%
    inner_join(total_households,by=c('name','state','county','tract')) %>%
    mutate(pct_limited_eng=as.numeric(as.character(limited_eng_hh))/as.numeric(as.character(total_households))) %>%
     select(name,state,county,tract,pct_limited_eng)
  
  
  
  #--------------------------------------------------------------------------------------------------
  
  
  total_housing <- rbind(total_hu.fn(state='06',year=yr,key=key),
                         total_hu.fn(state='41',year=yr,key=key),
                         total_hu.fn(state='53',year=yr,key=key))
  
  #-------------------------------------------------------------------------------------------------
  # housing units with 10 or more units per bldg
  
  #B25032_008E - total owner occupied housing units with 10-19 units in structure
  #B25032_009E - total owner occupied housing units with 20-49 units in structure
  #B25032_010E - total owner occupied housing units with 50 or more units in structure
  
  #B25032_019E - total renter occupied housing units with 10 - 19 units in structure
  #B25032_020E - total renter occupied housing units with 20 - 49 units in structure
  #B25032_021E - total renter occupied housing units with 50 or more units in structure
  
  
  units_per_structure <- rbind(housing_structures.fn(state='06',year=yr,key=key),
                               housing_structures.fn(state='41',year=yr,key=key),
                               housing_structures.fn(state='53',year=yr,key=key)) %>%
    inner_join(total_housing,by=c('name','state','county','tract')) %>%
    mutate(housing_gt_10_pct=as.numeric(as.character(more_than_10_units))/as.numeric(as.character(total_housing_units)))  %>%
    select(name,state,county,tract,housing_gt_10_pct)
  
  #-------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------------
  #Mobile homes as a percent of the housing stock
  
  # B25032_022E - renter occupied mobile homes
  # B25032_011E - owner occupied mobile homes
  
  mobile <- tbl_df(rbind(mobile_homes.fn(state='06',year=yr,key=key),
                         mobile_homes.fn(state='41',year=yr,key=key),
                         mobile_homes.fn(state='53',year=yr,key=key))) %>%
    inner_join(total_housing,by=c('name','state','county','tract')) %>%
    mutate(pct=as.numeric(as.character(total_mobile))/as.numeric(as.character(total_housing_units))) %>%
    select(name,state,county,tract,pct)
  
  names(mobile) <- c('name','state','county','tract','mobile_pct')
  
  #-------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------------
  #Occupants per room
  
  # summary table:
  # B25014_005E - owner occupied 1.01 - 1.5 occupants per room
  # B25014_006E - owner occupied 1.51 - 2 occupants per room
  # B25014_007E - owner occupied 2.01 or more occupants per room
  
  # B25014_011E - renter occupied 1.01 - 1.5 occupants per room
  # B25014_012E - renter occupied 1.51 - 2 occupants per room
  # B25014_013E - renter occupied 2.01 or more occupants per room
  
  # B25001# - total housing units
  
  
  people_per_room <- rbind(people_per_room.fn(state='06',year=yr,key=key),
                           people_per_room.fn(state='41',year=yr,key=key),
                           people_per_room.fn(state='53',year=yr,key=key)) %>%
    inner_join(total_housing,by=c('name','state','county','tract')) %>%
    mutate(pct_crowded=as.numeric(as.character(gt_1_per_room))/as.numeric(as.character(total_housing_units))) %>%
    select(name,state,county,tract,pct_crowded)
  
  
  #-------------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------------
  # fraction of households with no vehicle avaiable
  
  #B08201_002E - total households with no vehicle available
  
  no_vehicle <- rbind(no_vehicles.fn(state='06',year=yr,key=key),
                      no_vehicles.fn(state='41',year=yr,key=key),
                      no_vehicles.fn(state='53',year=yr,key=key)) %>%
    inner_join(total_housing,by=c('name','state','county','tract')) %>%
    mutate(pct_no_vehicle=as.numeric(as.character(no_vehicle))/as.numeric(as.character(total_housing_units))) %>%
    select(name,state,county,tract,pct_no_vehicle)  
  
  
  
  #-------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------------
  # percent of population living in group quarters
  
  pct_group <- rbind(group_quarters.fn(state='06',year=yr,key=key),
                     group_quarters.fn(state='41',year=yr,key=key),
                     group_quarters.fn(state='53',year=yr,key=key)) %>%
    inner_join(pop,by=c('name','state','county','tract')) %>%
    mutate(pct_group=as.numeric(as.character(group_pop))/as.numeric(as.character(total_pop))) %>%
    select(name,state,county,tract,pct_group)  
  
  
  
  #-------------------------------------------------------------------------------------------------
  
  
  
svi.df <- total_households %>% 
          inner_join(pop,by=c('name','state','county','tract')) %>%
          inner_join(poverty,by=c('name','state','county','tract')) %>%
          inner_join(unemp,by=c('name','state','county','tract')) %>%
          inner_join(edu,by=c('name','state','county','tract')) %>%
          inner_join(pci,by=c('name','state','county','tract')) %>%
          inner_join(ins,by=c('name','state','county','tract')) %>%
          inner_join(pop_under_18,by=c('name','state','county','tract')) %>%
          inner_join(pop_over_65,by=c('name','state','county','tract')) %>%
    inner_join(disable,by=c('name','state','county','tract')) %>%
    inner_join(single.mom,by=c('name','state','county','tract')) %>%
    inner_join(white_alone,by=c('name','state','county','tract')) %>%
    inner_join(limited_eng_hh,by=c('name','state','county','tract')) %>%
    inner_join(units_per_structure,by=c('name','state','county','tract')) %>%
    inner_join(mobile,by=c('name','state','county','tract')) %>%
    inner_join(people_per_room,by=c('name','state','county','tract')) %>%
    inner_join(no_vehicle,by=c('name','state','county','tract')) %>%
    inner_join(pct_group,by=c('name','state','county','tract'))
    
return(svi.df)
}

