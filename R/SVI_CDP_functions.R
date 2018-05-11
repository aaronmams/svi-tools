library(RJSONIO)

library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)
library(dplyr)
library(data.table)

############################################################################
############################################################################
############################################################################
# This script compiles the data for social vulnerability indicators for 
# Census Designated Places for CA, OR, and WA

# The script functions in 3 stages:

#1. get the individual data series
#2. organize and aggregate the individual data series in a long form data frame of 
#      social vulnerability metrics
#3. bind all the metrics together into a single data frame
############################################################################
############################################################################
############################################################################

############################################################################
############################################################################
############################################################################
############################################################################

# First I establish some functions that allow us to construct the 16 separate
# pieces of the SVI for all census tracts.

# Each function has 2 inputs:

# 1. A state
# 2. a year
# 3. an API key

key <- 'f5a32f694a14b28acf7301f4972eaab8551eafda'

#=======================================================================
# this function takes a year, series, state, and API key and formats a
# url for the api call to get the chosen data series
api_call.fn <- function(year,series,key,state){
  
  
  if(year %in% c(2009,2010)){
    call <- paste('https://api.census.gov/data/',year,'/acs5?key=',key,'&get=',series,
                  ',NAME&for=place:*&in=state:',state,
                  sep="")
  }else if(year %in% c(2011,2012,2013,2014)){
    
    call <- paste('https://api.census.gov/data/',year,'/acs5?get=NAME,',series,
                  '&for=place:*&in=state:',
                  state,'&key=',
                  key,sep="")
    
  }else{

    call <- paste('https://api.census.gov/data/',year,'/acs/acs5?get=NAME,',series,
                  '&for=place:*&in=state:',
                    state,'&key=',
                  key,sep="")
    
  }
  
  return(call)  
}
#==================================================================

#=================================================================
# this function uses the api_call.fn() to get the actual data 

data.function <- function(year,series,series.name,key,state){
  call <- api_call.fn(year=year,key=key,series=series,state=state)
  
  df <- fromJSON(call)
  df <- data.frame(rbindlist(lapply(df,function(x){
    x<-unlist(x)
    return(data.frame(name=x[1],value=x[2],state=x[3],data_series=series,data_name=series.name))
  })
  )) %>% filter(row_number() > 1)
  return(df)
}
#================================================================
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
# THIS IS THE BEGINNING OF STEP 1: GETTING THE INDIVIDUAL DATA SERIES

#=====================================================================================
#total population
pop <- rbind(data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='06'),
             data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='53'),
             data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='41')
             )
#========================================================================================

#========================================================================================
#total households - 'B11001_001E'
households <- rbind(data.function(year=2015,series='B11001_001E',series.name='total_households',
                        key=key,state='06'),
                  data.function(year=2015,series='B11001_001E',series.name='total_households',
                                key=key,state='41'),
                  data.function(year=2015,series='B11001_001E',series.name='total_households',
                                key=key,state='53'))
#================================================================================

#=================================================================================
#total housing units

# B25001_001E - total housing units
housing.units <- rbind(data.function(year=2015,series='B25001_001E',series.name='total_housing_units',
                                     key=key, state='06'),
                       data.function(year=2015,series='B25001_001E',series.name='total_housing_units',
                                     key=key, state='41'),
                       data.function(year=2015,series='B25001_001E',series.name='total_housing_units',
                                     key=key, state='53'))
#=================================================================================

#================================================================================
#population by age and sex

#------------------------------------------------------------------------------------
#males
series.df <- data.frame(series=c('B01001_003E','B01001_004E','B01001_005E','B01001_006E','B01001_007E','B01001_008E',
                                 'B01001_009E','B01001_010E','B01001_011E','B01001_012E','B01001_013E',
                                 'B01001_014E','B01001_015E','B01001_016E','B01001_017E','B01001_018E','B01001_019E',
                                 'B01001_020E','B01001_021E','B01001_022E','B01001_023E','B01001_024E','B01001_025E'),
                        label=c('M_5','M5_9','M10_14','M15_17','M18_19','M20','M21','M22_24','M25_29',
                                'M30_34','M35_39','M40_44','M45_49','M50_54','M55_59','M60_61','M62_64',
                                'M65_66','M67_69','M70_74','M75_79','M80_84','M85'))
series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

pop.male.ca <- list()
for(i in 1:nrow(series.df)){
pop.male.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                key=key,state='06')
Sys.sleep(t=20)
}
pop.male.or <- list()
for(i in 1:nrow(series.df)){
  pop.male.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                    key=key,state='41')
  Sys.sleep(t=20)
}
pop.male.wa <- list()
for(i in 1:nrow(series.df)){
  pop.male.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                    key=key,state='53')
  Sys.sleep(t=20)
}

pop.male <- rbind(data.frame(rbindlist(pop.male.ca)),
                  data.frame(rbindlist(pop.male.or)),
                  data.frame(rbindlist(pop.male.wa)))

#remove large list elements from the workspace
rm(list=c('pop.male.ca','pop.male.or','pop.male.wa'))
#------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# females
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

pop.female.ca <- list()
for(i in 1:nrow(series.df)){
  pop.female.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                    key=key,state='06')
  Sys.sleep(time = 20)
}
pop.female.or <- list()
for(i in 1:nrow(series.df)){
  pop.female.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                        key=key,state='41')
  Sys.sleep(time = 20)
  }
pop.female.wa <- list()
for(i in 1:nrow(series.df)){
  pop.female.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                    key=key,state='53')
  Sys.sleep(time = 20)
  }

pop.female <- rbind(data.frame(rbindlist(pop.female.ca)),
                    data.frame(rbindlist(pop.female.or)),
                    data.frame(rbindlist(pop.female.wa)))

#remove large lists from workspace
rm(list=c('pop.female.ca','pop.female.or','pop.female.wa'))
#=================================================================================



#===================================================================================
# A function to get percent of households below the poverty line from the ACS 5 year
# estimates for a particular year

#------------------------------------------------------------------------------
# population for which poverty status is determined B17001_001E
# population with income below the poverty line in the last 12 months B17001_002E

status <- rbind(data.function(year=2015,series='B17001_001E',series.name='pop.pov.status',
              key=key,state='06'),
              data.function(year=2015,series='B17001_001E',series.name='pop.pov.status',
                            key=key,state='41'),
              data.function(year=2015,series='B17001_001E',series.name='pop.pov.status',
                            key=key,state='53'))
Sys.sleep(t=20)
pov <- rbind(data.function(year=2015,series='B17001_002E',series.name='pop.pov',
                     key=key,state='06'),
             data.function(year=2015,series='B17001_002E',series.name='pop.pov',
                           key=key,state='41'),
             data.function(year=2015,series='B17001_002E',series.name='pop.pov',
                           key=key,state='53'))

pov <- tbl_df(pov) %>% left_join(status,by=c('name','state')) %>%
         mutate(value=as.numeric(as.character(value.x))/as.numeric(as.character(value.y))) %>%
         select(name,state,value) %>%
         mutate(data_name='pov_pct')
#=================================================================================


#=================================================================================
# Unemployment
series.df <- data.frame(series=c(
  'B23001_008E','B23001_015E','B23001_022E','B23001_029E','B23001_036E','B23001_043E','B23001_050E','B23001_057E',
  'B23001_064E','B23001_071E','B23001_094E','B23001_101E','B23001_108E','B23001_115E','B23001_122E','B23001_129E',
  'B23001_136E','B23001_143E','B23001_150E','B23001_157E'),
  labels=c('m16_19','m20_21','m22_24','m25_29','m30_34','m35_44','m45_54','m55_59','m60_61','m62_64',
           'f16_19','f20_21','f22_24','f25_29','f30_34','f35_44','f45_54','f55_59','f60_61','f62_64'))

series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

unemp.ca <- list()
for(i in 1:nrow(series.df)){
  unemp.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                      key=key,state='06')
  Sys.sleep(time = 20)
}
unemp.or <- list()
for(i in 1:nrow(series.df)){
  unemp.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                      key=key,state='41')
  Sys.sleep(time = 20)
}
unemp.wa <- list()
for(i in 1:nrow(series.df)){
  unemp.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                      key=key,state='53')
  Sys.sleep(time = 20)
}

unemp <- tbl_df(rbind(data.frame(rbindlist(unemp.ca)),
                    data.frame(rbindlist(unemp.or)),
                    data.frame(rbindlist(unemp.wa))))

rm(list=c('unemp.ca','unemp.or','unemp.wa'))


series.df <- data.frame(series=c(
  'B23001_004E','B23001_011E','B23001_018E','B23001_025E','B23001_032E','B23001_039E','B23001_046E','B23001_053E',
  'B23001_060E','B23001_067E','B23001_090E','B23001_097E','B23001_104E','B23001_111E','B23001_118E','B23001_125E',
  'B23001_132E','B23001_139E','B23001_146E','B23001_153E'),
  labels=c('m16_19','m20_21','m22_24','m25_29','m30_34','m35_44','m45_54','m55_59','m60_61','m62_64',
           'f16_19','f20_21','f22_24','f25_29','f30_34','f35_44','f45_54','f55_59','f60_61','f62_64'))

series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

lf.ca <- list()
for(i in 1:nrow(series.df)){
  lf.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                 key=key,state='06')
  Sys.sleep(time = 20)
}
lf.or <- list()
for(i in 1:nrow(series.df)){
  lf.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                 key=key,state='41')
  Sys.sleep(time = 20)
}
lf.wa <- list()
for(i in 1:nrow(series.df)){
  lf.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                 key=key,state='53')
  Sys.sleep(time = 20)
}

lf <- tbl_df(rbind(data.frame(rbindlist(lf.ca)),
                      data.frame(rbindlist(lf.or)),
                      data.frame(rbindlist(lf.wa))))

rm(list=c('lf.ca','lf.or','lf.wa'))
#=================================================================================

#=================================================================================
# EDUCATION - POPULATION PCT 25 AND OVER W/LESS THAN A HIGH SCHOOL DIPLOMA

series.df <- data.frame(series=c(
  'B15001_013E','B15001_021E',
  'B15001_029E','B15001_037E',
  'B15001_054E','B15001_062E',
  'B15001_070E','B15001_078E'),
  labels=c('m_25_34_nodiploma','m_35_44_nodiploma',
           'm_45_64_nodiploma','m_65_plus_nodiploma',
           'f_25_34_nodiploma','f_35_44_nodiploma',
           'f_45_64_nodiploma','f_65_plus_nodiploma'))

series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

edu.ca <- list()
for(i in 1:nrow(series.df)){
  edu.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                              key=key,state='06')
  Sys.sleep(time = 20)
}
edu.or <- list()
for(i in 1:nrow(series.df)){
  edu.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                              key=key,state='41')
  Sys.sleep(time = 20)
}
edu.wa <- list()
for(i in 1:nrow(series.df)){
  edu.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                              key=key,state='53')
  Sys.sleep(time = 20)
}

edu <- tbl_df(rbind(data.frame(rbindlist(edu.ca)),
                   data.frame(rbindlist(edu.or)),
                   data.frame(rbindlist(edu.wa))))

rm(list=c('edu.ca','edu.or','edu.wa'))


series.df <- data.frame(series=c('B15001_011E','B15001_019E','B15001_027E','B15001_035E',
                                 'B15001_052E','B15001_060E','B15001_068E','B15001_076E'),
                        labels=c('m_25_34','m_35_44','m_45_64','m_65_plus',
                                 'f_25_34','f_35_44','f_45_64','f_65_plus'))
series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

popcount.ca <- list()
for(i in 1:nrow(series.df)){
  popcount.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='06')
  Sys.sleep(time = 20)
}
popcount.or <- list()
for(i in 1:nrow(series.df)){
  popcount.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='41')
  Sys.sleep(time = 20)
}
popcount.wa <- list()
for(i in 1:nrow(series.df)){
  popcount.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='53')
  Sys.sleep(time = 20)
}

popcount <- tbl_df(rbind(data.frame(rbindlist(popcount.ca)),
                    data.frame(rbindlist(popcount.or)),
                    data.frame(rbindlist(popcount.wa))))

rm(list=c('popcount.ca','popcount.or','popcount.wa'))
#==========================================================================


#===========================================================================
#PER CAPITA INCOME

pci <- rbind(data.function(year=2015,series='B19301_001E',series.name='pci',
                           key=key,state='06'),
             data.function(year=2015,series='B19301_001E',series.name='pci',
                           key=key,state='41'),
             data.function(year=2015,series='B19301_001E',series.name='pci',
                           key=key,state='53'))
#============================================================================  
  

#============================================================================
#Health Insurance - pct of population with no health insurance

# B27001_
# 005E, 008E, 011E, 014E, 017E, 020E, 023E -males without health insurance
# 033E, 036E, 039E, 042E, 045E, 048E, 051E -females without health insurance

series.df <- data.frame(series=c(
  'B27001_005E','B27001_008E','B27001_011E','B27001_014E','B27001_017E','B27001_020E','B27001_023E',
  'B27001_033E','B27001_036E','B27001_039E','B27001_042E','B27001_045E','B27001_048E','B27001_051E'),
  labels=c('m6','m6_17','m18_24','m25_34','m35_44','m45-54','m55_64',
           'f6','f6_17','f18_24','f25_34','f35_44','f45_54','f55_64'))

series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

ins.ca <- list()
for(i in 1:nrow(series.df)){
  ins.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='06')
  Sys.sleep(time = 20)
}
ins.or <- list()
for(i in 1:nrow(series.df)){
  ins.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='41')
  Sys.sleep(time = 20)
}
ins.wa <- list()
for(i in 1:nrow(series.df)){
  ins.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='53')
  Sys.sleep(time = 20)
}

ins <- tbl_df(rbind(data.frame(rbindlist(ins.ca)),
                         data.frame(rbindlist(ins.or)),
                         data.frame(rbindlist(ins.wa))))

rm(list=c('ins.ca','ins.or','ins.wa'))

series.df <- data.frame(series=c(
  'B27001_003E','B27001_006E','B27001_009E','B27001_012E','B27001_015E','B27001_018E','B27001_021E',
  'B27001_031E','B27001_034E','B27001_037E','B27001_040E','B27001_043E','B27001_046E','B27001_049E'),
  labels=c('m6','m6_17','m18_24','m25_34','m35_44','m45-54','m55_64',
           'f6','f6_17','f18_24','f25_34','f35_44','f45_54','f55_64'))

series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label)

ins.pop.ca <- list()
for(i in 1:nrow(series.df)){
  ins.pop.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='06')
  Sys.sleep(time = 20)
}
ins.pop.or <- list()
for(i in 1:nrow(series.df)){
  ins.pop.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='41')
  Sys.sleep(time = 20)
}
ins.pop.wa <- list()
for(i in 1:nrow(series.df)){
  ins.pop.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                               key=key,state='53')
  Sys.sleep(time = 20)
}

ins.pop <- tbl_df(rbind(data.frame(rbindlist(ins.pop.ca)),
                    data.frame(rbindlist(ins.pop.or)),
                    data.frame(rbindlist(ins.pop.wa))))

rm(list=c('ins.pop.ca','ins.pop.or','ins.pop.wa'))

#===========================================================================

#===========================================================================
#Disability - age 65 and under with a disability
  
  # get the list of data series
  series.df <- data.frame(series=c('B18101_004E','B18101_007E','B18101_010E','B18101_013E',
                                   'B18101_023E','B18101_026E','B18101_029E','B18101_032E'),
                          label=c('dis_m_5','dis_m_5_17','dis_m_18_34','dis_m_35_64',
                                  'dis_f_5','dis_f_5_17','dis_f_18_34','dis_f_35_64'))
series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label) 

dis.pop.ca <- list()
for(i in 1:nrow(series.df)){
  dis.pop.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}
 
dis.pop.or <- list()
for(i in 1:nrow(series.df)){
  dis.pop.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}

dis.pop.wa <- list()
for(i in 1:nrow(series.df)){
  dis.pop.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}

dis.pop <- rbind(data.frame(rbindlist(dis.pop.ca)),
                 data.frame(rbindlist(dis.pop.or)),
                 data.frame(rbindlist(dis.pop.wa)))

rm(list=c('dis.pop.ca','dis.pop.or','dis.pop.wa'))


#---------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
## Single parent household, female head of household, no spouse present, as a 
# % of households

# not sure which exact series people generally use for this but I'm going to use:
  # B11001_006E - estimate!total!family households!other family!female householder, no husband present
  
single.ca <- data.function(state='06',series='B11001_006E', key=key, year=2015,
                           series.name='single_mom_household')
single.or <- data.function(state='41',series='B11001_006E', key=key, year=2015,
                           series.name='single_mom_household')
single.wa <- data.function(state='53',series='B11001_006E', key=key, year=2015,
                           series.name='single_mom_household')

single.mom.hh <- data.frame(rbind(single.ca,single.or,single.wa))

#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# Total white population

# B01001A_001E - total white population

white.pop <- rbind(data.function(state='06',series='B01001A_001E',key=key,year=2015,
                           series.name='total_pop_white'),
            data.function(state='41',series='B01001A_001E',key=key,year=2015,
              series.name='total_pop_white'),
            data.function(state='53',series='B01001A_001E',key=key,year=2015,
              series.name='total_pop_white'))
#------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# limited english households
#B16002_004E, _007E, _010E, _013E
# get the list of data series
series.df <- data.frame(series=c('B16002_004E','B16002_007E','B16002_010E','B16002_013E'),
                        label=c('limited_eng_span','limited_eng_IE','limited_english_AP',
                                'limited_english_oth'))
series.df$series <- as.character(series.df$series)
series.df$label <- as.character(series.df$label) 

le.ca <- list()
for(i in 1:nrow(series.df)){
  le.ca[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}

le.or <- list()
for(i in 1:nrow(series.df)){
  le.or[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}

le.wa <- list()
for(i in 1:nrow(series.df)){
  le.wa[[i]] <- data.function(year=2015,series=series.df$series[i],series.name=series.df$label[i],
                                   key=key,state='06')
  Sys.sleep(time = 20)
}

le.pop <- rbind(data.frame(rbindlist(le.ca)),
                 data.frame(rbindlist(le.or)),
                 data.frame(rbindlist(le.wa)))

rm(list=c('le.ca','le.or','le.wa'))


#-----------------------------------------------------------------------------------
#==========================================================================

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
# THIS IS THE BEGINNING OF STEP 2: FORMATTING THE INDIVIDUAL DATA FRAMES AND 
# AGGREGATING INDIVIDUAL DATA SERIES TO FORM METRICS

#now at this point we have all the individual data series compiled in a 
# long form data frame.  we need to massage that data frame a little to 
# create the individual series

#total population
# just drop the data series field
pop <- pop %>% select(name,state,value,data_name)

#total households
households <- households %>% select(name,state,value,data_name)

# pct of population 17 and under
pop.U17 <- tbl_df(rbind(pop.female,pop.male)) %>%
                filter(data_name %in% c('F_5','F5_9','F10_14','F15_17',
                                        'M_5','M5_9','M10_14','M15_17')) %>%
                group_by(name,state) %>%
                summarise(value=sum(as.numeric(as.character(value)),na.rm=T)) %>%
                mutate(data_name='population_under_17') %>%
                inner_join(pop,by=c('name','state')) %>%
                mutate(pct_under_17=value.x/as.numeric(as.character(value.y))) %>%
                select(name,state,pct_under_17) %>%
                rename(value=pct_under_17) %>% 
                mutate(data_name='pct_under_17')

# pct of population 65 and over
pop.65 <- tbl_df(rbind(pop.female,pop.male)) %>%
  filter(data_name %in% c('F65_66','F67_69','F70_74','F75_79','F80_84','F85',
                          'M65_66','M67_69','M70_74','M75_79','M80_84','M85')) %>%
  group_by(name,state) %>%
  summarise(value=sum(as.numeric(as.character(value)),na.rm=T)) %>%
  mutate(data_name='population_over_65') %>%
  inner_join(pop,by=c('name','state')) %>%
  mutate(pct_over_65=value.x/as.numeric(as.character(value.y))) %>%
  select(name,state,pct_over_65) %>%
  rename(value=pct_over_65) %>% 
  mutate(data_name='pct_over_65')

#population percent below poverty already in long form


#---------------------------------------------------------
#unemployed population
# unemployment is by age and sex so we need to merge it with
# labor force population and then normalize

unemp <- tbl_df(unemp) %>% group_by(name,state) %>%
          summarise(unemp = sum(as.numeric(as.character(value))))

lf <- tbl_df(lf) %>% group_by(name,state) %>%
  summarise(lf = sum(as.numeric(as.character(value))))

unemp <- unemp %>% inner_join(lf,by=c('name','state')) %>%
            mutate(value=unemp/lf,data_name='urate') %>%
         select(name,state,value,data_name)
#-----------------------------------------------------------

#-----------------------------------------------------------
edu <- tbl_df(edu) %>% group_by(name,state) %>%
        summarise(nodiploma=sum(as.numeric(as.character(value))))

popcount <- tbl_df(popcount) %>% group_by(name,state) %>% 
             summarise(total=sum(as.numeric(as.character(value))))

edu <- edu %>% inner_join(popcount,by=c('name','state')) %>%
        group_by(name,state) %>%
          summarise(value=nodiploma/total,
                 data_name='nodiploma.pct') 
       
#-----------------------------------------------------------

#-----------------------------------------------------------
# per capita income is well formatted but we don't need the 
# series names

pci <- tbl_df(pci) %>% select(name,state,value,data_name) 
         
#-----------------------------------------------------------

#-----------------------------------------------------------
# no health insurance needs to be summarised then normalized
# by total population from 5 - 65


ins.pop <- ins.pop %>% group_by(name,state) %>%
             summarise(value=sum(as.numeric(as.character(value)))) 

ins <- ins %>% 
  group_by(name,state) %>%
  summarise(value=sum(as.numeric(as.character(value)))) %>%
  inner_join(ins.pop,by=c('name','state')) %>%
  mutate(value=value.x/as.numeric(as.character(value.y)),
         data_name='pct_no_healthins')

#-----------------------------------------------------------

#-----------------------------------------------------------
# population percent with a disability (age 65 and under)

dis.pop.agg <- dis.pop %>% group_by(name,state) %>%
                  summarise(value=sum(as.numeric(as.character(value)),na.rm=T)) 
                  
pop.u65 <-   rbind(pop.female,pop.male) %>%
              filter(!data_name %in% c('F5','F65_66','F67_69','F70_74','F75_79','F80_84','F85',
                                       'M5','M65_66','M67_69','M70_74','M75_79','M80_84','M85') ) %>%
              group_by(name,state) %>%
              summarise(tpop=sum(as.numeric(as.character(value))))
              
dis.pop.agg <- dis.pop.agg %>% left_join(pop.u65,by=c('name','state')) %>%
                  mutate(value=value/tpop,data_name='disability_pct')  %>%
                  select(name,state,value,data_name)


#single mother head of household
single.mom.hh <- single.mom.hh %>% inner_join(households,by=c('name','state')) %>%
                   mutate(value=as.numeric(as.character(value.x))/as.numeric(as.character(value.y))) %>%
                   mutate(data_name='singlemom_hh_pct') %>%
                   select(name,state,value,data_name)

#-----------------------------------------------------------

#-----------------------------------------------------------
# non-white as a pop pct

white.pop.pct <- white.pop %>% inner_join(pop,by=c('name','state')) %>%
                  mutate(value=1-(as.numeric(as.character(value.x))/as.numeric(as.character(value.y)))) %>%
                  select(name,state,value) %>%
                  mutate(data_name='pct_nonwhite')


#----------------------------------------------------------

#----------------------------------------------------------
#limited english households as a % of total households
le.pop.pct <- le.pop %>%  
               group_by(name,state) %>%
               summarise(value=sum(as.numeric(as.character(value)),na.rm=T)) %>%
                inner_join(households,by=c('name','state')) %>%
               mutate(value=as.numeric(as.character(value.x))/as.numeric(as.character(value.y))) %>%
               select(name,state,value) %>%
              mutate(data_name='le_hh_pct')


#----------------------------------------------------------
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
# THIS IS THE BEGINNING OF STEP 3: PUTTING ALL THE METRICS TOGETHER IN A SINGLE
# LONG FORM DATA FRAME.

# put everything together in long form and save to RDA
# R doesn't like rbinding grouped data frames so just ungroup everything beforehand
pop.U17 <- pop.U17 %>% ungroup()
pop.65 <- pop.65 %>% ungroup()
pop <- pop %>% ungroup()
pov <- pov %>% ungroup()
households <- households %>% ungroup()
unemp <- unemp %>% ungroup()
edu <- edu %>% ungroup()
pci <- pci %>% ungroup()
ins <- ins %>% ungroup() 
dis.pop.agg <- dis.pop.agg %>% ungroup()
single.mom.hh <- single.mom.hh %>% ungroup()   
white.pop.pct <- white.pop.pct %>% ungroup()
le.pop.pct <- le.pop.pct %>% ungroup()

df <- rbind(pop.U17,pop.65,pop,pov,households,unemp,edu,pci)
saveRDS(df,'data/svi_cdp.RDA')

