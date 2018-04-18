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

key <- 'f5a32f694a14b28acf7301f4972eaab8551eafda'


#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

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
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

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
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
# now call the get.data function for each of the series needed to construct
# our metrics

pop <- rbind(data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='06'),
             data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='53'),
             data.function(year=2015,series='B01001_001E',series.name='total_population',
                           key=key,state='41')
             )

#total households - 'B11001_001E'
house.ca <- data.function(year=2015,series='B11001_001E',series.name='total_households',
                        key=key,state='06')

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

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



#-----------------------------------------------------------------------------------------

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

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



#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
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

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

#now at this point we have all the individual data series compiled in a 
# long form data frame.  we need to massage that data frame a little to 
# create the individual series

#total population
#total households

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

unemp <- tbl_df(unemp) %>% inner_join(lf,by=c('name','state'))


#-----------------------------------------------------------
