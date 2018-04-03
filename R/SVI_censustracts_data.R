
rm(list=ls())
library(RJSONIO)

library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)
library(dplyr)
library(data.table)

source('R/SVI_censustract_functions.R')
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
#This script downloads the SVI data series from ACS 5 year census for 
# census tracts in CA, WA, OR

############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

key <- 'f5a32f694a14b28acf7301f4972eaab8551eafda'

#call the svi census tract function to build the data
#svi.df <- svi_data_censustract.fn(yr=2015,key=key)
#saveRDS(svi.df,file='svi_censustract_2015.RDA')

# or just load the pre-compiled data
svi.df <- readRDS('data/svi_censustract_2015.RDA')







############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

############################################################################
############################################################################
############################################################################
############################################################################
# If you want to build individual metrics you can do it like this:

#' t <- Sys.time()
#' # there are a few basic series that we need in order to create percentages:
#' 
#' # 1. total households by census tract
#' 
#' total_households <- rbind(total_households.fn(state='06',year=2015,key=key),
#'                           total_households.fn(state='41',year=2015,key=key),
#'                           total_households.fn(state='53',year=2015,key=key))
#' 
#' 
#' # 2. total population
#' pop <- paste('https://api.census.gov/data/2015/acs/acs5?get=NAME,B01001_001E&for=tract:*&in=state:06&key=',
#'              key,sep="")
#' ljson<-fromJSON(pop)
#' 
#' pop <- tbl_df(data.frame(rbindlist(lapply(ljson,function(x){
#'   x <- unlist(x)
#'   return(data.frame(name=x[1],total_pop=x[2],state=x[3],county=x[4],tract=x[5]))
#' })))) %>% filter(row_number() >1)
#' 
#' 
#' # 3. population by age and sex
#' 
#' pop.age.male <- pop_age_male.fn(year=2015,state='06',key=key)
#' pop.age.female <- pop_age_female.fn(year=2015,state='06',key=key)
#' Sys.time() - t
#' 
#' 
#' 
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' 
#' 
#' 
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' 
#' 
#' # Socio-Economic Metrics
#' 
#' # 1. population below the poverty line
#' # 2. unemployment age 16 and up in the labor force
#' # 3. per capital income
#' # 4. age 25 and up w/o high school diploma
#' # 5. age 65 or less w/o health insurance
#' 
#' 
#' 
#' #------------------------------------------------------------------------------
#' # households below 100% of the poverty line
#' #'S0501_C01_104E'
#' 
#' poverty <- rbind(below_poverty.fn(year=2015,state='06',key=key),
#'                  below_poverty.fn(year=2015,state='41',key=key),
#'                  below_poverty.fn(year=2015,state='53',key=key))
#' 
#' #--------------------------------------------------------------------------------
#' 
#' #---------------------------------------------------------------------------------
#' # unemployment age 16 and up looking for work
#' 
#' unemp <- tbl_df(rbind(unemp.function(year=2015,state='06',key=key),
#'                unemp.function(year=2015,state='41',key=key),
#'                unemp.function(year=2015,state='53',key=key)))
#' 
#' 
#' #---------------------------------------------------------------------------------
#' 
#' 
#' #--------------------------------------------------------------------------------
#' # education - population 25 and over with less than a high school degree
#' # S0502_C01_047E - estimated population 25 and over
#' # S0502_C01_048E - population 25 and over with less than HS degree
#' 
#' edu <- tbl_df(rbind(edu.fn(state='06',year=2015,key=key),
#'              edu.fn(state='41',year=2015,key=key),
#'              edu.fn(state='53',year=2015,key=key)))
#' 
#' 
#' #--------------------------------------------------------------------------------------------
#' 
#' #--------------------------------------------------------------------------------------------
#' # per capital income
#' 
#' pci <- rbind(pci.fn(state='06',year=2015,key=key),
#'              pci.fn(state='41',year=2015,key=key),
#'              pci.fn(state='53',year=2015,key=key))
#' #--------------------------------------------------------------------------------------------
#' 
#' #-------------------------------------------------------------------------------------------
#' # health insurance
#' 
#' #only available for 2012 onward
#' 
#' #total population under 65 w/o health insurance
#' 
#' ins <- rbind(health_ins.fn(state='06',year=2015,key=key),
#'              health_ins.fn(state='41',year=2015,key=key),
#'              health_ins.fn(state='53',year=2015,key=key))
#' 
#' # now normalize by population 65 and under
#' p <- tbl_df(rbind(pop.age.male,pop.age.female)) %>%
#'        filter(!series %in% c('M65_66','M67_69','M70_74','M75_79','M80_84','M85',
#'                              'F65_66','F67_69','F70_74','F75_79','F80_84','F85')) %>%
#'        group_by(name,state,county,tract) %>%
#'        summarise(pop_under_65=sum(as.numeric(as.character(value)),na.rm=T))
#' 
#' ins <- ins %>% inner_join(p,by=c('name','state','county','tract')) %>%
#'           mutate(pct_no_healthins=no_health_insurance/pop_under_65)
#' 
#' 
#' 
#' 
#' 
#' #--------------------------------------------------------------------------------------------
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' ############################################################################
#' 
#' 
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' # Population metrics
#' # population less than 18
#' # population age 65 and older
#' # age 5 or more with disability
#' # % of single parent households
#' # hispanic or non-white race
#' # age 5 and older speaking english less than well
#' 
#' #----------------------------------------------------------------------------------------------------
#' #population under 18 and population over 65
#' 
#' 
#' pop_under_18 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
#'                    filter(series %in% c('M_5','M5_9','M10_14','M15_17','M18_19',
#'                                         'F_5','F5_9','F10_14','F15_17','F18_19')) %>%
#'                    group_by(name,state,county,tract) %>%
#'                    summarise(pop_under_18=sum(as.numeric(as.character(value))))
#' 
#' pop_over_65 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
#'   filter(series %in% c('M_65_66','M67_69','M70_74','M75_79','M80_84','M85',
#'                        'F_65_66','F67_69','F70_74','F75_79','F80_84','F85')) %>%
#'   group_by(name,state,county,tract) %>%
#'   summarise(pop_over_65=sum(as.numeric(as.character(value))))
#' 
#' 
#' #---------------------------------------------------------------------------------------------------
#' 
#' 
#' #---------------------------------------------------------------------------------------------------
#' # Age 65 or less with a disability
#' 
#' disable <- rbind(disability.fn(state='06',year=2015,key=key),
#'                  disability.fn(state='41',year=2015,key=key),
#'                  disability.fn(state='53',year=2015,key=key))
#' 
#' #merge with population figure to get % 
#' pop_under_65 <- tbl_df(rbind(pop.age.female,pop.age.male)) %>%
#'                   filter(!series %in% c('M65_66','M67_69','M70_74','M75_79','M80_85','M85',
#'                                         'F65_66','F67_69','F70_74','F75_79','F80_85','F85')) %>%
#'                   group_by(name,state,county,tract) %>%
#'                   summarise(pop_under_65=sum(as.numeric(as.character(value))))
#' 
#' disable <- disable %>% inner_join(pop_under_65,by=c('name','state','county','tract')) %>%
#'             mutate(diable_pct=disability/pop_under_65)
#' 
#' 
#' 
#' #---------------------------------------------------------------------------------------------------
#' 
#' #---------------------------------------------------------------------------------------------------
#' # single parent
#' 
#' # not sure which exact series people generally use for this but I'm going to use:
#' # B11001_006E - estimate!total!family households!other family!female householder, no husband present
#' 
#' single.mom <- rbind(single_mom.fn(state='06',year=2015,key=key),
#'                  single_mom.fn(state='41',year=2015,key=key),
#'                  single_mom.fn(state='53',year=2015,key=key))
#' 
#' 
#' # merge with total households to normalize
#' single.mom <- single.mom %>% inner_join(total_households,by=c('name','state','county','tract')) %>%
#'                  mutate(single_mom_pct = as.numeric(as.character(single.mom))/as.numeric(as.character(total_households)))
#' 
#' #---------------------------------------------------------------------------------------------------
#' 
#' #---------------------------------------------------------------------------------------------------
#' #Minority (hispanic and non-white race)
#' 
#' white_alone <- rbind(total_white.fn(state='06',year=2015,key=key),
#'                      total_white.fn(state='41',year=2015,key=key),
#'                      total_white.fn(state='53',year=2015,key=key))
#' 
#' total.pop <- rbind(pop.age.female,pop.age.male) %>%
#'               group_by(name,state,county,tract) %>%
#'               summarise(total_pop=sum(as.numeric(as.character(value))))
#' 
#' white_alone <- white_alone %>% inner_join(total.pop,by=c('name','state','county','tract')) %>%
#'                 mutate(pct.nonwhite=1-(as.numeric(as.character(white_alone))/total_pop))
#' 
#' 
#' #---------------------------------------------------------------------------------------------------
#' 
#' #--------------------------------------------------------------------------------------------------
#' # speaks english less than well
#' 
#' 
#' limited_eng_hh <- rbind(limited_english.fn(state='06',year=2015,key=key),
#'                         limited_english.fn(state='41',year=2015,key=key),
#'                         limited_english.fn(state='53',year=2015,key=key)) %>%
#'                  inner_join(total_households,by=c('name','state','county','tract')) %>%
#'                  mutate(pct_limited_eng=as.numeric(as.character(limited_eng_hh))/as.numeric(as.character(total_households)))
#' 
#' 
#' 
#' #--------------------------------------------------------------------------------------------------
#' 
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' # Now on to Housing and Tranport:
#' 
#' # 1. number of housing units with 10 or more units per bldg
#' # 2. number of mobile homes
#' # 3. number of housing units with more than 1 person per room
#' # 4. households with no vehicle available
#' # 5. population living in group quarters
#' 
#' #to simplify this a little we pull the total housing units first 
#' # because this is used for several calculations
#' 
#' total_housing <- rbind(total_hu.fn(state='06',year=2015,key=key),
#'                        total_hu.fn(state='41',year=2015,key=key),
#'                        total_hu.fn(state='53',year=2015,key=key))
#' 
#' 
#' 
#' #-------------------------------------------------------------------------------------------------
#' # housing units with 10 or more units per bldg
#' 
#' #B25032_008E - total owner occupied housing units with 10-19 units in structure
#' #B25032_009E - total owner occupied housing units with 20-49 units in structure
#' #B25032_010E - total owner occupied housing units with 50 or more units in structure
#' 
#' #B25032_019E - total renter occupied housing units with 10 - 19 units in structure
#' #B25032_020E - total renter occupied housing units with 20 - 49 units in structure
#' #B25032_021E - total renter occupied housing units with 50 or more units in structure
#' 
#' 
#' units_per_structure <- rbind(housing_structures.fn(state='06',year=2015,key=key),
#'                              housing_structures.fn(state='41',year=2015,key=key),
#'                             housing_structures.fn(state='53',year=2015,key=key)) %>%
#'                        inner_join(total_housing,by=c('name','state','county','tract')) %>%
#'   mutate(housing_gt_10_pct=as.numeric(as.character(more_than_10_units))/as.numeric(as.character(total_housing_units)))               
#'         
#' #-------------------------------------------------------------------------------------------------
#' 
#' #-------------------------------------------------------------------------------------------------
#' #Mobile homes as a percent of the housing stock
#' 
#' # B25032_022E - renter occupied mobile homes
#' # B25032_011E - owner occupied mobile homes
#' 
#' mobile <- tbl_df(rbind(mobile_homes.fn(state='06',year=2015,key=key),
#'                 mobile_homes.fn(state='41',year=2015,key=key),
#'                 mobile_homes.fn(state='53',year=2015,key=key))) %>%
#'          inner_join(total_housing,by=c('name','state','county','tract')) %>%
#'          mutate(pct=as.numeric(as.character(total_mobile))/as.numeric(as.character(total_housing_units)))
#' 
#' #-------------------------------------------------------------------------------------------------
#' 
#' #-------------------------------------------------------------------------------------------------
#' #Occupants per room
#' 
#' # summary table:
#' # B25014_005E - owner occupied 1.01 - 1.5 occupants per room
#' # B25014_006E - owner occupied 1.51 - 2 occupants per room
#' # B25014_007E - owner occupied 2.01 or more occupants per room
#' 
#' # B25014_011E - renter occupied 1.01 - 1.5 occupants per room
#' # B25014_012E - renter occupied 1.51 - 2 occupants per room
#' # B25014_013E - renter occupied 2.01 or more occupants per room
#' 
#' # B25001# - total housing units
#' 
#' 
#' people_per_room <- rbind(people_per_room.fn(state='06',year=2015,key=key),
#'                          people_per_room.fn(state='41',year=2015,key=key),
#'                          people_per_room.fn(state='53',year=2015,key=key)) %>%
#'                     inner_join(total_housing,by=c('name','state','county','tract')) %>%
#'     mutate(pct_crowded=as.numeric(as.character(gt_1_per_room))/as.numeric(as.character(total_housing_units)))
#' 
#' 
#' #-------------------------------------------------------------------------------------------------
#' 
#' 
#' #-------------------------------------------------------------------------------------------------
#' # fraction of households with no vehicle avaiable
#' 
#' #B08201_002E - total households with no vehicle available
#' 
#' no_vehicle <- rbind(no_vehicles.fn(state='06',year=2015,key=key),
#'                          no_vehicles.fn(state='41',year=2015,key=key),
#'                          no_vehicles.fn(state='53',year=2015,key=key)) %>%
#'   inner_join(total_housing,by=c('name','state','county','tract')) %>%
#'   mutate(pct_no_vehicle=as.numeric(as.character(no_vehicle))/as.numeric(as.character(total_housing_units)))
#' 
#' 
#' 
#' 
#' #-------------------------------------------------------------------------------------------------
#' 
#' #-------------------------------------------------------------------------------------------------
#' # percent of population living in group quarters
#' 
#' pct_group <- rbind(group_quarters.fn(state='06',year=2015,key=key),
#'                          group_quarters.fn(state='41',year=2015,key=key),
#'                          group_quarters.fn(state='53',year=2015,key=key)) %>%
#'   inner_join(total.pop,by=c('name','state','county','tract')) %>%
#'   mutate(pct_group=as.numeric(as.character(group_pop))/as.numeric(as.character(total_pop)))
#' 
#' 
#' 
#' 
#' #-------------------------------------------------------------------------------------------------
#' Sys.time() - test.time
#' 
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' ##########################################################################################################
#' 
#' 
#' #Now we glue everything together
#' 
#' svi.data <- tbl_df(pov) %>%
#'             inner_join(unemp,by=c('name','state','county','tract')) %>%
#'             inner_join(pci,by=c('name','state','county','tract')) %>%
#'             inner_join(pop25,by=c('name','state','county','tract')) %>%
#'             inner_join(ins,by=c('name','state','county','tract')) %>%
#'             inner_join(pop.sum,by=c('name','state','county','tract')) %>%
#'             inner_join(dis,by=c('name','state','county','tract')) %>%
#'             inner_join(single_parent,by=c('name','state','county','tract')) %>%
#'             inner_join(total_white,by=c('name','state','county','tract')) %>%
#'             inner_join(limited_eng,by=c('name','state','county','tract')) %>%
#'             inner_join(units,by=c('name','state','county','tract')) %>%
#'             inner_join(mobile,by=c('name','state','county','tract')) %>%
#'             inner_join(crowding,by=c('name','state','county','tract')) %>%
#'             inner_join(no_vehicle,by=c('name','state','county','tract')) %>%
#'             inner_join(group_quarters,by=c('name','state','county','tract'))
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
#' ####################################################################################################
