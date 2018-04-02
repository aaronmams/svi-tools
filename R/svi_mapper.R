library(sf)
library(ggplot2)
library(dplyr)

#----------------------------------------------------------------------------
# 
# 1. get the SVI data from ACS 5 yr for 2015
# 2. map ports to counties
# 3. map places to counties
# 4. 


#----------------------------------------------------------------------------



#-----------------------------------------------------------------------
# Get shapefiles for county and place boundaries
ca_place <- st_read("shapefiles/cb_2016_06_place_500K/cb_2016_06_place_500K.shp") %>% 
  st_transform(4326) # WGS84

ca_county <- st_read("shapefiles/cb_2015_us_county_500K/cb_2015_us_county_500K.shp") %>% 
  st_transform(4326) %>%  # WGS84
  filter(STATEFP=='06') 
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# map county and place boundaries for CA together
basemap <- ggplot() + 
  geom_sf(data = ca_county, colour = "gray50", fill=NA) 

bm2 <- basemap + 
  geom_sf(data=ca_place,colour='red',fill=NA)
#--------------------------------------------------------------------------

#-------------------------------------------------------------------------
#join places and counties on geometry
test <- st_join(ca_place,ca_county)

#now map all places in a subset of counties
counties <- c('Del Norte')
places <- test$NAME.x[test$NAME.y %in% counties]

basemap <- ggplot() + 
  geom_sf(data = subset(ca_county,NAME %in% counties), colour = "gray50", fill=NA) 

bm2 <- basemap + 
  geom_sf(data=subset(ca_place,NAME %in% places),colour='red',fill=NA) + theme_bw()
#--------------------------------------------------------------------------