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
counties <- c('Del Norte','Humboldt','Mendocino')
places <- test$NAME.x[test$NAME.y %in% counties]

basemap <- ggplot() + 
  geom_sf(data = subset(ca_county,NAME %in% counties), colour = "gray50", fill="gray50") 

bm2 <- basemap + 
  geom_sf(data=subset(ca_place,NAME %in% places),colour='red',fill=NA) + theme_bw()
#--------------------------------------------------------------------------


#-------------------------------------------------------------------------
#join port point locations to county
ports.point <- tbl_df(read.csv('data/port_locs.csv')) %>%
          select(PACFIN_PORT_CODE, PORT_NAME, LATITUDE, LONGITUDE) %>%
          filter(!is.na(LATITUDE) & !is.na(LONGITUDE))
names(ports.point) <- c('port_code','port_name','latitude','longitude')

ports.point <- st_as_sf(ports.point, coords = c("longitude","latitude"), crs = 4326)

port.county <- st_join(ports.point,ca_county)
#--------------------------------------------------------------------------


basemap <- ggplot() + 
  geom_sf(data = port.county, colour = "gray50", 
          fill="gray50") 
