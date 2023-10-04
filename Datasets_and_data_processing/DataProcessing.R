
rm(list = ls())

packages <- c("tidyUSDA","tidyverse","sf","here","leaflet")

lapply(packages, require,character.only =T)

####

key <- "D923D273-EDCC-3FA9-AE2B-E5513DD00E06"


years <- c("1976","1977","1978","1979","1980","1981","1982","1983","1984",
           "1985","1986","1987","1988","1989","1990","1991","1992","1993",
           "1994","1995","1996","1997","1998","1999","2000","2001","2002",
           "2003","2004","2005","2006","2007","2008","2009","2010","2011",
           "2012","2013","2014","2015","2016","2017","2018","2019","2020",
           "2021","2022")


Sunflower_Yield <- getQuickstat(sector='CROPS',
                                group = "FIELD CROPS",
                                commodity = "SUNFLOWER",
                                category = "YIELD",
                                domain = "TOTAL",
                                key = key,
                                program = 'SURVEY',
                                data_item = "SUNFLOWER, OIL TYPE - YIELD, MEASURED IN LB / ACRE",
                                geographic_level = 'COUNTY',
                                year = years)



########### AREA HARVESTED DATA ##### 

Area_Harvested <- getQuickstat(sector='CROPS',
                               group = "FIELD CROPS",
                               commodity = "SUNFLOWER",
                               category = "AREA HARVESTED",
                               domain = "TOTAL",
                               key = key,
                               program = 'SURVEY',
                               data_item = "SUNFLOWER, OIL TYPE - ACRES HARVESTED",
                               geographic_level = 'COUNTY',
                               year = years)



###### AREA PLANTED DATA #### 

Area_Planted <- getQuickstat(sector='CROPS',
                             group = "FIELD CROPS",
                             commodity = "SUNFLOWER",
                             category = "AREA PLANTED",
                             domain = "TOTAL",
                             key = key,
                             program = 'SURVEY',
                             data_item = "SUNFLOWER, OIL TYPE - ACRES PLANTED",
                             geographic_level = 'COUNTY',
                             year = years)



#########
###  Cleaning the downloaded datasets ###
######## 

Sunflower_Yield <- Sunflower_Yield %>% 
  dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
  dplyr::filter(county_name != "OTHER COUNTIES") %>%
  dplyr::filter(state_name != "CALIFORNIA") %>%
  dplyr::select(year,state_alpha,county_name,Value) %>%
  dplyr::rename(YEAR = year,
                YIELD = Value,
                STATE = state_alpha,
                COUNTYNAME = county_name) %>% 
  mutate(COUNTYNAME = gsub(" ","_",COUNTYNAME))


Area_Harvested <- Area_Harvested %>% 
  dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
  dplyr::filter(county_name != "OTHER COUNTIES") %>%
  dplyr::filter(state_name != "CALIFORNIA") %>%
  dplyr::select(year,state_alpha,county_name,Value) %>%
  dplyr::rename(YEAR = year,
                ACRES_HARVESTED = Value,
                STATE = state_alpha,
                COUNTYNAME = county_name) %>% 
  mutate(COUNTYNAME = gsub(" ","_",COUNTYNAME))


Area_Planted <- Area_Planted %>% 
  dplyr::filter(county_name != "OTHER (COMBINED) COUNTIES") %>%
  dplyr::filter(county_name != "OTHER COUNTIES") %>%
  dplyr::filter(state_name != "CALIFORNIA") %>%
  dplyr::select(year,state_alpha,county_name,Value) %>%
  dplyr::rename(YEAR = year,
                ACRES_PLANTED = Value,
                STATE = state_alpha,
                COUNTYNAME = county_name) %>% 
  mutate(COUNTYNAME = gsub(" ","_",COUNTYNAME))


########
#### Joining the area planted, harvested and yield datasets ###
###### 

Sunflower <- list(Sunflower_Yield,Area_Planted,
                  Area_Harvested) %>% 
  purrr::reduce(inner_join) 


#### read in the state shape file #### 

States <- st_read(here("Datasets_and_data_processing","US_county_centroids",
                       "c_08mr23.shp")) %>% 
  dplyr::mutate(COUNTYNAME = str_to_upper(COUNTYNAME)) %>% 
  dplyr::select(STATE,COUNTYNAME,LON,LAT,geometry)

st_crs(States)

### Lets join the two ### 
### and filter the counties which have only one representation

Sunflower <- Sunflower %>% 
                inner_join(States)

## I am doing this for shinylive so that I have a better looking non-interactive map ##
Sunflower_2 <- Sunflower %>% 
                  right_join(States)



######### removing counties which have only occured once in the dataset and also,
## removing years where we have only one county 

Sunflower <- Sunflower %>%
  # Remove counties that only occur once in the dataset
  group_by(COUNTYNAME) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  # Remove years where only one county is represented
  group_by(YEAR) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  # Remove state-year combinations with only one county or no yield values
  group_by(STATE, YEAR) %>%
  filter(n_distinct(COUNTYNAME) > 1 & any(!is.na(YIELD))) %>%
  ungroup() %>%
  # Convert to simple features object (if your data is spatial)
  st_as_sf() 


Sunflower_2 <- Sunflower_2 %>%
  # Remove counties that only occur once in the dataset
  group_by(COUNTYNAME) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  # Remove years where only one county is represented
  group_by(YEAR) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  # Remove state-year combinations with only one county or no yield values
  group_by(STATE, YEAR) %>%
  filter(n_distinct(COUNTYNAME) > 1 & any(!is.na(YIELD))) %>%
  ungroup() %>%
  # Convert to simple features object (if your data is spatial)
  st_as_sf() 





### save this file #### 

path_to_save <- here("YieldScapeUSA",
                     "Sunflower.RDS")




saveRDS(Sunflower,path_to_save)



path_to_save_shiny_live <- here("YieldScapeUSA_shinylive",
                                "Sunflower_shinylive.RDS")

saveRDS(Sunflower_2,path_to_save_shiny_live)



