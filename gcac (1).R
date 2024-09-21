# gcac SVI flow

library(tidyverse)

# load warehouse files
gcac_wh  <- read.csv('data/gcac/gcac_wh.csv') %>%
  filter(!(outcome_type %in% c('Report','Disposal','Admin, Lost, Missing or Stolen'))) %>%
  filter(!(intake_type %in% c('Report','Disposal / DOA'))) %>%
  # ~25 dupe records
  distinct(src_animal_id, species, intake_date, intake_type, outcome_date, 
           outcome_type, found_location_lat, .keep_all=T)

# merge adoptions files (WH and geocoding) 
gcac_adopt_geo <- read.csv('data/gcac/gcac_adopt_geo.csv')
gcac_adopt <- read.csv('data/gcac/gcac_adopt.csv') %>% filter(Outcome.Type=='Adoption')

gcac_adopt <-  gcac_adopt %>%
  mutate(geocode_id=as.character(id)) %>%
  mutate(outcome_date_match=as.character(as.Date(Outcome.Date,'%m/%d/%Y'))) %>%
  rename(src_outcome_date=Outcome.Date, src_animal_id=Animal.ID) %>%
  inner_join(gcac_adopt_geo, by='geocode_id')  %>%
  rename(outcome_lng=lng, outcome_lat=lat, outcome_tract=tract)

names(gcac_adopt)

# standard intake reason group
gcac_wh$reason_short <- case_when(
  str_detect(gcac_wh$hass_intake_reason,'Sickness') ~ 'Medical',
  str_detect(gcac_wh$hass_intake_reason,'Housing') ~ 'Housing',
  str_detect(gcac_wh$hass_intake_reason,'Behavior') ~ 'Behavior',
  str_detect(gcac_wh$hass_intake_reason,'afford') ~ 'Cost',
  gcac_wh$hass_intake_reason=='' ~ NA,
  TRUE ~ 'Other',
)
table(gcac_wh$reason_short)

# pull geocodes from geo census file that I used to improve positionstack results
geo_census <- read.csv('data/gcac/wh_geo_census.csv')
# only 188 geocoded for gcac
count(geo_census, hass_shelter_id, is.na(new_lng))

gcac_wh %>%
  mutate(hass_shelter_id='gcac') %>%
  left_join(geo_census %>% select(c(1,2,3,5,10,11,12)),
            by=c('hass_shelter_id','src_animal_id','src_intake_date','src_intake_subtype')) %>%
  filter(is.na(new_tract)) %>% 
  filter(str_detect(found_location_lng,'fail'))# %>% names()

# actual joining - new location columns are now combined_lng, combined_lat
gcac_wh <- gcac_wh %>% 
  mutate(hass_shelter_id='GCAC') %>%
  left_join(geo_census %>% select(hass_shelter_id,src_animal_id, src_intake_type, 
                                  src_found_location,new_lng,new_lat,new_tract),
            by=c('hass_shelter_id','src_animal_id','src_intake_type','src_found_location'), multiple='first') %>%
  mutate(combined_lng = ifelse(found_location_lng=='failed geocoding' &
                                 !is.na(new_lng),new_lng,found_location_lng)) %>%
  mutate(combined_lat = ifelse(found_location_lat=='failed geocoding' &
                                 !is.na(new_lat),new_lat,found_location_lat)) 

# got 173 pf the 188
gcac_wh %>%   count(found_location_lng=='failed geocoding',is.na(new_lng))
gcac_wh %>%   count(found_location_lng=='failed geocoding',combined_lng=='failed geocoding')

# testing join wh data onto adoption geocoding 
gcac_wh %>% 
  #mutate(outcome_date_wh=as.character(as.Date(src_outcome_date,'%m/%d/%Y'))) %>%
  filter(outcome_type=='Adopted') %>%  #nrow() # 7392 adoptions in WH
  #filter(intake_type!='Service In / Public Assistance') %>%
  left_join(gcac_adopt,by=c('src_animal_id')) %>% 
  mutate(date_diff = as.Date(outcome_date_match) - as.Date(outcome_date)) %>% #count(date_diff)
  filter(date_diff >= 0 & date_diff <=1) %>% #count(src_animal_id) %>% count(n)
  #filter(is.na(outcome_lng)) %>%  # gaining 28 dupes 
  nrow()

# starting from 7392 adoptions in 2022-2023 whose intake was also in 2022-2023
adoptions <- gcac_wh %>% 
  filter(outcome_type=='Adopted') %>% #count(intake_type) #nrow() # 7392 adoptions
  inner_join(gcac_adopt,by=c('src_animal_id')) %>% 
  mutate(date_diff = as.Date(outcome_date_match) - as.Date(outcome_date)) %>% #count(date_diff)
  filter(date_diff >= 0 & date_diff <=1)  

table(adoptions$intake_type)
# 4 born in care, 33 service in, 25 transfers, 60 wildlife = 122 expected to be removed
# leaving 7270. Can see below that about 800 animals had found locations in the shelter

# check geo quality of intake - 79 missing in 2022, 75 in 2023 - leaving 1459
adoptions %>% count(src_found_location) %>% arrange(desc(n)) %>% head(20)
shelter_addr <- '328 furman|298 broadcast dr'

# remove failed intake geocoding addresses
adoptions <- 
  adoptions %>%
  mutate(addr = case_when(
    src_found_location=='' | is.na(src_found_location) ~ 'no address',
    str_detect(str_to_lower(src_found_location),shelter_addr) ~ 'shelter address', # shelter address 
    found_location_lng=='' ~ 'not attempted geo',
    str_detect(combined_lng,'fail|skip|warn') ~ 'attempted geo',
    TRUE ~  'success geo'
  ))  %>%
  mutate(intake_lng = case_when(
    found_location_lng=='' ~ NA,
    str_detect(str_to_lower(src_found_location),shelter_addr) ~ NA, # shelter address 
    str_detect(combined_lng,'fail|skip|warn') ~ NA,
    TRUE ~  combined_lng
  )) %>%
  mutate(intake_lat = case_when(
    found_location_lat=='' ~ NA,
    str_detect(str_to_lower(src_found_location),shelter_addr) ~ NA, # shelter address 
    str_detect(combined_lat,'fail|skip|warn') ~ NA,
    TRUE ~ combined_lat
  )) %>%
  # gcac - looking into many OS who had intake shelter address - these are FIELD OS
  #filter(intake_type=='OS' & addr=='shelter address') %>% count(src_intake_subtype)
  #filter(intake_type=='Stray' & addr=='attempted geo') %>% count(src_found_location) %>% arrange(desc(n))
  #count(intake_type,addr)
  # filter intake types - transfer and BIC bc no in addr, ORE bc only 8 and Ser In not geod
  filter(!is.na(intake_lat) & !is.na(intake_lng)) %>%
  filter(!(intake_type %in% c('Transfer In','Born In Care', 'Wildlife',
                              'Service In / Public Assistance','Owner Requested Euthanasia')))


# in addition to removing 263 BIC, 2 transf, 177 service in as expected, 
# 107 failed/no OS (17%) plus 515 has shelter address ( field OS), 24 failed returns (15%),
# 882 failed stray geocoding - looks like a lot of missing addresses
  
# adoptions df at this point has 5557 -> 5619

# so far only filtered by intake geography - need to remove missing adopt lng lat
#  only 75! so now at 5544
adoptions %>%  count(is.na(outcome_lng))
adoptions %>%  count(str_detect(outcome_lng,'fail|skip|warn'))

# changing to also remove those who failed
adoptions <- adoptions %>% filter(!is.na(outcome_lng)) %>%
  filter(!str_detect(outcome_lng,'fail|skip|warn'))

# group reasons - barely using, not worth it
table(adoptions$src_intake_subtype)

# get census tracts
library(tidycensus)
library(sf)

table(adoptions$state)

census_data <- get_acs(geography = "tract",
                       variables = "B01003_001",
                       state=c(13,37,45),
                       #county='510', # c('005','510','003'), #
                       year=2022,
                       geometry = T,
                       summary_var = "B01003_001") # total population to get percentage

sf_census <- census_data %>% st_as_sf()

table(adoptions$intake_type)

# make points as sf
adoptions$id <- 1:nrow(adoptions)

intake_tract_points <- adoptions %>%
  mutate(intake_lng=as.numeric(intake_lng),intake_lat=as.numeric(intake_lat)) %>%
  select(id, intake_lng, intake_lat) %>%
  st_as_sf(coords = c("intake_lng", "intake_lat"), crs = 4326)


# get indexes of tract
#options(warn = 1)
tract_indexes <- st_intersects(intake_tract_points,
                               sf_census %>% st_transform(4326), sparse = T) %>% 
  as.character() %>% as.numeric()

head(intake_tract_points)
head(tract_indexes)

# test - looks like it worked well 
plot(sf_census$geometry[6553])
plot(intake_tract_points[1,], add=T)

sum(is.na(tract_indexes)) #  6 points lost, came from outside these states I guess

# assign tract using the index found by st_intersects
intake_tract_points$intake_GEOID <- sf_census$GEOID[tract_indexes]

# rejoin intake tract (geoid into adoptions)
adoptions <- adoptions %>%
  inner_join(intake_tract_points %>% as.data.frame() %>% 
               select(id,intake_GEOID), by='id')


# some outcomes geocoded already with census already, just need GEOID format
summary(is.na(adoptions)) # but 471 are not
adoptions$outcome_GEOID_census <- paste0(sprintf('%02d',adoptions$state),
                                  sprintf('%03d',adoptions$county),
                                  sprintf('%06d',adoptions$outcome_tract))

# repeat for outcomes to make up missing ones and also compare to census geocoding output
outcome_tract_points <- 
  adoptions %>%
  mutate(outcome_lng=ifelse(outcome_lng=='#N/A',0,outcome_lng),outcome_lat=ifelse(outcome_lat=='#N/A',0,outcome_lat)) %>%
  mutate(outcome_lng=as.numeric(outcome_lng),outcome_lat=as.numeric(outcome_lat)) %>%
  select(id, outcome_lng, outcome_lat) %>%
  st_as_sf(coords = c("outcome_lng", "outcome_lat"), crs = 4326)


# get indexes of tract
#options(warn = 1)
outcome_tract_indexes <- st_intersects(outcome_tract_points,
                               sf_census %>% st_transform(4326), sparse = T) %>% 
  as.character() %>% as.numeric()

head(outcome_tract_points)
head(outcome_tract_indexes)

# test - looks like it worked well 
plot(sf_census$geometry[6481])
plot(outcome_tract_points[1,], add=T)

sum(is.na(outcome_tract_indexes)) #  30 points lost, came from outside these states I guess

# assign tract using the index found by st_intersects
outcome_tract_points$outcome_GEOID <- sf_census$GEOID[outcome_tract_indexes]

# rejoin intake tract (geoid into adoptions)
adoptions <- adoptions %>%
  inner_join(outcome_tract_points %>% as.data.frame() %>% 
               select(id,outcome_GEOID), by='id')


#add SVI for intake and outcome tract
# NOTE: in aggregate analysis this is overriden by code in all.R
gcac_svi <- read.csv('data/gcac/SouthCarolina.csv') %>%
  rbind(read.csv('data/gcac/NorthCarolina.csv')) %>%
  rbind(read.csv('data/gcac/Georgia.csv')) %>%
  select(STCNTY, COUNTY, FIPS, LOCATION, E_HH, E_TOTPOP, starts_with('RPL_TH')) %>%
  mutate(GEOID=as.character(FIPS)) # always ST-CTY-0TRACT

adoptions$intake_GEOID # looks like similar format, good on that front
adoptions$outcome_GEOID # looks like similar format, good on that front
# CA- need to pad with a zero bc intake_GEOID have 06 for CA while SVI file just 6
gcac_svi$GEOID# <- paste0('0',fresno_svi$GEOID)

# join svi of intake census tract
adoptions_svi <- adoptions %>%
  left_join(gcac_svi %>% select(intake_GEOID=GEOID,starts_with('RPL')), 
            by='intake_GEOID') %>%
  left_join(gcac_svi %>% select(outcome_GEOID=GEOID,starts_with('RPL')), 
            by='outcome_GEOID', suffix=c('_in','_out')) 

# 5,29 missing IN SVI or Out SVI, none have -999 for some reason
#" a value of -999 in any field either means the value was unavailable from the original
# census data or we could not calculate a derived value because of unavailable census data."

adoptions_svi %>%  filter(RPL_THEMES_out==-999) # 0
adoptions_svi %>% filter(is.na(RPL_THEMES_out)) # 6 removed in, 24 out

#  ~40 removed here bc they were adopted into other states, but none more than 5
# so  not worth fetching
adoptions_svi <- adoptions_svi %>%
  filter(!is.na(RPL_THEMES_out)) %>%
  filter(!is.na(RPL_THEMES_in)) %>%
  filter(RPL_THEMES_out!=-999 & RPL_THEMES_in!=-999) 

# FINAL FULL DATA RECORD COUNT - 4984 adoptions from starting point of ~ see above 
# save file for re-geocoding for adoptions because tablea's geo 
saveRDS(adoptions_svi, 'data/gcac/gcac_adoptions_svi_final.rds')

# 2022-2023 non adoptions  for consistency
non_adoptions <- gcac_wh # starting from the same WH file

# joint onto intake_with_geos - lose ~ 1000 outcomes (I guess for lack of address)
non_adoptions<- 
  non_adoptions %>%
  filter(str_detect(outcome_date,'2023|2022')) %>% #nrow() # 30205 records
  #count(intake_type, src_intake_subtype)
  # removing service in, wildlife, born in care -> 13990 records
  filter(intake_type %in% c('Stray','Seized / Confiscate / Quarantine','Returns',
                            'OS', 'Owner Requested Euthanasia')) %>% 
  # only those with geo intake
  mutate(intake_lng = case_when(
    found_location_lng=='' ~ NA,
    str_detect(str_to_lower(src_found_location),shelter_addr) ~ NA, # shelter address 
    str_detect(combined_lng,'fail|skip|warn') ~ NA,
    TRUE ~  combined_lng
  )) %>%
  mutate(intake_lat = case_when(
    found_location_lat=='' ~ NA,
    str_detect(str_to_lower(src_found_location),shelter_addr) ~ NA, # shelter address 
    str_detect(combined_lat,'fail|skip|warn') ~ NA,
    TRUE ~ combined_lat
  )) %>% 
  #count(intake_type, src_intake_subtype, is.na(intake_lng))
  filter(!is.na(intake_lng)) # removed 1400/3000 os, 12/180 OREs, 38/500 return, 2k/10k strays

10651/13990

# find census tract of all these intakes
non_adoptions$id <- 1:nrow(non_adoptions)

# make points as sf
intake_tract_points <- non_adoptions %>%
  select(id, intake_lng, intake_lat) %>%
  st_as_sf(coords = c("intake_lng", "intake_lat"), crs = 4326)

# get indexes of tract
tract_indexes <- st_intersects(intake_tract_points,
                               sf_census %>% st_transform(4326), sparse = T) %>% 
  as.character() %>% as.numeric()

sum(is.na(tract_indexes)) # 10 intakes missing

# assign tract using the index found by st_intersects
intake_tract_points$GEOID <- sf_census$GEOID[tract_indexes]

# rejoin intake tract (geoid into adoptions)
non_adoptions <- non_adoptions %>%
  inner_join(intake_tract_points %>% as.data.frame() %>% 
               select(id,intake_GEOID=GEOID), by='id')

non_adoptions <- non_adoptions %>%
  filter(!is.na(intake_GEOID))

# join svi of intake census tract - all kept
non_adoptions <- non_adoptions %>%
  left_join(gcac_svi %>% select(intake_GEOID=GEOID,starts_with('RPL')), 
            by='intake_GEOID') %>%
  filter(!is.na(RPL_THEMES) & RPL_THEMES!=-999)

# FINAL FULL DATA RECORD COUNT - 10651 non-adoptions 

# rearrange columns
non_adoptions <- non_adoptions %>%
  select(id, starts_with('intake'), starts_with('src_intake'),
         starts_with('src_out'), starts_with('outcome'), starts_with('RPL'),everything()) 


# BOTH DATA SETS DONE
saveRDS(non_adoptions, 'data/gcac/gcac_non_adoptions_final.rds')
