library(tidyverse)
library(readr)
library(readxl)
library(tidygeocoder)
library(httr)

setwd("C:/Users/john/Documents/GitHub_Data/VCAP-Shelter-Flow-Data")

#I am fairly positive that utah_outcomes is actual intakes and utah intakes are actualy outcomes
utah_intakes <- read_excel("Utah-Intake.xlsx")
utah_outcomes <- read_excel("Utah-Outcome.xlsx")
SVI_index <- read_csv("SVI_2022_US.csv")

table(utah_outcomes$`Outcome Type`)

table(utah_intakes$`Intake Type`)

utah_outcomes_cleaned <- utah_outcomes %>% 
  rename("Age Group at Outcome" = "Age Group",
         "Primary Breed at Outcome" = "Primary Breed",
         "Secoundary Breed at Outcome" = "Secondary Breed",
         "Age (Months) at Outcome" = "Age (Months)",
         "Size Group at Outcome" = "Size Group",
         "Outcome Sub-Type" = "Outcome Sub-type") %>% 
  filter(Species == "Cat" | Species == "Dog") %>% 
  mutate(`Age Group at Outcome` = case_when(`Age Group at Outcome` == "Adult Cat (1-7 years)" ~ "Adult",
                                            `Age Group at Outcome` == "Adult Dog (1-7 years)" ~ "Adult",
                                            `Age Group at Outcome` == "Kitten (8 weeks-5 months)" ~ "Youth",
                                            `Age Group at Outcome` == "Puppy (8 weeks-5 months)" ~ "Youth",
                                            `Age Group at Outcome` == "Senior Cats (7+years)" ~ "Adult",
                                            `Age Group at Outcome` == "Senior Dog (7+years)" ~ "Adult",
                                            `Age Group at Outcome` == "Teenage Kittens (5 months-1 years)" ~ "Youth",
                                            `Age Group at Outcome` == "Teenage Puppy (5 months-1 years)" ~ "Youth",
                                            `Age Group at Outcome` == "Underage Kitten (4-8 weeks)" ~ "Youth",
                                            `Age Group at Outcome` == "Underage Puppy (4-8 weeks)" ~ "Youth",
                                            `Age Group at Outcome` == "Unweaned (0-4 weeks)" ~ "Youth",
                                            TRUE ~ `Age Group at Outcome`))


utah_intakes_cleaned <- utah_intakes %>% 
  rename("Age Group at Intake" = "Age Group",
         "Primary Breed at Intake" = "Primary Breed",
         "Secoundary Breed at Intake" = "Secondary Breed",
         "Age (Months) at Intake" = "Age (Months)",
         "Size Group at Intake" = "Size Group",
         "Outcome To (Address)" = "Address",
         "Outcome To (City)" = "City",
         "Outcome To (State)" = "State",
         "Outcome To (Zip Code)" = "Zip Code") %>% 
  filter(Species == "Cat" | Species == "Dog") %>% 
  mutate(`Age Group at Intake` = case_when(`Age Group at Intake` == "Adult Cat (1-7 years)" ~ "Adult",
                                            `Age Group at Intake` == "Adult Dog (1-7 years)" ~ "Adult",
                                            `Age Group at Intake` == "Kitten (8 weeks-5 months)" ~ "Youth",
                                            `Age Group at Intake` == "Puppy (8 weeks-5 months)" ~ "Youth",
                                            `Age Group at Intake` == "Senior Cats (7+years)" ~ "Adult",
                                            `Age Group at Intake` == "Senior Dog (7+years)" ~ "Adult",
                                            `Age Group at Intake` == "Teenage Kittens (5 months-1 years)" ~ "Youth",
                                            `Age Group at Intake` == "Teenage Puppy (5 months-1 years)" ~ "Youth",
                                            `Age Group at Intake` == "Underage Kitten (4-8 weeks)" ~ "Youth",
                                            `Age Group at Intake` == "Underage Puppy (4-8 weeks)" ~ "Youth",
                                            `Age Group at Intake` == "Unweaned (0-4 weeks)" ~ "Youth"))

#Both dataframes share the columns above and there are no duplicates. We will do a union join to return all entries that are present in both
sum(duplicated(utah_outcomes_both))
sum(duplicated(utah_intakes_both))


colSums(is.na(utah_outcomes_both))
colSums(is.na(utah_intakes_both))

combined_df <- utah_outcomes_cleaned %>% 
  left_join(utah_intakes_cleaned, by = c("Intake Date", "Intake Type", "Name", "Outcome Date", "Outcome Type", "Sex", "Species")) %>% 
  select(-"Address Found (Full Address)", -"Address Found (Zip Code)", -"Address Found (Cross Street)", -"Intake Sub-Type.y", -"Outcome Sub-Type.y") %>% 
  rename("Intake Sub-Type" = "Intake Sub-Type.x",
         "Outcome Sub-Type" = "Outcome Sub-Type.x") %>% 
  drop_na(`Outcome To (Address)`) %>% 
  drop_na(`Intake From (Street Address)`) %>% 
  drop_na(`Intake Sub-Type`) %>% 
  #filtering for intakes and outcomes I need
  filter(`Outcome Type` == "Adoption") %>% 
  mutate(`Intake Sub-Type` = case_when(`Intake Sub-Type` == "Behavior of this pet" ~ "Behavior",
                                       `Intake Sub-Type` == "Too many pets" ~ "Preferences/Time",
                                       `Intake Sub-Type` == "Owner Health" ~ "Permanent Change",
                                       `Intake Sub-Type` == "No time for basic care" ~ "Preferences/Time",
                                       `Intake Sub-Type` == "Moving - can't find pet friendly housing" ~ "Housing",
                                       `Intake Sub-Type` == "Medical need of this pet" ~ "Medical",
                                       `Intake Sub-Type` == "Behavior of residents pets or people" ~ "Behavior",
                                       `Intake Sub-Type` == "Cost of basic care" ~ "Cost",
                                       `Intake Sub-Type` == "Homeless" ~ "Housing",
                                       `Intake Sub-Type` == "Owner health" ~ "Permenent Change",
                                       `Intake Sub-Type` == "Owner intended euthanasia" ~ "Preferences/Time"),
         full_intake_address = paste0(`Intake From (Street Address)`, ", ", `Intake From (City)`, ", ", `Intake From (State)`, " ", `Intake From (Zip Code)`, ", ", "USA"),
         full_outcome_address = paste0(`Outcome To (Address)`, ", ", `Outcome To (City)`, ", ", `Outcome To (State)`, " ", `Outcome To (Zip Code)`, ", ", "USA"))


#### Geocoding Intake and Outcome Addresses


geocoded_addresses_1st_batch <- geocode(combined_df,
                              address = full_intake_address,
                              method = "google")

combined_df_geo_code_Int <- geocoded_addresses_1st_batch %>% 
  rename("intake_lat" = "lat",
         "intake_long" = "long") %>% 
  mutate()

geocoded_addresses_2nd_batch <- geocode(combined_df_geo_code_Int,
                                        address = full_outcome_address,
                                        method = "google")


geo_coded_df <- geocoded_addresses_2nd_batch %>% 
  rename("outcome_lat" = "lat",
         "outcome_long" = "long")

####Grabbing Census tracts from lat and long

grabCensusTract <- function(lat, long){
  
  url <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=", 
                long, "&y=", lat, "&benchmark=Public_AR_Current&vintage=Current_Current&format=json")
  
  response <- GET(url)
  
  geo_data <- content(response, as = "parsed")
  
  GEOID = geo_data$result$geographies$`Census Tracts`[[1]]$GEOID
  
  return(GEOID)
  
}


final_geo_coded_df <- geo_coded_df %>% 
  mutate(intake_census_tract = map2_chr(intake_lat, intake_long, grabCensusTract),
         outcome_census_tract = map2_chr(outcome_lat, outcome_long, grabCensusTract))

final_df_w_SVI <- final_geo_coded_df %>% 
  left_join(select(SVI_index, "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "FIPS"), SVI_index, by = c("intake_census_tract" = "FIPS")) %>% 
  rename("RPL_THEME1_INT" = "RPL_THEME1",
         "RPL_THEME2_INT" = "RPL_THEME2",
         "RPL_THEME3_INT" = "RPL_THEME3",
         "RPL_THEME4_INT" = "RPL_THEME4",
         "RPL_THEMES_INT" = "RPL_THEMES") %>% 
  left_join(select(SVI_index, "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "FIPS"), SVI_index, by = c("outcome_census_tract" = "FIPS")) %>% 
  rename("RPL_THEME1_OUT" = "RPL_THEME1",
         "RPL_THEME2_OUT" = "RPL_THEME2",
         "RPL_THEME3_OUT" = "RPL_THEME3",
         "RPL_THEME4_OUT" = "RPL_THEME4",
         "RPL_THEMES_OUT" = "RPL_THEMES")

write_csv(final_df_w_SVI, "final_utah_dat.csv")
saveRDS(final_df_w_SVI, "final_utah_dat.rds")
