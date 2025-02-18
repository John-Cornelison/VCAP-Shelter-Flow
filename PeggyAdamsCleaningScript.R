library(tidyverse)
library(readr)
library(readxl)
library(tidygeocoder)
library(httr)
library(stringi)
library(stringr)

setwd("C:/Users/john/Documents/GitHub_Data/VCAP-Shelter-Flow-Data/PeggyAdmas")

#I am fairly positive that utah_outcomes is actual intakes and utah intakes are actualy outcomes
PA_stray_intakes_2022 <- read_csv("Peggy Adams 2022 Total Intake Squarehead - Stray Found Locations.csv")
PA_intakes_2022 <- read_csv("Peggy Adams 2022 Total Intake Squarehead.csv") %>% 
  select(-Squarehead)
PA_outcomes_2022 <- read_csv("Peggy Adams 2022 Total Outcomes.csv") %>% 
  select(-Squarehead)
PA_stray_intakes_2023 <- read_csv("Peggy Adams 2023 Total Intake Squarehead - Stray Found Locations.csv")
PA_intakes_2023 <- read_csv("Peggy Adams 2023 Total Intake Squarehead.csv") %>% 
  select(-Squarehead, -`Age (Month)`)
PA_outcomes_2023 <- read_csv("Peggy Adams 2023 Total Outcomes.csv") %>% 
  rename("Street" = "Address")
SVI_index <- read_csv("SVI_2022_US.csv")


#combining 2022 and 2023
PA_intakes_comb <- rbind(PA_intakes_2022, PA_intakes_2023) %>% 
  distinct()
PA_outcomes_comb <- rbind(PA_outcomes_2022, PA_outcomes_2023) %>% 
  distinct()


###Both DataFrames should have the same animal ids.
PA_intake_a_ids <- PA_intakes_comb$`Animal ID`
PA_outcome_a_ids <- PA_outcomes_comb$`Animal ID`

#Figure 9 and 10 include other outcome types, but for the most part, all will be adoptions. So I will 
#create an adoptions df and a total outcomes df.

PA_outcomes_cleaned <- PA_outcomes_comb %>% 
  filter(`Animal ID` %in% PA_intake_a_ids) %>% 
  rename("Age_Group_at_Outcome" = "Age Group",
         "Size_Group_at_Outcome" = "Size Group") %>% 
  mutate(Age_Group_at_Outcome = case_when(Age_Group_at_Outcome == "Adult Cat (6 months-7 years)" ~ "10",
                                          Age_Group_at_Outcome == "Adult Dog (6 months-7 years)" ~ "10",
                                          Age_Group_at_Outcome == "Kitten (6 weeks-6 months)" ~ "1",
                                          Age_Group_at_Outcome == "Neonate (0-6 weeks)" ~ "1",
                                          Age_Group_at_Outcome == "Puppy (6 weeks-6 months)" ~ "1",
                                          Age_Group_at_Outcome == "Senior Cat (7+years)" ~ "10",
                                          Age_Group_at_Outcome == "Senior Dog (7+years)" ~ "10",
                                          TRUE ~ Age_Group_at_Outcome),
         Age_Group_at_Outcome = as.numeric(Age_Group_at_Outcome),
         Age_Group_at_Outcome = case_when(Age_Group_at_Outcome > 0 & Age_Group_at_Outcome < 7 ~ "Youth",
                                          Age_Group_at_Outcome >= 7 ~ "Adult",
                                          TRUE ~ as.character(Age_Group_at_Outcome)),
         Full_Address_Outcome = paste0(Street, ", ", City, ", ", State, " ", Zip, ", ", "USA"),
         Outcome_Date = mdy(`Outcome Date`))

PA_intakes_cleaned <- PA_intakes_comb %>%
  filter(`Animal ID` %in% PA_outcome_a_ids) %>% 
  rename("Age_Group_at_Intake" = "Age Group",
         "Size_Group_at_Intake" = "Size Group") %>% 
  mutate(Age_Group_at_Intake = case_when(Age_Group_at_Intake == "Adult Cat (6 months-7 years)" ~ "Adult",
                                         Age_Group_at_Intake == "Adult Dog (6 months-7 years)" ~ "Adult",
                                         Age_Group_at_Intake == "Kitten (6 weeks-6 months)" ~ "Youth",
                                         Age_Group_at_Intake == "Neonate (0-6 weeks)" ~ "Youth",
                                         Age_Group_at_Intake == "Puppy (6 weeks-6 months)" ~ "Youth",
                                         Age_Group_at_Intake == "Senior Cat (7+years)" ~ "Adult",
                                         Age_Group_at_Intake == "Senior Dog (7+years)" ~ "Adult",
                                         TRUE ~ Age_Group_at_Intake),
         Full_Address_Intake = paste0(Street, ", ", City, ", ", State, " ", Zip, ", ", "USA"),
         Intake_Date = mdy(`Intake Date`)) %>% 
  select(-Species, -`Primary Breed`, -Street, -City, -State, -Zip)

combined_df <- PA_outcomes_cleaned %>% 
  left_join(PA_intakes_cleaned, by = "Animal ID") %>% 
  group_by(`Animal ID`) %>% 
  filter(Outcome_Date >= Intake_Date) %>% 
  mutate(LOS = Outcome_Date - Intake_Date,
         unique_intake = paste0(`Animal ID`, "_", Outcome_Date)) %>%
  group_by(unique_intake) %>% 
  filter(LOS == min(LOS)) %>% 
  ungroup() %>% 
  select(`Animal ID`, Species, `Primary Breed`, Intake_Date, `Intake Type`,
         Age_Group_at_Intake, Full_Address_Intake, Outcome_Date, `Outcome Type`,
         Age_Group_at_Outcome, Full_Address_Outcome) %>% 
  drop_na(Age_Group_at_Outcome)


cleaned_combined_df <- combined_df %>% 
  mutate(Full_Address_Intake = stri_trans_general(Full_Address_Intake, "Latin-ASCII"),
         Full_Address_Outcome = stri_trans_general(Full_Address_Outcome, "Latin-ASCII"),
         across(c(Full_Address_Intake, Full_Address_Outcome), ~ gsub(",,", ",", as.character(.))),
         intake_address_comma_count = str_count(Full_Address_Intake, ","),
         outcome_address_comma_count = str_count(Full_Address_Outcome, ","),
         #This cleans appartment addresses.
         Full_Address_Outcome = ifelse(outcome_address_comma_count == 4,
                                              gsub("^[^,]*,\\s*", "", Full_Address_Outcome),
                                              Full_Address_Outcome),
         `Intake Type` = case_when(`Intake Type` == "Transfer in" ~ "Transfer In",
                                   TRUE ~ `Intake Type`),
         Species = case_when(Species == "CAT" ~ "Cat",
                             Species == "DOG" ~ "Dog",
                             TRUE ~ Species)) %>% 
  filter(!Full_Address_Intake %in% c("N, NA, FL NA, USA", "Po Box 2444, Palm Beach, FL 33480, USA", "P.O. Box 774 West Palm Beach, Fl, 33402, West Palm Beach, FL 33402, USA"))



Intake_Addresses <- cleaned_combined_df$Full_Address_Intake
Outcome_Addresses <- cleaned_combined_df$Full_Address_Outcome

All_Addresses <- data.frame(c(Intake_Addresses, Outcome_Addresses)) %>% 
  unique() %>% 
  rename("Addresses" = "c.Intake_Addresses..Outcome_Addresses.")

geocoded_addresses <- geocode(All_Addresses,
                                        address = Addresses,
                                        method = "google")

geocoded_addresses <- geocoded_addresses %>% 
  filter(Addresses != "9 Old Mill Road, Toronto, ON NA, USA")

####Grabbing Census tracts from lat and long

grabCensusTract <- function(lat, long){
  
  url <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=", 
                long, "&y=", lat, "&benchmark=Public_AR_Current&vintage=Current_Current&format=json")
  
  response <- GET(url)
  
  geo_data <- content(response, as = "parsed")
  
  GEOID = geo_data$result$geographies$`Census Tracts`[[1]]$GEOID
  
  return(GEOID)
  
}

final_geo_coded_df <- geocoded_addresses %>% 
  mutate(census_tract = map2_chr(lat, long, grabCensusTract))

#write_csv(final_geo_coded_df, "PeggyAdamsCensusTracts.csv")

PeggyAdamsCensusTracts <- read_csv("PeggyAdamsCensusTracts.csv")


###Creating final Dataframe for Graphics
#seperating intakes and outcomes columns to two dfs
cleaned_combined_w_census_tracts <- cleaned_combined_df %>% 
  left_join(PeggyAdamsCensusTracts, by = c("Full_Address_Intake" = "Addresses")) %>% 
  rename("intake_lat" = "lat",
         "intake_long" = "long",
         "intake_census_tract" = "census_tract") %>% 
  mutate(intake_census_tract = case_when(nchar(as.character(intake_census_tract)) == 10 ~ paste0("0", as.character(intake_census_tract)),
                                         TRUE ~ as.character(intake_census_tract))) %>% 
  left_join(PeggyAdamsCensusTracts, by = c("Full_Address_Outcome" = "Addresses")) %>% 
  rename("outcome_lat" = "lat",
         "outcome_long" = "long",
         "outcome_census_tract" = "census_tract") %>% 
  mutate(outcome_census_tract = case_when(nchar(as.character(outcome_census_tract)) == 10 ~ paste0("0", as.character(outcome_census_tract)),
                                         TRUE ~ as.character(outcome_census_tract))) %>% 
  left_join(select(SVI_index, "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "E_HH", "FIPS"), SVI_index, by = c("intake_census_tract" = "FIPS")) %>% 
  rename("RPL_THEME1_INT" = "RPL_THEME1",
         "RPL_THEME2_INT" = "RPL_THEME2",
         "RPL_THEME3_INT" = "RPL_THEME3",
         "RPL_THEME4_INT" = "RPL_THEME4",
         "RPL_THEMES_INT" = "RPL_THEMES",
         "E_HH_INT" = "E_HH") %>% 
  left_join(select(SVI_index, "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "E_HH", "FIPS"), SVI_index, by = c("outcome_census_tract" = "FIPS")) %>% 
  rename("RPL_THEME1_OUT" = "RPL_THEME1",
         "RPL_THEME2_OUT" = "RPL_THEME2",
         "RPL_THEME3_OUT" = "RPL_THEME3",
         "RPL_THEME4_OUT" = "RPL_THEME4",
         "RPL_THEMES_OUT" = "RPL_THEMES",
         "E_HH_OUT" = "E_HH") %>% 
  filter(RPL_THEMES_INT != -999.0000) %>% 
  filter(RPL_THEMES_OUT != -999.0000)

write_csv(cleaned_combined_w_census_tracts, "final_peggy_adams_dat.csv")
saveRDS(cleaned_combined_w_census_tracts, "final_peggy_adams_dat.rds")

