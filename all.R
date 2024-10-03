### create joint data frame with NATIONAL SVI

# load national SVI file
svi <- read.csv('data/SVI_2022_US.csv') %>%
  select(STCNTY, COUNTY, FIPS, LOCATION, E_HH, E_TOTPOP, starts_with('RPL_TH')) %>%
  mutate(GEOID=as.character(FIPS)) # always ST-CTY-0TRACT

# create new joint df but with SVI values from the national SVI file
joint_adoptions_svi <- data.frame()
joint_non_adoptions <- data.frame()
shelter_names <- c('barcs','cac','fresno','kc','rochester','pvas','gcac')
# create one big df - not that SVI values are not being pulled from individual files bc added from national file
for (sh in shelter_names){
  cat(sh," ")
  joint_adoptions_svi <- rbind(joint_adoptions_svi, 
                               readRDS(paste0('data/',sh,'/',sh,'_adoptions_svi_final.rds')) %>%
                                 select(id, hass_shelter_id, species, src_intake_type, src_intake_subtype, intake_type, reason_short,
                                        hass_intake_reason, age_group, size_group, outcome_type, intake_GEOID, outcome_GEOID))
  
  joint_non_adoptions <- rbind(joint_non_adoptions, 
                               readRDS(paste0('data/',sh,'/',sh,'_non_adoptions_final.rds')) %>%
                                 select(id, hass_shelter_id, species, src_intake_type, src_intake_subtype, intake_type, reason_short,
                                        hass_intake_reason, age_group, size_group, outcome_type, intake_GEOID))
}

# join new SVI value - KC, CAC, BARCS, GCAC will have some meaningful differences - worth rerunning 
joint_adoptions_svi <- joint_adoptions_svi %>% 
  # fix california geoids - '6' instead of '06'
  mutate(intake_GEOID=ifelse(substr(intake_GEOID,0,2)=='06',str_sub(intake_GEOID,2,-1),intake_GEOID)) %>%
  left_join(svi %>% select(intake_GEOID=GEOID,starts_with('RPL')), 
            by='intake_GEOID') %>%
  mutate(outcome_GEOID=ifelse(substr(outcome_GEOID,0,2)=='06',str_sub(outcome_GEOID,2,-1),outcome_GEOID)) %>%
  left_join(svi %>% select(outcome_GEOID=GEOID,starts_with('RPL')), 
            by='outcome_GEOID', suffix=c('_in','_out'))

joint_non_adoptions <- joint_non_adoptions %>% 
  mutate(intake_GEOID=ifelse(substr(intake_GEOID,0,2)=='06',str_sub(intake_GEOID,2,-1),intake_GEOID)) %>%
  left_join(svi %>% select(intake_GEOID=GEOID,starts_with('RPL')), 
            by='intake_GEOID') 

# change BARCS seized custody to stray
joint_adoptions_svi %>%
  filter(hass_shelter_id=='BARCS') %>%
  count(src_intake_type,src_intake_subtype)

joint_adoptions_svi <- joint_adoptions_svi %>%
  mutate(src_intake_type=ifelse(hass_shelter_id=='BARCS' & str_detect(src_intake_subtype,'Stray'),'Stray',src_intake_type)) %>%
  mutate(src_intake_type=ifelse(hass_shelter_id=='BARCS' & str_detect(src_intake_subtype,'Field Owner surrender'),'Owner/Guardian Surrender',src_intake_type)) 

joint_non_adoptions <- joint_non_adoptions %>%
  mutate(src_intake_type=ifelse(hass_shelter_id=='BARCS' & str_detect(src_intake_subtype,'Stray'),'Stray',src_intake_type))  %>%
  mutate(src_intake_type=ifelse(hass_shelter_id=='BARCS' & str_detect(src_intake_subtype,'Field Owner surrender'),'Owner/Guardian Surrender',src_intake_type)) 


# edit reason_short to reflect NAs or general reasons
# also seems like some shelters have "other" where it should be a reason so redo it
# also created new category, leaving other as owner death, perm life change
table(joint_adoptions_svi$reason_short, joint_adoptions_svi$hass_intake_reason)

joint_adoptions_svi <- joint_adoptions_svi %>%
  mutate(reason_short=case_when(
    hass_intake_reason=='Not applicable (abandoned, found)' ~ 'Unknown',
    hass_intake_reason=='General/Other' ~ 'Unknown',
    is.na(hass_intake_reason) ~ 'Unknown',
    str_detect(hass_intake_reason,'Behavior') ~ 'Behavior',
    str_detect(hass_intake_reason,'afford|term life crisis') ~ 'Cost',
    str_detect(hass_intake_reason,'Housing') ~ 'Housing',
    str_detect(hass_intake_reason,'Sickness') ~ 'Medical',
    str_detect(hass_intake_reason,'preferences|many pets|overwhelm') ~ 'Preferences/Time',
    str_detect(hass_intake_reason,'death|Permanent') ~ 'Permanent Change',
    TRUE ~ reason_short
  ))

# same grouping herre
joint_non_adoptions <- joint_non_adoptions %>%
  mutate(reason_short=case_when(
    hass_intake_reason=='Not applicable (abandoned, found)' ~ 'Unknown',
    hass_intake_reason=='General/Other' ~ 'Unknown',
    is.na(hass_intake_reason) ~ 'Unknown',
    str_detect(hass_intake_reason,'Behavior') ~ 'Behavior',
    str_detect(hass_intake_reason,'afford|term life crisis') ~ 'Cost',
    str_detect(hass_intake_reason,'Housing') ~ 'Housing',
    str_detect(hass_intake_reason,'Sickness') ~ 'Medical',
    str_detect(hass_intake_reason,'preferences|many pets|overwhelm') ~ 'Preferences/Time',
    str_detect(hass_intake_reason,'death|Permanent') ~ 'Permanent Change',
    TRUE ~ reason_short
  ))

# REMOVE ADOPTION OUTCOMES FROM NON_ADOPTIONS DATASET
joint_non_adoptions <- joint_non_adoptions %>%
  filter(!(outcome_type %in% c('Disposal','Adopted','Admin, Lost, Missing or Stolen',
                               'Wildlife Out','Report'))) %>%
  # also remove intake types not included in analysis
  filter(intake_type!='Service In / Public Assistance') %>%
  filter(intake_type!='Disposal / DOA') %>%
  filter(intake_type!='Born In Care') 

# relabel some OS as seized and Stray as seized
joint_non_adoptions <- joint_non_adoptions %>%
  mutate(src_intake_type=case_when(
    src_intake_type=='Owner/Guardian Surrender' & 
        str_detect(src_intake_subtype,'Bite|uarantin') ~ 'Seized / Custody',
    src_intake_type=='Stray' & 
        str_detect(src_intake_subtype,'onfiscate|uarantin|nforcement|Police') ~ 'Seized / Custody',
    TRUE ~ src_intake_type
  )) %>%
  mutate(src_intake_type=ifelse(src_intake_type=='Return','OS / Returns',src_intake_type)) %>%
  mutate(src_intake_type=ifelse(src_intake_type=='Owner/Guardian Surrender','OS / Returns',src_intake_type)) 


# REMOVE other species FROM ADOPTIONS_SVI
joint_adoptions_svi <- joint_adoptions_svi %>%
  filter(species!='Other' & species!='') %>%
  filter(src_intake_type!='Clinic' & src_intake_type!='Wildlife In')

# remove rare intake types and rename OS and return into one category
joint_adoptions_svi <- joint_adoptions_svi %>%
  filter(intake_type!='Born In Care' & intake_type!='Service In / Public Assistance') %>%
  filter(intake_type!='Disposal / DOA') %>%
  mutate(src_intake_type=ifelse(src_intake_type=='Return','OS / Returns',src_intake_type)) %>%
  mutate(src_intake_type=ifelse(src_intake_type=='Owner/Guardian Surrender','OS / Returns',src_intake_type)) 

# recategorize some stray as SEIZED 
joint_adoptions_svi <- joint_adoptions_svi %>%
  mutate(src_intake_type=case_when(
    src_intake_type=='OS / Returns' & 
      str_detect(src_intake_subtype,'Bite|uarantin') ~ 'Seized / Custody',
    src_intake_type=='Stray' & 
      str_detect(src_intake_subtype,'onfiscate|uarantin|nforcement|Police') ~ 'Seized / Custody',
    TRUE ~ src_intake_type
  ))

table(joint_adoptions_svi$src_intake_type)
table(joint_non_adoptions$src_intake_type)

adoptions_svi <- joint_adoptions_svi
non_adoptions <- joint_non_adoptions
svi_file <- svi

adoptions_svi %>%
  filter(is.na(RPL_THEMES_in))

# save final result
saveRDS(joint_adoptions_svi, 'data/joint/joint_adoptions_svi_final.rds')
saveRDS(joint_non_adoptions, 'data/joint/joint_non_adoptions_final.rds')
saveRDS(svi_file, 'data/joint_svi.rds')


# statistical tests
# Wilcoxon Signed Rank Test: nonparametric test that can be used to determine whether 
#   two dependent samples were selected from populations having the same distribution.
# effect size - dividing the Wilcoxon test statistic by the sqrt of the sample size
# 0.10–0.29 is small, 0.30–0.49 is moderate effect, and ≥0.5 large

# overall test - using scores (not quintiles)
library(rcompanion) 

# ALL ADOPTIONS
foo <- adoptions_svi %>% 
  select(RPL_THEMES_in, RPL_THEMES_out) %>%
  pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
  arrange(in_out)

wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
            paired=TRUE, exact=FALSE) # the test
wilcoxonPairedR(x = foo$svi, g = foo$in_out) # effect size

# BY INTAKE TYPE
for (t in c('Seized / Custody','OS / Returns','Stray')){
  foo <- adoptions_svi %>% 
    #mutate(src_intake_type=ifelse(src_intake_type=='Return','Owner/Guardian Surrender',src_intake_type)) %>%
    filter(src_intake_type==t) %>%
    select(RPL_THEMES_in, RPL_THEMES_out) %>%
    pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
    arrange(in_out)
  
  cat("number of rows selected for", t, ":", nrow(foo)/2,'\n')
  tst <- wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
                     paired=TRUE, exact=FALSE) # the test
  cat('test statistic: ', tst$statistic, 'p-value: ', tst$p.value,'\n')
  cat('effect size: ',wilcoxonPairedR(x = foo$svi, g = foo$in_out),'\n') # effect size
}

# ROTATING SVI THEME FOR ALL ADOPTIONS
for (t in 1:4){ 
  foo <- adoptions_svi %>% 
    select(RPL_THEMES_in=!!sym(paste0('RPL_THEME',t,'_in')), 
           RPL_THEMES_out=!!sym(paste0('RPL_THEME',t,'_out'))) %>%
    pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
    arrange(in_out)
  
  cat("Using theme ",svi_theme_names[[t]],'\n')
  tst <- wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
                     paired=TRUE, exact=FALSE) # the test
  cat('test statistic: ', tst$statistic, 'p-value: ', tst$p.value,'\n')
  cat('effect size: ',wilcoxonPairedR(x = foo$svi, g = foo$in_out),'\n') # effect size
}

# BY species
for (t in c('Dog','Cat')){
  foo <- adoptions_svi %>% 
    filter(species==t) %>%
    select(RPL_THEMES_in, RPL_THEMES_out) %>%
    pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
    arrange(in_out)
  
  cat("number of rows selected for", t, ":", nrow(foo)/2,'\n')
  tst <- wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
                     paired=TRUE, exact=FALSE) # the test
  cat('test statistic: ', tst$statistic, 'p-value: ', tst$p.value,'\n')
  cat('effect size: ',wilcoxonPairedR(x = foo$svi, g = foo$in_out),'\n') # effect size
}


# by age + species: fairly small differences
# cat adult 0.36 youth 0.393, dog adult 0.43 youth 0.45
for (t in c('Cat Adult','Cat Youth','Dog Adult', 'Dog Youth')){
  foo <- adoptions_svi %>% 
    mutate(age_group=case_when(
      age_group=='Unknown' ~ 'Adult',
      age_group=='Senior' ~ 'Adult',
      age_group=='Neonate' ~ 'Youth',
      age_group=='Juvenile' ~ 'Youth',
      TRUE ~ age_group
    )) %>%   mutate(age_group = paste(species, age_group)) %>% 
    filter(species %in% c('Cat','Dog')) %>%
    filter(age_group==t) %>%
    select(RPL_THEMES_in, RPL_THEMES_out) %>%
    pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
    arrange(in_out)
  
  cat("number of rows selected for", t, ":", nrow(foo)/2,'\n')
  tst <- wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
                     paired=TRUE, exact=FALSE) # the test
  cat('test statistic: ', tst$statistic, 'p-value: ', tst$p.value,'\n')
  cat('effect size: ',wilcoxonPairedR(x = foo$svi, g = foo$in_out),'\n') # effect size
}


# ROTATING SVI THEME AND INTAKE TYPE 
for (ty in c('Seized / Custody','Owner/Guardian Surrender','Stray')){
  cat('********** intype type',ty,'********** \n')
  for (t in 1:4){ 
    foo <- adoptions_svi %>% 
      mutate(src_intake_type=ifelse(src_intake_type=='Return','Owner/Guardian Surrender',src_intake_type)) %>%
      filter(src_intake_type==ty) %>%
      select(RPL_THEMES_in=!!sym(paste0('RPL_THEME',t,'_in')), 
             RPL_THEMES_out=!!sym(paste0('RPL_THEME',t,'_out'))) %>%
      pivot_longer(cols=everything(),names_to = 'in_out',values_to = 'svi') %>%
      arrange(in_out)
  cat("Using theme ",svi_theme_names[[t]],'\n')
  tst <- wilcox.test(foo$svi[foo$in_out=='RPL_THEMES_in'], foo$svi[foo$in_out=='RPL_THEMES_out'],
                     paired=TRUE, exact=FALSE) # the test
  cat('test statistic: ', tst$statistic, 'p-value: ', tst$p.value,'\n')
  cat('effect size: ',wilcoxonPairedR(x = foo$svi, g = foo$in_out),'\n') # effect size
  }
}


