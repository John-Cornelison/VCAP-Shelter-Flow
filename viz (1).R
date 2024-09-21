# make figures for flow analysis - one place
# adoptions_svi and non_adoptions are the two DFs prepared in advance

library(tidyverse)
library(ggalluvial)
library(kableExtra)

# read files
sh <- 'joint' # shelter name as used in file names
adoptions_svi <- readRDS(paste0('data/',sh,'/',sh,'_adoptions_svi_final.rds'))
non_adoptions <- readRDS(paste0('data/',sh,'/',sh,'_non_adoptions_final.rds'))

# which svi file used and whether viz quartiles or quintile
svi_file <- readRDS('data/joint_svi.rds') # all svi files together
shelter_name <- 'All Shelters'
how_many <- 'Quintile'

# verify correct files are loaded
table(adoptions_svi$hass_shelter_id)
table(non_adoptions$hass_shelter_id)
nrow(svi_file)

# make id column unique in joint file
adoptions_svi$id <- paste0(adoptions_svi$hass_shelter_id, '_', adoptions_svi$id)
non_adoptions$id <- paste0(non_adoptions$hass_shelter_id, '_', non_adoptions$id)

# turn data to quintiles or quartile
numToQuintile <- function(x,n=5){
  if(n==5){
    return (
      case_when(
        x>=0 & x<=0.2 ~ 1,
        x>0.2 & x<=0.4 ~ 2,
        x>0.4 & x<=0.6 ~ 3,
        x>0.6 & x<=0.8 ~ 4,
        x>0.8 & x<=1 ~ 5
      ))
  }
  if(n==4){
    return (
      case_when(
        x>=0 & x<=0.25 ~ 1,
        x>0.25 & x<=0.5 ~ 2,
        x>0.5 & x<=0.75 ~ 3,
        x>0.75 & x<=1 ~ 4
      ))
  }
}

# svi names to theme
svi_theme_names <- c(
  'RPL_THEME1' = 'Socioeconomic Status',
  'RPL_THEME2' = 'Household Characteristics',
  'RPL_THEME3' = 'Racial & Ethnic Minority Status',
  'RPL_THEME4' = 'Housing Type & Transportation',
  'RPL_THEMES' = 'SVI Overall Ranking'
)

# County SVI Breakdown
svi_file %>%
  filter(GEOID %in% unique(adoptions_svi$intake_GEOID) | GEOID %in% unique(adoptions_svi$outcome_GEOID)) %>% #nrow()  
  mutate(across(starts_with('RPL'), function(x) factor(numToQuintile(x,ifelse(how_many=='Quartile',4,5))))) %>%  # SVIs to Qrtiles
  select(GEOID, starts_with('RPL')) %>%
  pivot_longer(-GEOID, names_to = 'svi_group', values_to = 'quartile') %>%
  mutate(svi_group = svi_theme_names[svi_group]) %>%
  ggplot(aes(x=svi_group, fill=quartile)) +
  geom_bar(position = "fill") +
  labs(y='Percent', x='SVI Theme', fill=how_many, 
       title=sprintf('Breakdown of %s Counties SVI %s Rankings',shelter_name,how_many))+
  coord_flip()+
  scale_fill_viridis_d(option = "plasma", direction = 1, alpha = 0.6) +
  theme(axis.text.y = element_text(size=12))+
  #theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent) +
  geom_text(
    aes(label=paste0(round(100*signif(after_stat(count) / tapply(after_stat(count), after_stat(x), sum)[as.character(after_stat(x))], digits=3),1),'%')),
    stat="count",
    position=position_fill(vjust=0.5)) 


# correlation between all SVIs
round(cor(svi_file %>%
            filter(RPL_THEMES!=-999) %>%
            filter(GEOID %in% unique(adoptions_svi$intake_GEOID) | GEOID %in% unique(adoptions_svi$outcome_GEOID)) %>% #nrow()  
            select(starts_with('RPL_THEME')),use='complete.obs'),2) %>%
  `[<-` (lower.tri(.), NA) %>%
  data.frame(Var1=row.names(.)) %>% 
  pivot_longer(-Var1, names_to = 'Var2') %>%
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 12, hjust = 1))+
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3.5) +
  scale_x_discrete(labels= ~svi_theme_names[.])+
  scale_y_discrete(labels=~svi_theme_names[.])+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

# FLOW OF ADOPTIONS - IN SVI <-> OUT SVI ALL TOGETHER
adoptions_svi %>%
  mutate(across(starts_with('RPL'), function(x) numToQuintile(x,ifelse(how_many=='Quartile',4,5)))) %>%  # SVIs to Qrtiles
  count(RPL_THEMES_in, RPL_THEMES_out) %>%
  mutate(RPL_THEMES_in=factor(RPL_THEMES_in)) %>% #, labels = c("1", "2", "3", "4","5"))) %>%
  mutate(RPL_THEMES_out=factor(RPL_THEMES_out)) %>% # , labels = c("1", "2", "3", "4","5"))) %>%
  # viz
  ggplot(aes(axis1 = RPL_THEMES_in, axis2 = RPL_THEMES_out, y = n)) +
  geom_alluvium(aes(fill = RPL_THEMES_in) , curve_type = "quintic") +
  geom_stratum(alpha=0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), nudge_x=-0.04) +
  scale_x_discrete(expand = c(.1, .1), limits = c(sprintf("Intake SVI %s",how_many), sprintf("Outcome SVI %s",how_many)))+
  theme(axis.text.x = element_text(size=14))+
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  scale_y_continuous(labels = NULL, breaks = NULL) + labs(y = NULL)+
  geom_text(stat = "stratum", aes(label = paste0('(',scales::percent(after_stat(prop),accuracy = .1),')')),
            nudge_x = 0.03)+
  theme(legend.position = "none")

# ALL ADOPTIONS - SVI SUBTYPES 
adoptions_svi %>%
  mutate(across(starts_with('RPL'), function(x) numToQuintile(x,ifelse(how_many=='Quartile',4,5)))) %>%  # SVIs to Qrtiles
  select(id, matches('RPL_THEME[1-4].*')) %>%
  pivot_longer(-id, names_to=c('which_svi','in_out'),names_pattern = '(.*)_(.*)',
               values_to = 'quartile') %>%
  pivot_wider(id_cols=c(id, which_svi),
              names_from = 'in_out', values_from = 'quartile') %>%
  count(which_svi, RPL_THEMES_in=`in`,RPL_THEMES_out=out) %>% # confirm it works filter(which_svi=='RPL_THEME1') %>% pull(n) %>% sum()
  mutate(which_svi = svi_theme_names[which_svi]) %>%
  mutate(RPL_THEMES_in=factor(RPL_THEMES_in)) %>% #, labels = c("1", "2", "3", "4"))) %>%
  mutate(RPL_THEMES_out=factor(RPL_THEMES_out)) %>% # , labels = c("1", "2", "3", "4"))) %>%
  # viz
  ggplot(aes(axis1 = RPL_THEMES_in, axis2 = RPL_THEMES_out, y = n)) +
  geom_alluvium(aes(fill = RPL_THEMES_in) , curve_type = "quintic") +
  geom_stratum(alpha=0.3) +
  facet_wrap(~which_svi, scales='free_y')+   # FACET
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),nudge_x=-0.05) +
  geom_text(stat = "stratum", aes(label = paste0('(',scales::percent(after_stat(prop),accuracy = .1),')')),
            nudge_x = 0.04)+
  scale_x_discrete(expand = c(.1, .1), limits = c(sprintf("Intake SVI %s",how_many), sprintf("Outcome SVI %s",how_many)))+
  theme(axis.text.x = element_text(size=14))+
  theme(strip.text = element_text(size=14))+
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  scale_y_continuous(labels = NULL, breaks = NULL) + labs(y = NULL)+
  theme(legend.position = "none")

### flow FACETS
# 1. species (seems like no difference)
# 2. src_intake_type after filtering (interesting - animal control higher 3+4 and largest source)
# 3. dog size (don't see anything there)
# 4. both species and intake for sue requets

# currently set to show by intake type - no need for all the commented out code 
adoptions_svi %>%
  #mutate(src_intake_type=ifelse(src_intake_type=='Return','Owner/Guardian Surrender',src_intake_type)) %>%
  #filter(src_intake_type %in% c('Owner/Guardian Surrender','Stray','Seized / Custody')) %>%
  #  ***intake plus species combo for sue
  #mutate(src_intake_type=paste(species,src_intake_type)) %>%
  #  ***dog size filters
  #filter(species=='Dog') %>% mutate(size_group=ifelse(size_group=='Extra Large','Large',size_group)) %>% 
  #filter(size_group!="") %>%
  #  age 
  # mutate(age_group=case_when(
  #   age_group=='Unknown' ~ 'Adult',
  #   age_group=='Senior' ~ 'Adult',
  #   age_group=='Neonate' ~ 'Youth',
  #   age_group=='Juvenile' ~ 'Youth',
  #   TRUE ~ age_group
  # )) %>% 
  #filter(age_group=='Adult') %>% # for the figure of size without Youths
  #mutate(age_group = paste(species, age_group)) %>% #count(age_group)
  mutate(across(starts_with('RPL'), function(x) numToQuintile(x,ifelse(how_many=='Quartile',4,5)))) %>%  # SVIs to Qrtiles
  # ADD N TO FACET AND CHOOSE FACET VARIABLE (twice)
  group_by(src_intake_type) %>% mutate(n_group=n()) %>% mutate(src_intake_type=paste0(src_intake_type,' (n = ',formatC(n_group, big.mark = ','),')')) %>% ungroup() %>%
  count(src_intake_type, RPL_THEMES_in, RPL_THEMES_out) %>%
  mutate(RPL_THEMES_in=factor(RPL_THEMES_in)) %>% #, labels = c("1", "2", "3", "4","5"))) %>%
  mutate(RPL_THEMES_out=factor(RPL_THEMES_out)) %>% #, labels = c("1", "2", "3", "4","5"))) %>%
  # viz
  ggplot(aes(axis1 = RPL_THEMES_in, axis2 = RPL_THEMES_out, y = n)) +
  geom_alluvium(aes(fill = RPL_THEMES_in) , curve_type = "quintic") +
  geom_stratum(alpha=0.3) +
  # CHOOSE FACET HERE TOO
  facet_wrap(~src_intake_type, scales='free_y')+   # FACET
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),nudge_x=-0.05) +
  geom_text(stat = "stratum", aes(label = paste0('(',scales::percent(after_stat(prop),accuracy = .1),')')),
            nudge_x = 0.04)+
  scale_x_discrete(expand = c(.1, .1), limits = c(sprintf("Intake SVI %s",how_many), sprintf("Outcome SVI %s",how_many)))+
  theme(axis.text.x = element_text(size=14))+
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  scale_y_continuous(labels = NULL, breaks = NULL) + labs(y = NULL)+
  theme(strip.text = element_text(size=12))+
  theme(legend.position = "none")


# AGE SPECIES flow FACETS
adoptions_svi %>%
  #  age 
  mutate(age_group=case_when(
    age_group=='Unknown' ~ 'Adult',
    age_group=='Senior' ~ 'Adult',
    age_group=='Neonate' ~ 'Youth',
    age_group=='Juvenile' ~ 'Youth',
    TRUE ~ age_group
  )) %>% 
  mutate(age_group = paste(species, age_group)) %>% #count(age_group)
  mutate(across(starts_with('RPL'), function(x) numToQuintile(x,ifelse(how_many=='Quartile',4,5)))) %>%  # SVIs to Qrtiles
  # ADD N TO FACET AND CHOOSE FACET VARIABLE (paste twice)
  group_by(age_group) %>% mutate(n_group=n()) %>% mutate(age_group=paste0(age_group,' (n = ',formatC(n_group, big.mark = ','),')')) %>% ungroup() %>%
  count(age_group, RPL_THEMES_in, RPL_THEMES_out) %>%
  mutate(RPL_THEMES_in=factor(RPL_THEMES_in)) %>% #, labels = c("1", "2", "3", "4","5"))) %>%
  mutate(RPL_THEMES_out=factor(RPL_THEMES_out)) %>% #, labels = c("1", "2", "3", "4","5"))) %>%
  # viz
  ggplot(aes(axis1 = RPL_THEMES_in, axis2 = RPL_THEMES_out, y = n)) +
  geom_alluvium(aes(fill = RPL_THEMES_in) , curve_type = "quintic") +
  geom_stratum(alpha=0.3) +
  # CHOOSE FACET HERE TOO
  facet_wrap(~age_group, scales='free_y')+   # FACET
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),nudge_x=-0.05) +
  geom_text(stat = "stratum", aes(label = paste0('(',scales::percent(after_stat(prop),accuracy = .1),')')),
            nudge_x = 0.04)+
  scale_x_discrete(expand = c(.1, .1), limits = c(sprintf("Intake SVI %s",how_many), sprintf("Outcome SVI %s",how_many)))+
  theme(axis.text.x = element_text(size=14))+
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  scale_y_continuous(labels = NULL, breaks = NULL) + labs(y = NULL)+
  theme(strip.text = element_text(size=12))+
  theme(legend.position = "none")

# age species table
adoptions_svi %>%
  select(age_group, species, src_intake_type) %>% rbind(non_adoptions %>% filter(species %in% c('Cat','Dog')) %>% select(age_group, species, src_intake_type)) %>%
  #  age 
  mutate(age_group=case_when(
    age_group=='Unknown' ~ 'Adult',
    age_group=='Senior' ~ 'Adult',
    age_group=='Neonate' ~ 'Youth',
    age_group=='Juvenile' ~ 'Youth',
    TRUE ~ age_group
  )) %>% 
  mutate(age_group = paste(species, age_group)) %>% 
  count(age_group, src_intake_type) %>%
  pivot_wider(names_from = 'src_intake_type', values_from = 'n') %>%
  mutate(total=`OS / Returns` + `Seized / Custody` + Stray) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total"))) %>%
  mutate(across(2:4, function(x) paste0(x,' (',round(100*x/total,1),'%)'))) %>%
  kbl() %>% kable_styling('striped')


# in SVI breakdown within intake reasons - all intakes 
adoptions_svi %>%
  select(hass_shelter_id,id,RPL_THEMES=RPL_THEMES_in, reason_short, intake_type) %>%
  #rbind(non_adoptions %>% select(hass_shelter_id,id,RPL_THEMES, reason_short, intake_type)) %>%
  filter(intake_type %in% c('OS','Returns')) %>% #count(reason_short)
  filter(hass_shelter_id!='PVAS' & hass_shelter_id!='Fresno') %>%
  mutate(across(starts_with('RPL'), function(x) factor(numToQuintile(x,ifelse(how_many=='Quartile',4,5))))) %>%  # SVIs to Qrtiles
  # handle the others and NAS
  filter(!is.na(reason_short)) %>%
  filter(reason_short!='Other') %>%
  filter(reason_short!='Unknown') %>%
  #mutate(reason_short=ifelse(is.na(reason_short),'Other',reason_short)) %>%
  ggplot(aes(x=reorder(RPL_THEMES,desc(RPL_THEMES)), fill=reason_short)) +
  geom_bar(position = "fill") +
  #labs(y='Percent', x='Intake Reason', fill=sprintf('Intake SVI\n%s',how_many))+
  labs(y='Percent', x='SVI Quintile', fill='Intake Reason')+
  coord_flip()+
  scale_fill_viridis_d(option = "plasma", direction = 1, alpha = 0.6) +
  theme(axis.text.y = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent) +
  #facet_wrap(~hass_shelter_id)+
  geom_text(
    aes(label=paste0(round(100*signif(after_stat(count) / tapply(after_stat(count), ..x.., sum)[as.character(..x..)], digits=3),1),'%')),
    stat="count", size=4,
    position=position_fill(vjust=0.5)) 


#outcome type breakdown inside each IN SVI
non_adoptions %>%
  select(id, RPL_THEMES, outcome_type, intake_type) %>%
  rbind(
    adoptions_svi %>% select(id,RPL_THEMES=RPL_THEMES_in, outcome_type, intake_type)
  ) %>%
  mutate(outcome_type=ifelse(outcome_type=='Adoption','Adopted',outcome_type)) %>% # pvas
  mutate(outcome_type=ifelse(outcome_type=='Owner Requested Euthanasia','Euthanasia',outcome_type)) %>% # pvas
  filter(!(intake_type %in% c('Born In Care','Disposal / DOA'))) %>%
  filter(!(outcome_type %in% c('Service Out', 'Report','Disposal','Died in Care'))) %>%
  filter(!(outcome_type %in% c('SNR', 'TNR', 'Wildlife Out','UNMAPPED'))) %>%
  mutate(across(starts_with('RPL'), function(x) factor(numToQuintile(x,ifelse(how_many=='Quartile',4,5))))) %>%  # SVIs to Qrtiles
  filter(!is.na(RPL_THEMES)) %>%
  select(id, RPL_THEMES, outcome_type) %>%
  ggplot(aes(x=reorder(RPL_THEMES,desc(RPL_THEMES)), fill=outcome_type)) +
  geom_bar(position = "fill") +
  labs(y='Percent', x='Outcome Type', fill=sprintf('Intake SVI\n%s',how_many))+
  labs(y='Percent', x='In SVI Quintile ', fill='Outcome Type')+
  coord_flip()+
  scale_fill_viridis_d(option = "plasma", direction = 1, alpha = 0.6) +
  theme(axis.text.y = element_text(size=12))+
  theme(legend.text = element_text(size=12))+
  #theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent) +
  geom_text(
    aes(label=paste0(round(100*signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3),1),'%')),
    stat="count", size=4,
    position=position_fill(vjust=0.5))


# normalized figure - intake per group per households (/pop) and outcome
min_per_tract <- 10 # how many intake+outcomes from a tract to count it?

svi_file %>%
  # include only tracts with some intakes
 filter(GEOID %in% (c(adoptions_svi$intake_GEOID,adoptions_svi$outcome_GEOID ,non_adoptions$intake_GEOID) %>%
     table %>% data.frame() %>% filter(Freq>=min_per_tract) %>% pull(1) %>% as.character())) %>%
 filter(RPL_THEMES!=-999) %>% # should be resolved in file but just in case
 mutate(across(starts_with('RPL'), function(x) factor(numToQuintile(x,ifelse(how_many=='Quartile',4,5))))) %>%
  select(GEOID, RPL_THEMES, hh=E_HH) %>%
 left_join(adoptions_svi %>% select(GEOID=intake_GEOID) %>% 
             rbind(non_adoptions %>% select(GEOID=intake_GEOID)) %>%
             count(GEOID), by='GEOID') %>% # join in 
 left_join(adoptions_svi %>% count(GEOID=outcome_GEOID), by='GEOID', suffix=c('_in','_out')) %>%
 replace(is.na(.),0) %>% # if no intake or outcome insert 0
 group_by(RPL_THEMES) %>% 
 summarize(total_hh=sum(hh), sum_in=sum(n_in), sum_out=sum(n_out)) %>%
 mutate(in_per_k = (sum_in)/total_hh*1000, out_per_k=sum_out/total_hh*1000) %>%
  # viz - barplot
  select(RPL_THEMES, Intakes=in_per_k, Adoptions=out_per_k) %>%
  pivot_longer(-RPL_THEMES, names_to = 'variable',values_to = 'value') %>%
  ggplot(aes(x=reorder(RPL_THEMES,desc(RPL_THEMES)), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip()+
  labs(x='SVI Quintile', y='Animals per 1000 Households', fill='') +
  geom_text(
    aes(label=round(value)),
    size=4, position=position_dodge(.9),
    hjust=1.25)+
  theme(text = element_text(size=14))

# same figure but now faceted by intake type
svi_file %>%
  # excluded  temporarily
  filter(GEOID %in% (c(adoptions_svi$intake_GEOID,adoptions_svi$outcome_GEOID,non_adoptions$intake_GEOID) %>%
      table %>% data.frame() %>% filter(Freq>=min_per_tract) %>% pull(1) %>% as.character())) %>%
  filter(RPL_THEMES!=-999) %>% # should be resolved in file but just in case
  mutate(across(starts_with('RPL'), function(x) factor(numToQuintile(x,ifelse(how_many=='Quartile',4,5))))) %>%
  select(GEOID, RPL_THEMES, hh=E_HH) %>%
  group_by(RPL_THEMES) %>% mutate(total_hh = sum(hh)) %>% ungroup() %>%
  # join intakes
  left_join(adoptions_svi %>%  
              select(GEOID=intake_GEOID, src_intake_type,species) %>%
              rbind(non_adoptions %>% select(GEOID=intake_GEOID, src_intake_type,species)) %>%
              #mutate(src_intake_type=ifelse(src_intake_type=='Return','Owner/Guardian Surrender',src_intake_type)) %>%
              #mutate(src_intake_type=paste(species,src_intake_type)) %>% # for intakes+species
              #filter(src_intake_type %in% c('Owner/Guardian Surrender','Stray','Seized / Custody')) %>%
              count(GEOID, src_intake_type) %>% 
              pivot_wider(names_from='src_intake_type',values_from='n'),
              by='GEOID') %>% # join in 
  # join outcome SVI per intake type
  left_join(adoptions_svi %>% 
              #mutate(src_intake_type=ifelse(src_intake_type=='Return','Owner/Guardian Surrender',src_intake_type)) %>%
              #mutate(src_intake_type=paste(species,src_intake_type)) %>% # for intakes+species
              #filter(src_intake_type %in% c('Owner/Guardian Surrender','Stray','Seized / Custody')) %>%
              count(GEOID=outcome_GEOID, src_intake_type) %>%
              pivot_wider(names_from='src_intake_type',values_from='n'),
            by=c('GEOID'), suffix=c('_in','_out')) %>%
  replace(is.na(.),0) %>% # if no intake or outcome insert 0
  pivot_longer(-c(GEOID,RPL_THEMES,hh,total_hh), names_to=c('src_intake_type','in_out'), 
              names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = 'in_out', values_from = 'value') %>%
  group_by(RPL_THEMES, total_hh, src_intake_type) %>% 
  summarize(sum_in=sum(`in`), sum_out=sum(out)) %>%
  mutate(in_per_k = (sum_in)/total_hh*1000, out_per_k = (sum_out)/total_hh*1000) %>%
  ungroup() %>%
  # viz - barplot - FILTERING OUTCOMES BELOW
  select(src_intake_type, RPL_THEMES, Intakes=in_per_k, Adoptions=out_per_k) %>%
  #filter(src_intake_type %in% c('Owner/Guardian Surrender','Stray','Seized / Custody')) %>%
  #filter(src_intake_type %in% c('Cat Owner/Guardian Surrender', 'Dog Owner/Guardian Surrender','Cat Stray','Dog Stray', 'Cat Seized / Custody', 'Dog Seized / Custody')) %>%
  #filter(src_intake_type %in% c('Dog','Cat')) %>% # for by species figure (need to edit above)
  pivot_longer(-c(src_intake_type,RPL_THEMES), names_to = 'variable',values_to = 'value') %>%
  #mutate(RPL_THEMES=ifelse(RPL_THEMES==1,'1 (least vulnerable)',ifelse(RPL_THEMES==5,'5 (most vulnerable)',RPL_THEMES))) %>%
  ggplot(aes(x=reorder(RPL_THEMES,desc(RPL_THEMES)), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~src_intake_type,nrow = 3)+
  coord_flip()+
  labs(x='SVI Quintile', y='Animals per 1000 Households', fill='') +
  #geom_text(aes(label=round(value,0), hjust=ifelse(value==max(value),1.25,-0.25)),
  #  size=3.5, position=position_dodge(.9))+
  theme(text = element_text(size=14))


# table of counts by shelter and proportion
adoptions_svi %>% 
  count(hass_shelter_id, src_intake_type) %>%
  pivot_wider(names_from = 'src_intake_type', values_from = 'n') %>%
  mutate(hass_shelter_id=paste0('Shelter ',1:7)) %>%
  mutate(Total = `OS / Returns`+`Seized / Custody`+Stray) %>%
  mutate(perc = paste0(round(100*Total/sum(Total),1),'%')) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total"))) %>%
  mutate(Total = paste0(Total,' (',perc,')')) %>% select(-perc) %>%
  select(hass_shelter_id,Stray,`OS / Returns`,`Seized / Custody`,Total) %>%
  # join all intakes table
  left_join(non_adoptions %>% select(hass_shelter_id, src_intake_type) %>%
              rbind(adoptions_svi %>% select(hass_shelter_id, src_intake_type)) %>%
              count(hass_shelter_id, src_intake_type) %>%
              pivot_wider(names_from = 'src_intake_type', values_from = 'n') %>%
              mutate(hass_shelter_id=paste0('Shelter ',1:7)) %>%
              mutate(Total = `OS / Returns`+`Seized / Custody`+Stray) %>%
              mutate(perc = paste0(round(100*Total/sum(Total),1),'%')) %>%
              bind_rows(summarise(.,
                                  across(where(is.numeric), sum),
                                  across(where(is.character), ~"Total"))) %>%
              mutate(Total = paste0(Total,' (',perc,')')) %>% select(-perc) %>%
              select(hass_shelter_id,Stray,`OS / Returns`,`Seized / Custody`,Total),
            by='hass_shelter_id', suffix=c(' ','  ')) %>%
  rename(Shelter=hass_shelter_id) %>%
  kbl() %>% kable_styling('striped') %>%
  add_header_above(c(' '=1,'Animals Adopted'=4, 'All Intakes'=4))


