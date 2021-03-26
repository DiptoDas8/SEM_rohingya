library(dplyr)
library(tidyverse)
library(psych)
library(readxl)

rohingya.df <- read_excel('../data/rohingya.xlsx', sheet = 'Sheet1')
# View(rohingya.df)
rohingya.df <- na.omit(rohingya.df)
rohingya.df <- rohingya.df %>% select(!id)

##### framework domains match column name #####
# means and markers

employment <- c('number_of_jobs', 'income_from_jobs',) # 'sex'
housing <- c('housing_space', 'sanitation_of_housing', 'relocation_to_bhasanchar')
education <- c('lt_12_education', '12_18_education', 'gt_18_education', 
               'quality_of_education')
health <- c('number_of_healthcare_facilities', 'quality_of_healthcare', 
            'psychological_healthcare')

# social connections

social.links <- c('help_from_ngo', 'trust_on_law_enforcement', 'trust_on_ngo')
social.bridges <- c('friendship_with_host', 'possibility_of_friendship_with_host', 
                    'marriage_to_host')
social.bonds <- c('bond_with_neighbors', 'bond_with_majhis', 'bond_with_imams', 
                  'number_of_friends', 'bond_with_rohingyas_outside')

# facilitators
language.cultural.knowledge <- c('cultural_mixability', 
                                 'removal_of_religious_barriers', 
                                 'number_of_religious_facilities')
safety.stability <- c('stress', 'improvement_in_6mos', 
                      'discussion_about_violent_groups', 'number_of_violence', 
                      'number_of_sexual_harrassment', 'fear_of_children_safety',
                       'knowledge_of_missing_children',
                      'intention_to_leave', 'fear_of_leaving_again', 
                      'feeling_about_future')
# foundation
rights.and.citizenship <- c('rights_in_home', 'return_to_home', 
                            'repatriation_in_home')

efa.df <- rohingya.df %>% select(all_of(social.bridges))
# View(efa.df)
psych::pca(efa.df, nfactors = length(names(efa.df)))

# cor(rohingya.df)
