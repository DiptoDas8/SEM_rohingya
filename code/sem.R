library(lavaan)
library(tidyverse)
library(psych)
library(readxl)
library(semPlot)
library(tidySEM)

rohingya.df <- read_excel('../data/rohingya.xlsx', sheet = 'Sheet1')
# View(rohingya.df)
rohingya.df <- na.omit(rohingya.df)
rohingya.df <- rohingya.df %>% select(!id)

model.desc <- '
# measurement model

  # ***   DOMAINS   ***
  employment  =~ number_of_jobs + income_from_jobs
              # + sex
  housing     =~ housing_space + sanitation_of_housing
              # + relocation_to_bhasanchar
  education   =~ quality_of_education + lt_12_education + between_12_18_education + gt_18_education
  health      =~ psychological_healthcare + number_of_healthcare_facilities + quality_of_healthcare
  
  social_links    =~ help_from_ngo + trust_on_ngo + trust_on_law_enforcement
  social_bridges  =~ possibility_of_friendship_with_host + friendship_with_host + marriage_to_host
  social_bonds    =~ bond_with_rohingyas_outside + bond_with_neighbors + bond_with_majhis
                  # + number_of_friends + bond_with_imams
  
  language_and_cultural_knowledge =~ number_of_religious_facilities + removal_of_religious_barriers + cultural_mixability 
  safety_and_stability            =~ intention_to_leave + improvement_in_6mos + feeling_about_future + number_of_violence + fear_of_children_safety 
                                  # + stress + knowledge_of_missing_children + number_of_sexual_harassment + fear_of_leaving_again + discussion_about_violent_groups
  
  rights_and_citizenship  =~ repatriation_in_home + return_to_home + rights_in_home
  
  # ***   THEMES   ***
  markers_and_means   =~ education + employment + housing + health
  social_connections  =~ social_bonds + social_bridges + social_links
  facilitators        =~ language_and_cultural_knowledge + safety_and_stability
  foundation          =~ rights_and_citizenship  
  integration         =~ markers_and_means + social_connections + facilitators + foundation
  
  
  # regressions
  number_of_jobs  ~ sex    # should we use it?
  
  # residual correlations
  removal_of_religious_barriers   ~~ number_of_religious_facilities
'

model.fit <- lavaan::sem(model = model.desc, data = rohingya.df)
lavaan::summary(model.fit, standardized=TRUE)
graph_sem(model.fit)