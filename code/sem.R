library(lavaan)
library(dplyr)
library(psych)
library(tidyverse)
library(readxl)
library(semPlot)
library(tidySEM)
library(lavaanPlot)

rohingya.df <- read_excel('../data/rohingya.xlsx', sheet = 'Sheet1')
# View(rohingya.df)
dim(rohingya.df) # [1] 1277   42
dim(na.omit(rohingya.df)) # [1] 1043   42
summary(rohingya.df)
# Need to check missing data strategy: Up to 109 cells missing on one variable
# possibly following a systematic pattern. Is there a sensible value that can be
# substituted if the respondent does not have knowledge of children?

rohingya.df <- na.omit(rohingya.df)
rohingya.df <- rohingya.df %>% dplyr::select(!c(id,fear_of_missing_children))
summary(rohingya.df)
sapply(rohingya.df, sd)

hist(cor(rohingya.df))

##### framework domains match column name #####

# means and markers
employment <- c('sex', 'number_of_jobs', 'income_from_jobs') # 
housing <- c('housing_space', 'sanitation_of_housing', 'relocation_to_bhasanchar')
education <- c('lt_12_education', 'between_12_18_education', 'gt_18_education', 
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
                      'number_of_sexual_harassment', 'fear_of_children_safety',
                      'knowledge_of_missing_children',
                      'intention_to_leave', 'fear_of_leaving_again', 
                      'feeling_about_future')
# foundation
rights.and.citizenship <- c('rights_in_home', 'return_to_home', 
                            'repatriation_in_home')

loading.vars <- c('number_of_jobs', 'income_from_jobs',
                  'housing_space', 'sanitation_of_housing', 'relocation_to_bhasanchar',
                  'lt_12_education', 'between_12_18_education', 'gt_18_education', 
                  'quality_of_education',
                  'number_of_healthcare_facilities', 'quality_of_healthcare', 
                  'psychological_healthcare',
                  'help_from_ngo', 'trust_on_law_enforcement', 'trust_on_ngo',
                  'friendship_with_host', 'possibility_of_friendship_with_host',
                  'bond_with_neighbors', 'bond_with_majhis', 'bond_with_imams', 
                  'number_of_friends', 'bond_with_rohingyas_outside',
                  'cultural_mixability', 'number_of_religious_facilities',
                  'stress', 'improvement_in_6mos', 
                  'discussion_about_violent_groups', 'number_of_violence', 
                  'number_of_sexual_harassment', 'fear_of_children_safety',
                  'intention_to_leave', 'fear_of_leaving_again', 
                  'feeling_about_future',
                  'rights_in_home', 'repatriation_in_home')

#*************************************#
##### Exploratory factor analysis #####
#*************************************#
efa <- function(variables){
  efa.df <- rohingya.df %>% select(all_of(variables))
  paranOut <- paran::paran(x=efa.df, graph=TRUE) # How many factors?
  # Adjusted eigenvalues > 1 indicate dimensions to retain.
  # (7 components retained)
  
  prinOut <- psych::principal(efa.df, nfactor=paranOut$Retained, rotate = "promax")
  prinOut$loadings
  
  # Here's a little code to blank out small loadings
  loadDF <- data.frame(matrix(as.numeric(prinOut$loadings), 
                              attributes(prinOut$loadings)$dim, 
                              dimnames=attributes(prinOut$loadings)$dimnames))
  loadDF <- round(loadDF, 2)
  strOut <- apply(loadDF, MARGIN=c(1,2), function(x) {if (abs(x)<.4) {''} else {x}})
  knitr::kable(strOut)
}

efa(employment)

#**************************************#
####  Structural equation modeling  ####
#**************************************#
cfa.and.plot <- function(description){
  model.fit <- lavaan::cfa(model = description, data = rohingya.df)
  lavaan::summary(model.fit, standardized=TRUE)
  lavaanPlot(model = model.fit, coefs=TRUE, covs=TRUE)
}

safety.stability.model <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups + number_of_violence + number_of_sexual_harassment + feeling_about_future
'
cfa.and.plot(safety.stability.model)  # This worked out as expected

housing.model <- '
  housing =~ housing_space + sanitation_of_housing + relocation_to_bhasanchar
'
cfa.and.plot(housing.model) # This worked out as expected

means.markers.1.model <- '
  # a bit more complex model comnbining housing and employment
  employment =~ income_from_jobs + number_of_jobs
  housing =~ housing_space + sanitation_of_housing + relocation_to_bhasanchar
'
cfa.and.plot(means.markers.1.model) # lavaan WARNING: some estimated ov variances are negative

social.links.model <- '
  social.links =~ trust_on_law_enforcement + trust_on_ngo + help_from_ngo
'
cfa.and.plot(social.links.model)  # This worked out as expected

social.bridges.model <- '
  # this model is a bit more complex than the previous one by combining social links, bridges, and bonds
  social.links =~ trust_on_law_enforcement + trust_on_ngo + help_from_ngo
  social.bridges =~ friendship_with_host + possibility_of_friendship_with_host
  social.bonds =~ bond_with_majhis + bond_with_neighbors
'
cfa.and.plot(social.bridges.model)  # lavaan WARNING: some estimated lv variances are negative

# I am confused about these following simple models
education.model <- '
  education =~ between_12_18_education + gt_18_education + lt_12_education + quality_of_education
  # I added the indicators from both RC1 and RC2 together,
  # did not get any error, but between_12_18_education had a p-value>0.05
  # so, tried removing the first element from the measurement, and
  # in that case, the model cannot be identified
'
cfa.and.plot(education.model)

health.model <- '
  health =~ psychological_healthcare + quality_of_healthcare + number_of_healthcare_facilities
'
cfa.and.plot(health.model)  # lavaan WARNING: some estimated lv variances are negative

# Anything with cross loadings should be given to the scale with the
# biggest loading, but if the two values are close, then the item should
# probably be dropped because it is going to mess up your measurement model.
# Items with negative weights should be checked to make sure they are sensible.
# You may need to reverse code them, but you should keep some documentation
# of all decisions like that.



# # Don't try to do any SEM or CFA until you've gotten a feel for the 
# # internal structure of the data.
# model.desc <- '
# # measurement model
# 
#   # ***   DOMAINS   ***
#   employment  =~ number_of_jobs + income_from_jobs
#   
#   housing     =~ housing_space + sanitation_of_housing
#               # + relocation_to_bhasanchar
#   education   =~ quality_of_education + lt_12_education + between_12_18_education + gt_18_education
#   health      =~ psychological_healthcare + number_of_healthcare_facilities + quality_of_healthcare
# 
#   social_links    =~ help_from_ngo + trust_on_ngo + trust_on_law_enforcement
#   social_bridges  =~ possibility_of_friendship_with_host + friendship_with_host + marriage_to_host
#   social_bonds    =~ bond_with_rohingyas_outside + bond_with_neighbors + bond_with_majhis
#                   # + number_of_friends + bond_with_imams
# 
#   language_and_cultural_knowledge =~ number_of_religious_facilities + removal_of_religious_barriers + cultural_mixability
#   safety_and_stability            =~ intention_to_leave + improvement_in_6mos + feeling_about_future + number_of_violence + fear_of_children_safety
#                                   # + stress + knowledge_of_missing_children + number_of_sexual_harassment + fear_of_leaving_again + discussion_about_violent_groups
# 
#   rights_and_citizenship  =~ repatriation_in_home + return_to_home + rights_in_home
# 
#   # ***   THEMES   ***
#   markers_and_means   =~ education + employment + housing + health
#   # social_connections  =~ social_bonds + social_bridges + social_links
#   # facilitators        =~ language_and_cultural_knowledge + safety_and_stability
#   # foundation          =~ rights_and_citizenship
#   # integration         =~ markers_and_means + social_connections + facilitators + foundation
# 
# 
#   # regressions
#   
# 
#   # residual correlations
#   
# '
# 
# model.fit <- lavaan::sem(model = model.desc, data = rohingya.df)
# lavaan::summary(model.fit, standardized=TRUE)
# graph_sem(model.fit)