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

# Cor matrix shows that these items don't fit together
housing <- c('housing_space', 'sanitation_of_housing', 'relocation_to_bhasanchar')

education <- c('lt_12_education', 'between_12_18_education', 'gt_18_education', 
               'quality_of_education')
advanced.education <- c('between_12_18_education', 'gt_18_education')


health <- c('number_of_healthcare_facilities', 'quality_of_healthcare', 
            'psychological_healthcare')
health.quality <- c('quality_of_healthcare', 
                    'psychological_healthcare')

markers.and.means <- c('sex', 'number_of_jobs', 'income_from_jobs', 'housing_space', 'sanitation_of_housing', 'relocation_to_bhasanchar', 'lt_12_education', 'between_12_18_education', 'gt_18_education', 'quality_of_education', 'number_of_healthcare_facilities', 'quality_of_healthcare', 'psychological_healthcare')

# social connections

social.links <- c('help_from_ngo', 'trust_on_law_enforcement', 'trust_on_ngo')
social.bridges <- c('friendship_with_host', 'possibility_of_friendship_with_host', 'marriage_to_host')
social.bonds <- c('bond_with_neighbors', 'bond_with_majhis', 'bond_with_imams', 
                  'number_of_friends', 'bond_with_rohingyas_outside')
simple.bonds <- c('bond_with_neighbors', 'bond_with_majhis')

social.connections <- c('help_from_ngo', 'trust_on_law_enforcement', 'trust_on_ngo', 'friendship_with_host', 'possibility_of_friendship_with_host', 'marriage_to_host', 'bond_with_neighbors', 'bond_with_majhis', 'bond_with_imams', 'number_of_friends', 'bond_with_rohingyas_outside')

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

facilitators <- c('cultural_mixability', 'removal_of_religious_barriers', 'number_of_religious_facilities', 'stress', 'improvement_in_6mos',  'discussion_about_violent_groups', 'number_of_violence', 'number_of_sexual_harassment', 'fear_of_children_safety', 'knowledge_of_missing_children', 'intention_to_leave', 'fear_of_leaving_again', 'feeling_about_future')

# foundation
rights.and.citizenship <- c('rights_in_home', 'return_to_home', 
                            'repatriation_in_home') # Return to home performs poorly
rights.and.repatriate <- c('rights_in_home','repatriation_in_home')

trust <- c('trust_on_ngo', 'friendship_with_host', 'possibility_of_friendship_with_host', 'trust_on_law_enforcement')

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
  print(cor(efa.df))
  paranOut <- paran::paran(x=efa.df, graph=TRUE, ) # How many factors?
  # Adjusted eigenvalues > 1 indicate dimensions to retain.
  # (7 components retained)
  
  prinOut <- psych::principal(efa.df, nfactor=paranOut$Retained, rotate = "promax")
  prinOut$loadings
  
  # Here's a little code to blank out small loadings
  loadDF <- data.frame(matrix(as.numeric(prinOut$loadings), 
                              attributes(prinOut$loadings)$dim, 
                              dimnames=attributes(prinOut$loadings)$dimnames))
  loadDF <- round(loadDF, 2)
  strOut <- apply(loadDF, MARGIN=c(1,2), function(x) {if (abs(x)<.63) {''} else {x}})
  knitr::kable(strOut)
}

efa(employment) # Sex does not fit
efa(housing) # Poor fit
efa(education) # between_12_18_education and gt_18_education OK but drop the other two
efa(advanced.education) # Good
efa(rights.and.citizenship) # Return to home performs poorly
efa(rights.and.repatriate) # Good
efa(language.cultural.knowledge) # Items don't fit together
efa(social.links) # Not ideal
efa(social.bridges) # Not ideal
efa(social.bonds)
efa(simple.bonds)
efa(health) # One bad item
efa(health.quality) # Better
efa(trust)
efa(safety.stability)

efa(markers.and.means)
efa(social.connections)
efa(facilitators)

# Anything with cross loadings should be given to the scale with the
# biggest loading, but if the two values are close, then the item should
# probably be dropped because it is going to mess up your measurement model.
# Items with negative weights should be checked to make sure they are sensible.
# You may need to reverse code them, but you should keep some documentation
# of all decisions like that.

#**************************************#
####  Structural equation modeling  ####
#**************************************#
# specify fit indices of interest
# Chi squared
# Degrees of freedom
# CFI - Comparative Fit Index (Bentler)
# Root mean square error of approximation
# standardized root mean square residual
# McDonald fit index, AKA McDonald's Centrality Index or called "Mc" by Hu and Bentler
fit.indices <- c("chisq", "df", "cfi", "rmsea", "srmr", "mfi")


cfa.and.plot <- function(description){
  model.fit <- lavaan::sem(model = description, data = rohingya.df)
  # orth.model.fit <- lavaan::sem(model = description, data = rohingya.df, orthogonal = TRUE)
  # lavaan::summary(model.fit, standardized=TRUE)
  
  print(lavaan::fitMeasures(model.fit, fit.indices))
  # print(lavaan::fitMeasures(orth.model.fit, fit.indices))
  # print(lavaan::modindices(model.fit, sort.=TRUE, free.remove = FALSE))
  # print(lavaan::lavInspect(model.fit, "cov.lv"))
  lavaanPlot(model = model.fit, coefs=TRUE, covs=TRUE)
}

safety.stability.model <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups + number_of_violence + number_of_sexual_harassment + feeling_about_future
'
cfa.and.plot(safety.stability.model)  # This worked out as expected

# Modification indices:
# 14             improvement_in_6mos ~~     number_of_sexual_harassment 20.094
# 13             improvement_in_6mos ~~              number_of_violence 18.515
# 16 discussion_about_violent_groups ~~              number_of_violence 15.944
# Suggests that number of violence should be dropped

safety.stability.model2 <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
'
cfa.and.plot(safety.stability.model2) 
# This is better:
# chisq    df   cfi rmsea  srmr   mfi 
# 6.257 2.000 0.998 0.045 0.011 0.998 

# Now let's put together safety and advanced.education:
safety.educ.model <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  educ =~ between_12_18_education +  gt_18_education 
'
cfa.and.plot(safety.educ.model) 
# Empirically underidentified because of low correlation between educ
# and safety and stability

safety.educ.rights <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  educ =~ between_12_18_education +  gt_18_education 
  rights =~ rights_in_home + repatriation_in_home
'
cfa.and.plot(safety.educ.rights) 
# Still empirically underidentified because of low correlation between educ
# and other lvs


safety.educ.rights.bonds <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  educ =~ between_12_18_education +  gt_18_education 
  rights =~ rights_in_home + repatriation_in_home
  bonds =~ bond_with_neighbors + bond_with_majhis
'
cfa.and.plot(safety.educ.rights.bonds) 

# Still no connections to educ, try dropping it
safety.rights.bonds <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  rights =~ repatriation_in_home + rights_in_home 
  bonds =~ bond_with_neighbors + bond_with_majhis
'
cfa.and.plot(safety.rights.bonds) 
# Negative variance on rights_in_home 
plot(rohingya.df$repatriation_in_home, rohingya.df$rights_in_home)
hist(rohingya.df$repatriation_in_home)
hist(rohingya.df$rights_in_home)
# Both of these are dichotomous - not going to function well with Pearson cors
table(rohingya.df$repatriation_in_home, rohingya.df$rights_in_home)
# Very unbalanced categories too

# Drop rights
safety.bonds <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  bonds =~ bond_with_neighbors + bond_with_majhis
'
cfa.and.plot(safety.bonds) 
# rmsea is a little too high, but let's proceed

# Add healthcare quality

safety.bonds.health <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  bonds =~ bond_with_neighbors + bond_with_majhis
  health =~ quality_of_healthcare + psychological_healthcare
  
  # Correlated uniqueness
  feeling_about_future ~~ bond_with_neighbors
'
cfa.and.plot(safety.bonds.health) 

# The model converges, but still does not have a very good fit
# It is good enough to proceed with, however

safety.bonds.health.trust <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  bonds =~ bond_with_neighbors + bond_with_majhis
  health =~ quality_of_healthcare + psychological_healthcare
  trust =~ trust_on_ngo + friendship_with_host
  
  # Correlated uniqueness
  feeling_about_future ~~ bond_with_neighbors
'
cfa.and.plot(safety.bonds.health.trust) 

# The model converges, but still does not have a very good fit
# There are enough latent variables here to create some alternative models
# So now you can create a structural model that predicts safety and stability

safety.bonds.health.trust.employment <- '
  safety_and_stability =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  bonds =~ bond_with_neighbors + bond_with_majhis
  health =~ quality_of_healthcare + psychological_healthcare
  trust =~ trust_on_ngo + friendship_with_host
  
  employment =~ income_from_jobs + number_of_jobs
  
  # Correlated uniqueness
  feeling_about_future ~~ bond_with_neighbors
'
cfa.and.plot(safety.bonds.health.trust.employment) 

broad.model <- '
  markers_and_means =~ income_from_jobs + number_of_jobs + quality_of_healthcare + psychological_healthcare
  facilitators =~ improvement_in_6mos + discussion_about_violent_groups  + number_of_sexual_harassment + feeling_about_future
  social_connections =~ bond_with_neighbors + bond_with_majhis + trust_on_ngo + friendship_with_host
  
  # Correlated uniqueness
  feeling_about_future ~~ bond_with_neighbors
  improvement_in_6mos ~~ psychological_healthcare
'
cfa.and.plot(broad.model)

# housing.model <- '
#   housing =~ housing_space + sanitation_of_housing + relocation_to_bhasanchar
# '
# cfa.and.plot(housing.model) # This worked out as expected
# 
# means.markers.1.model <- '
#   # a bit more complex model comnbining housing and employment
#   employment =~ income_from_jobs + number_of_jobs
#   housing =~ housing_space + sanitation_of_housing + relocation_to_bhasanchar
# '
# cfa.and.plot(means.markers.1.model) # lavaan WARNING: some estimated ov variances are negative
# 
# social.links.model <- '
#   social.links =~ trust_on_law_enforcement + trust_on_ngo + help_from_ngo
# '
# cfa.and.plot(social.links.model)  # This worked out as expected
# 
# social.bridges.model <- '
#   # this model is a bit more complex than the previous one by combining social links, bridges, and bonds
#   social.links =~ trust_on_law_enforcement + trust_on_ngo + help_from_ngo
#   social.bridges =~ friendship_with_host + possibility_of_friendship_with_host
#   # social.bonds =~ bond_with_majhis + bond_with_neighbors
#   
#   # if we have links and bridges, lavaan WARNING: some estimated ov variances are negative
#   # if we have all three components, lavaan WARNING: some estimated lv variances are negative
# '
# cfa.and.plot(social.bridges.model)
# 
# # I am confused about these following simple models
# education.model <- '
#   education =~ between_12_18_education + gt_18_education + lt_12_education + quality_of_education
#   # I added the indicators from both RC1 and RC2 together,
#   # did not get any error, but between_12_18_education had a p-value>0.05
#   # so, tried removing the first element from the measurement, and
#   # in that case, the model cannot be identified
# '
# cfa.and.plot(education.model)
# 
# health.model <- '
#   health =~ psychological_healthcare + quality_of_healthcare + number_of_healthcare_facilities
# '
# cfa.and.plot(health.model)  # lavaan WARNING: some estimated lv variances are negative
# 

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