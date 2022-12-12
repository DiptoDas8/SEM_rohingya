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

model.kitchensink <- '
        #This is a kitchen sink model
        
        #measurement model
        outcome =~sex+number_of_jobs+income_from_jobs+\
        housing_space+sanitation_of_housing+relocation_to_bhasanchar+\
        lt_12_education+between_12_18_education+gt_18_education+quality_of_education+\
        number_of_healthcare_facilities+quality_of_healthcare+psychological_healthcare+\
        
        help_from_ngo+trust_on_law_enforcement+trust_on_ngo+\
        friendship_with_host+possibility_of_friendship_with_host+marriage_to_host+\
        bond_with_neighbors+bond_with_majhis+bond_with_imams+number_of_friends+bond_with_rohingyas_outside+\
        
        cultural_mixability+removal_of_religious_barriers+number_of_religious_facilities+\
        stress+improvement_in_6mos+discussion_about_violent_groups+number_of_violence+number_of_sexual_harassment+fear_of_children_safety+knowledge_of_missing_children+intention_to_leave+fear_of_leaving_again+feeling_about_future
        
        #regressions
        
        #residual correlations
'
fit.kitchensink <- sem(model.kitchensink, data = rohingya.df)
summary(fit.kitchensink, standardize=TRUE)
fitmeasures(fit.kitchensink, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))

model.highloadings <- '
        #This is a model with factors with loadings over threshold
        
        #measurement model
        outcome =~number_of_jobs+income_from_jobs+\
        housing_space+\
        lt_12_education+between_12_18_education+gt_18_education+quality_of_education+\
        number_of_healthcare_facilities+quality_of_healthcare+psychological_healthcare+\
        
        trust_on_law_enforcement+trust_on_ngo+\
        friendship_with_host+possibility_of_friendship_with_host+\
        bond_with_neighbors+bond_with_majhis+bond_with_imams+number_of_friends+bond_with_rohingyas_outside+\
        
        cultural_mixability+number_of_religious_facilities+\
        stress+improvement_in_6mos+discussion_about_violent_groups+number_of_sexual_harassment+intention_to_leave+fear_of_leaving_again+feeling_about_future
        
        #regressions
        
        #residual correlations
'
fit.highloadings <- sem(model.highloadings, data=rohingya.df)
summary(fit.highloadings, standardize=TRUE)
fitmeasures(fit.highloadings, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))

model.firstcomp <- '
        #This is a model with factors in the first pricipal component
        
        #measurement model
        outcome =~number_of_jobs+income_from_jobs+\
        housing_space+\
        between_12_18_education+gt_18_education+\
        quality_of_healthcare+psychological_healthcare+\
        
        trust_on_law_enforcement+trust_on_ngo+\
        friendship_with_host+possibility_of_friendship_with_host+\
        bond_with_neighbors+bond_with_majhis+\
        
        cultural_mixability+number_of_religious_facilities+\
        improvement_in_6mos+discussion_about_violent_groups+number_of_sexual_harassment+feeling_about_future
        
        #regressions
        
        #residual correlations
'
fit.firstcomp <- sem(model.firstcomp, data=rohingya.df)
summary(fit.firstcomp, standardize=TRUE)
fitmeasures(fit.firstcomp, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))

model.domain<-'
        #This is a model with factors of first pricipal component under DOMAINS
        
        #measurement model
        employment=~number_of_jobs+income_from_jobs
        housing=~housing_space
        education=~between_12_18_education+gt_18_education
        health=~quality_of_healthcare+psychological_healthcare
        
        social_link=~trust_on_law_enforcement+trust_on_ngo
        social_bridge=~friendship_with_host+possibility_of_friendship_with_host
        social_bond=~bond_with_neighbors+bond_with_majhis
        
        languge_cultural_mixability=~cultural_mixability+number_of_religious_facilities
        safety_and_stability=~improvement_in_6mos+discussion_about_violent_groups+number_of_sexual_harassment+feeling_about_future
        
        #regressions
        
        #residual correlations
'
fit.domain <- sem(model.domain, data=rohingya.df)
summary(fit.domain, standardize=TRUE)
fitmeasures(fit.domain, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))

model.theme <- '
        #This is a model with factors of first pricipal component under THEMES
        
        #measurement model
        means_and_markers=~number_of_jobs+income_from_jobs+\
        housing_space+\
        between_12_18_education+gt_18_education+\
        quality_of_healthcare+psychological_healthcare
        
        social_connection=~trust_on_law_enforcement+trust_on_ngo+\
        friendship_with_host+possibility_of_friendship_with_host+\
        bond_with_neighbors+bond_with_majhis
        
        facilitator=~cultural_mixability+number_of_religious_facilities+\
        improvement_in_6mos+discussion_about_violent_groups+number_of_sexual_harassment+feeling_about_future
        
        #regressions
        
        #residual correlations
'
fit.theme <- sem(model.theme, data = rohingya.df)
summary(fit.theme, standardize=TRUE)
fitmeasures(fit.theme, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))



model.domain.tweak<-'
        #Tweak domain-based model
        
        #measurement model
        #removed number_of_jobs for low std.all in variances and low factor loading
        employment=~income_from_jobs
        
        # removed housing=~housing_space for low std.all in variances
        
        education=~between_12_18_education+gt_18_education
        health=~quality_of_healthcare+psychological_healthcare
        
        #social_link=~trust_on_ngo #removed trust_on_ngo for low factor loading
        #removing trust_on_law_enforcement because of low std.all
        
        #removing social_brige because of NA std.lv and std.all
        #social_bridge=~friendship_with_host+possibility_of_friendship_with_host
        
        social_bond=~bond_with_neighbors+bond_with_majhis
        
        languge_cultural_mixability=~number_of_religious_facilities #removing cultural_mixability because of low std.all
        
        #removing feeling_about_future for low factor loading
        safety_and_stability=~improvement_in_6mos+discussion_about_violent_groups+number_of_sexual_harassment
        
        #regressions
        
        #residual correlations
'
fit.domain.tweak <- sem(model.domain.tweak, data=rohingya.df)
summary(fit.domain.tweak, standardize=TRUE)
fitmeasures(fit.domain.tweak, c("chisq", "df", "cfi", "rmsea", "srmr", "mfi"))

lavaanPlot(name='domain_tweak', fit.domain.tweak, coefs=TRUE, covs=TRUE)
