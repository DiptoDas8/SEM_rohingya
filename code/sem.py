import semopy
from semopy import Model
import pandas as pd
from pprint import pprint

data = pd.read_excel('../data/rohingya.xlsx')
print(data.columns)

desc = '''

# measurement model
employment  =~ sex + number_of_jobs + income_from_jobs
housing     =~ housing_space + sanitation_of_housing + relocation_to_bhasanchar
education   =~ lt_12_education + 12_18_education + gt_18_education + quality_of_education
health      =~ number_of_healthcare_facilities + quality_of_healthcare + psychological_healthcare

social_links    =~ help_from_ngo + trust_on_law_enforcement + trust_on_ngo
social_bridges  =~ friendship_with_host + possibility_of_friendship_with_host + marriage_to_host
social_bonds    =~ bond_with_neighbors + bond_with_majhis + bond_with_imams + number_of_friends + bond_with_rohingyas_outside

language_and_cultural_knowledge =~ cultural_mixability + removal_of_religious_barriers + number_of_religious_facilities
safety_and_stability            =~ stress + improvement_in_6mos + discussion_about_violent_groups + number_of_violence + number_of_sexual_harrassment + fear_of_children_safety + fear_of_missing_children + knowledge_of_missing_children + intention_to_leave + fear_of_leaving_again + feeling_about_future

rights_and_citizenship  =~ rights_in_home + return_to_home + repatriation_in_home


# regressions
markers_and_means   =~ employment + housing + education + health
social_connections  =~ social_links + social_bridges + social_bonds
facilitators        =~ language_and_cultural_knowledge + safety_and_stability
foundation          =~ rights_and_citizenship

# residual correlations
number_of_jobs  ~~ sex

'''

mod = Model(desc)
res = mod.fit(data)

# ins = mod.inspect()
# pprint(ins)

g = semopy.semplot(mod, '../plots/all.png')
