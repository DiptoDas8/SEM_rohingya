import semopy
from semopy import Model
import pandas as pd
from pprint import pprint

dataset = pd.read_excel('../data/rohingya.xlsx')
# print(dataset.columns)
dataset.drop(columns=['lt_12_education'])
dataset.dropna()

def sem(name, data):
    desc = '''
    
    # measurement model
    
    # ***   DOMAINS   ***
    employment  =~ number_of_jobs + income_from_jobs
                # + sex
    housing     =~ housing_space + sanitation_of_housing
                # + relocation_to_bhasanchar
    education   =~ quality_of_education + lt_12_education + 12_18_education + gt_18_education
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
    
    '''

    mod = Model(desc)
    res = mod.fit(data)

    ins = mod.inspect()
    # pprint(ins)
    ins.to_excel('../result/inspect_'+name+'.xlsx', index=False)

    stat = semopy.calc_stats(mod)
    stat = stat.T
    stat.to_excel('../result/stat_'+name+'.xlsx')

    g = semopy.semplot(mod, '../plots/'+name+'.pdf')


sem('all', dataset)

# mdata = dataset.loc[dataset['sex']==1]
# sem('male', mdata)
# fdata = dataset.loc[dataset['sex']==0]
# sem('female', fdata)
