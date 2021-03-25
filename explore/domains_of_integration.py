import json
import pandas as pd

domains_of_integration_refugee_dataset = {
    'Markers and Means':{
        'employment':{
            'Sex': 'sex',
            'How satisfied are you with the number of job opportunities available to Rohingya adults in the camp?': 'number_of_jobs',
            'How satisfied are you with the income your family makes in the camp?': 'income_from_jobs',
        },
        'housing':{
            'How satisfied are you with the amount of space allocated to you and your family in the camp?': 'housing_space',
            'How satisfied are you with the level of hygiene and sanitation in the camp?': 'sanitation_of_housing',
            'Are you willing to go?': 'relocation_to_bhasanchar',   #"Are you willing to go (to Bhasan Char)?"
        },
        'education':{
            'Do you think there are enough educational opportunities for Rohingya children below the age of 12 in the camp?': 'lt_12_education',
            'Do you think there are enough educational opportunities for Rohingya adolescents (between 12 and 18 years old) in the camp?': '12_18_education',
            'Do you think there are enough educational opportunities for Rohingya adults (older than 18 years old) in the camp?': 'gt_18_education',
            'How satisfied are you with the quality of education you and your family are receiving in the camp?': 'quality_of_education'
        },
        'health':{
            'Do you think there are enough healthcare facilities for Rohingya (like aid station, health centre, hospital)?': 'number_of_healthcare_facilities',
            'How satisfied are you with the quality of healthcare you and your family are receiving in the camp?': 'quality_of_healthcare',
            'How satisfied are you with the level of psychological support you and your family are receiving in the camp?': 'psychological_healthcare',
        }
    },

    'Social Connections':{
        'social_links':{
            'How satisfied are you with the overall help you and your family have been receiving from NGOs?': 'help_from_ngo',
            'How much do you trust the police/military/guards of camps in Bangladesh?': 'trust_on_law_enforcement',
            'How much do you trust the NGOs in the camp?': 'trust_on_ngo',
        },
        'social_bridges':{
            'Do you consider any Bangladeshis as your friends?': 'friendship_with_host',
            'Do you believe Rohingya and Bangladeshis can be good friends?': 'possibility_of_friendship_with_host',
            'Do you think that there are Rohingya families who have married their daughter off to Bangladeshi boys/men?': 'marriage_to_host',
        },
        'social_bonds':{
            'How satisfied are you with the level of social support you and your family receive from neighbours and friends?': 'bond_with_neighbors',
            'How much do you trust the Majhis?': 'bond_with_majhis',
            'How much do you trust the Imam (religious leader)?': 'bond_with_imams',
            'Do you have enough good friends in the camp?': 'number_of_friends',
            'How often do you talk with Rohingya people outside Bangladesh and Myanmar?': 'bond_with_rohingyas_outside'
        }
    },

    'Facilitators':{
        'language_and_cultural_knowledge':{
            'To what extent do you feel at home in the camp in Bangladesh now?': 'cultural_mixability',
            'Do you think you are able to exercise your religion freely in the camp?': 'removal_of_religious_barriers',
            'How satisfied are you with the number of religious facilities (like madrasas, mosques) in the camp?': 'number_of_religious_facilities',        
        },
        'safety_and_stability':{
            'Do you often feel stressed or overwhelmed by your situation?': 'stress', #core

            'Do you think the situation in the camp is better now or 6 months ago?': 'improvement_in_6mos',
            'How often do you hear people talking about violent groups in the camp?': 'discussion_about_violent_groups',
            'How do you think violence in the camp has changed over time?': 'number_of_violence',
            'Do you think that sexual harassment in the camp happens often?': 'number_of_sexual_harrassment',
            'How afraid are you to leave your child unattended in the camp after it gets dark?': 'fear_of_children_safety',
            'Do you think that there are Rohingya families whose child has gone missing since they moved to Bangladesh?': 'fear_of_missing_children',
            'Do you personally know any Rohingya families whose child has gone missing since they moved to this camp?': 'knowledge_of_missing_children',

            'Have you thought about leaving to go to another country other than Myanmar and Bangladesh?': 'intention_to_leave', #can keep or throw
            'How scared are you to leave for another country?': 'fear_of_leaving_again',

            'How do you feel about the future?': 'feeling_about_future'
        }
    },

    'Foundation':{
        'rights_and_citizenship':{
            'Do you think that the government of Myanmar will recognize the rights of Rohingya in the next two years?': 'rights_in_home', #maybe safety_and_stability
            'If the Myanmar government recognizes the rights of Rohingya in the next two years, will you return to Myanmar?': 'return_to_home',
            'Do you think that repatriation to Myanmar will happen in the next two years?': 'repatriation_in_home',
        }
    }
}

with open('domains_of_integration_rohingya_dataset.json', 'w') as fp:
    json.dump(domains_of_integration_refugee_dataset, fp, indent=4)

df = pd.read_excel('../data/rohingya_2019.xlsx')

flatten_dict = {}
columns = ['Row ID', ]
processed_columns = ['id', ]

for k in domains_of_integration_refugee_dataset.keys():
    broad_domain = domains_of_integration_refugee_dataset[k]
    for key in broad_domain.keys():
        domain = broad_domain[key]
        for question in domain.keys():
            flatten_dict[question] = domain[question]
            columns.append(question)
            processed_columns.append(domain[question])

df = df[columns]
df.columns = processed_columns
print(df.head())

# print(columns)
categorical_to_numeric = {
    'Female': 0,
    'Male': 1,

    'I don\'t want to answer': '',
    'No': -1,
    'Yes': 1,

    'Very unsatisfied': -2,
    'Unsatisfied': -1,
    'Neither satisfied nor unsatisfied': 0,
    'Satisfied': 1,
    'Very satisfied': 2,

    'No trust at all': -2,
    'Little trust': -1,
    'Neither trust nor distrust': 0,
    'Quite a bit of trust': 1,
    'A lot of trust': 2,

    'Never': 0,
    'Once a year': 1,
    'Every few months': 2,
    'Once a month': 3,
    'Three times a month': 4,
    'Once a week': 5,
    'More than once a week': 6,

    'Not at all': 0,
    'Very little': 1,
    'Somewhat': 2,
    'To a great extent': 3,

    'six months ago': -1,
    'same-nothing has changed': 0,
    'now': 1,

    'Never': 0,
    'A few times a year': 1,
    'A few times a month': 2,
    'A few times a week': 3,

    'Decreased a lot': -2,
    'Decreased': -1,
    'Same': 0,
    'Increased': 1,
    'Increased a lot': 2,

    'I don\'t have children - does not apply': '',
    'Not afraid': 0,
    'Afraid': -1,
    'Very afraid': -2,

    'very scared': -2,
    'scared': -1,
    'neither scared nor not scared': 0,
    'not scared': 1,
    'not at all scared': 2,

    'very negative': -2,
    'negative': -1,
    'neither positive nor negative': 0,
    'positive': 1,
    'very positive': 2,
}

df = df.replace(categorical_to_numeric)
print(df.head())

df.to_excel('../data/rohingya.xlsx', index=False)
