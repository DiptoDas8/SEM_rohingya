from numpy.core.defchararray import index
import pandas as pd
import numpy as np
from pprint import pprint

def prepare_data_dictionary(filename):
    df = pd.read_excel(filename+'.xlsx')
    data_dictionary = {'column_name': [],
                        'response_formats': [],
                        'responses': [],
                        'missing_values': [],
                        'variable_name': []}
    for colname in df.columns:
        print(colname)
        data_dictionary['column_name'].append(colname)
        data_dictionary['missing_values'].append(df[colname].isna().sum())
        responses = df[colname].dropna().unique().tolist()
        if df[colname].dropna().dtypes==np.int64 or df[colname].dropna().dtypes==np.float64:
            print(responses)
            data_dictionary['responses'].append(' - '.join(str(x) for x in [min(responses), max(responses)]))
        else:
            data_dictionary['responses'].append(', '.join(str(x) for x in responses))
        data_dictionary['response_formats'].append(str(df[colname].dtypes))
        data_dictionary['variable_name'].append('')
    data_dictionary_df = pd.DataFrame.from_dict(data_dictionary)
    # print(data_dictionary_df.head())
    data_dictionary_df.to_excel(filename+'_data_dict.xlsx', index=False)

prepare_data_dictionary('./rohingya-2019')
prepare_data_dictionary('./hostcommunity-2018')