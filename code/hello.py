import semopy
from semopy import Model
import pandas as pd
from pprint import pprint

desc = semopy.examples.political_democracy.get_model()
# print(desc)

data = semopy.examples.political_democracy.get_data()
# print(type(data), data.columns)

mod = Model(desc)
res = mod.fit(data)
print(res)

ins = mod.inspect()
# pprint(ins)
