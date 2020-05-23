print('Importing packages...')
# import numpy as np
import pandas as pd

data = pd.read_csv('../Data/humans_only_absent.csv')
print('Data loaded!')
# print(data[['Dyad', 'Player', 'Score']][:3])

dyads_LR = ['419-723', '435-261', '469-569', '637-838', '648-175', \
            '656-979', '947-704', '948-444']

dyads_AN = ['352-425', '356-137', '416-710', '462-640', '487-811', '505-833', '938-219']

dyads_TD = ['152-727', '251-716', '277-491', '331-863', \
            '354-344', '546-111', '588-564', '768-541', '825-534']

dyads_IO = ['140-615']

Dyads = dyads_LR + dyads_AN + dyads_TD + dyads_IO

data['Mantener'] = data['Dyad'].apply(lambda x: x in Dyads)
print(data[['Dyad', 'Mantener']][:3])

data = data[data['Mantener']]
print(data[['Dyad', 'Mantener']][:3])

del data['Mantener']

data.to_csv('../Data/high_performing_human_dyads.csv', index=False)
