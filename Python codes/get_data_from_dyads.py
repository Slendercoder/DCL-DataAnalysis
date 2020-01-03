print('Importing packages...')
# import numpy as np
import pandas as pd

data = pd.read_csv('humans.csv')
print('Data loaded!')
# print(data[['Dyad', 'Player', 'Score']][:3])

for key, grp in data.groupby('Dyad'):
    print('Saving data from dyad', key)
    f = 'Dyads/output-' + key + '.csv'
    grp.to_csv(f, index=False)

for key, grp in data.groupby(['Dyad', 'Player']):
    print('Saving data from player', key[1])
    f = 'Dyads/output-' + key[1] + '.csv'
    grp.to_csv(f, index=False)
