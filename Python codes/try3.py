import numpy as np
import pandas as pd

data = pd.read_csv('output.csv')

print(data[['Dyad', 'Player', 'Score']][:3])

for key, grp in data.groupby('Dyad'):
    f = 'Dyads/output-' + key + '.csv'
    grp.to_csv(f, index=False)
