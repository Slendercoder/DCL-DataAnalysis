# Create figures from parameter sweeps
# Edgar Andrade-Lotero 2019
# Run with Python 3

print('Importing packages...')
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sys import argv
print('Done!')

script, data_archivo = argv

# Opens the file with data from DCL experiment and parses it into data
print("Reading data...")
data = pd.read_csv(data_archivo, index_col=False)
print("Data red!")
# print data[:3]

# Produce graphics
print("Producing graphics...")

# # figs for av. DLindex per round
# fig, axes = plt.subplots()
# axes.set_xlabel('Rounds (unicorn absent)', fontsize = 12)
# # axes.set_ylabel('Score', fontsize = 14)
# axes.set_ylim([minimum_score-2, maximum_score+2])
# axes.set_ylabel('DLIndex', fontsize = 14)
# # axes.yaxis.tick_right()
# # axes.yaxis.set_label_position("right")

# Players = data.Player.unique()
# Grp = data.groupby(['Player']).get_group(Players[0])
d = {}
for key, grp in data.groupby(['Round']):
    d[key] = grp['DLIndex'].mean()
# d = pd.Series(Grp.DLIndex.values,index=Grp.Round).to_dict()
x = pd.DataFrame(pd.Series(range(1,60)))
x.columns = ['A']
x['B'] = [0]*len(x['A'])
print(x.columns)
x['B'] = x['A'].map(d)
# print(x)
xs = x['A'].tolist()
xs = np.array(xs)
series1 = x['B'].tolist()
series1 = np.array(series1)
s1mask = np.isfinite(series1)

# Plotting DLIndex per round
fig, axes = plt.subplots()
axes.set_xlabel('Rounds (unicorn absent)', fontsize = 12)
axes.set_ylim([0, 1])
axes.set_ylabel('Average DLIndex', fontsize = 14)
# Par1 = data.Focal.unique()[0]
# Par2 = data.Gamma.unique()[0]
# axes.set_title('Focal: ' + str(Par1) + ' Gamma: ' + str(Par2))

axes.plot(xs[s1mask], series1[s1mask], linestyle='-', color='black')

plt.show()

# Plotting Consistency vs Norm_Score_LAG1
fig, axes = plt.subplots()
axes.set_xlabel('Norm. score previous round', fontsize = 12)
axes.set_ylim([0, 1])
axes.set_ylabel('Consistency', fontsize = 14)
# Par1 = data.Focal.unique()[0]
# Par2 = data.Gamma.unique()[0]
# axes.set_title('Focal: ' + str(Par1) + ' Gamma: ' + str(Par2))

axes.scatter(data['Norm_Score_LAG1'], data['Consistency'])

plt.show()
