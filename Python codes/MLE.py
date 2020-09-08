# Functions to implement the Maximum Likelihood Parameter Estimation
# Edgar Andrade-Lotero 2020
print("loading packages...")
import numpy as np
import pandas as pd
from FRA import *
from MSD import *
print("Done!")

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")
data = pd.read_csv('../Data/humans_absent.csv')
data['Region'] = data['Region'].apply(lambda x: [int(y) for y in x[1:-1].split(',')])
data['RegionGo'] = data['RegionGo'].apply(lambda x: [int(y) for y in x[1:-1].split(',')])
data['Overlap'] = data['Overlap'].apply(lambda x: [int(y) for y in x[1:-1].split(' ')])
data['Sims1'] = data['Sims1'].apply(lambda x: [float(y) for y in x[1:-1].split(',')])
data.head()
focals, estrategias = create_regions_and_strategies(8)

# Initial parameters
modelParameters = [0.01, 0.01, 0.01, 0.01, # Biases
               0, 0, 0, # Win Stay
               0, 0, 0, # Attraction
               0, 0, 0 # Repulsion to complement
               # 1, 100, 31.5, # Win Stay
               # 1, 20, 0.9, # Attraction
               # 1, 20, 0.7 # Repulsion to complement
              ]
minimos = [0] * 13
#maximos = [0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0, 0, 0, 0, 0, 0]
maximos = [0.25, 0.25, 0.25, 0.25, 1, 100, 32, 1, 100, 1, 1, 100, 1]
PARS = [data, focals, estrategias]
res = optimizar(modelParameters, minimos, maximos, PARS)
f = open('../Data/parameter_fit_humans.csv', 'w')
sl = ', '.join([str(x) for x in res.x])
f.close()
