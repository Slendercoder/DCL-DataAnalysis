# Functions to implement the Maximum Likelihood Parameter Estimation
# Edgar Andrade-Lotero 2020
print("loading packages...")
import numpy as np
import pandas as pd
from FRA import *
print("Done!")

####################################################################
# GLOBAL VARIABLES
####################################################################

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")
data = pd.read_csv('../Data/humans_absent.csv')
data['Region'] = data['Region'].apply(lambda x: [int(y) for y in x[1:-1].split(',')])
data['RegionGo'] = data['RegionGo'].apply(lambda x: [int(y) for y in x[1:-1].split(',')])
data['Overlap'] = data['Overlap'].apply(lambda x: [int(y) for y in x[1:-1].split(' ')])
data['Sims1'] = data['Sims1'].apply(lambda x: [float(y) for y in x[1:-1].split(',')])
data.head()
focals, estrategias = create_regions_and_strategies(8)

####################################################################
# FUNCTIONS
####################################################################

def MSD(params):

    global data
    global focals
    global estrategias

    pl = 0
    Num_Loc = 8
    p = 4

#    er = err2_model(data, pl, modelParameters, Num_Loc, focals, estrategias, p)
#    print("Parametros: " + str(params) + "\nError: " + str(er))
#    return er

    return err2_model(data, pl, modelParameters, Num_Loc, focals, estrategias, p)

def optimizar(parametros, minimos, maximos):
    x0 = np.array(parametros)
    bounds = Bounds(minimos, maximos)
    print("Finding fitting parameters. Please wait...")
    res = minimize(MSD, x0, method='trust-constr', bounds=bounds, options={'verbose':1})
    return res.x

####################################################################
# MAIN
####################################################################

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
optimizar(modelParameters, minimos, maximos)
