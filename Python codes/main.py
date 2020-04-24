# Simulation of probabilistic heuristic for WSLS and FRA solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2020
# Run with Python 3

print('Importing packages...')
import run_model as RM
print('Done!')


##########################################################################
#
#  Simulation starts here
#
##########################################################################

# Create experiment
p = 0.5 # probability of there being a unicorn
pl = 2 # number of players
n = 8 # number of rows/columns in grid
rounds = 60 # number of rounds
dyads = 50 # number of dyads
gameParameters = [p, pl, n, rounds, dyads]

# Model Parameters
modelParameters = [0.056, 0.038, 0.01, 0.001, 499.577, 499.451, 2.113, 497.689, 499.938, 0.947] #PL1
# modelParameters = [0.001, 0.001, 0.001, 0.001, 500, 500, 32, 500, 500, 0.7] #PL1
# modelParameters += [0.001, 0.001, 0.001, 0.001, 200, 500, 32, 200, 500, 0.7] #PL2
modelParameters += modelParameters # Both players equal parameters
# modelParameters += [0, 0, 0, 0.4, 500, 500, 32, 0, 0, 0] #PL2

RM.standard_simulation(gameParameters, modelParameters)
# RM.sample_variation(gameParameters, modelParameters, "FRA", 100)
