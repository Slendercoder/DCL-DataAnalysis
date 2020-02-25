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
# modelParameters = [0.001, 0.001, 0.001, 0.001, 200, 500, 32, 200, 500, 0.7] #PL1
# modelParameters += [0.001, 0.001, 0.001, 0.001, 200, 500, 32, 200, 500, 0.7] #PL2
modelParameters = [0.05, 0.05, 0.05, 0.05, 500, 500, 32, 0, 0, 0] #PL1
modelParameters += modelParameters # Both players equal parameters
# modelParameters += [0, 0, 0, 0.4, 500, 500, 32, 0, 0, 0] #PL2

RM.standard_simulation(gameParameters, modelParameters)
