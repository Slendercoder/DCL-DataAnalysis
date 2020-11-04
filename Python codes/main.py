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

modelParameters = [True] # Attractor to ALL
modelParameters += [False] # Attractor to NOTHING
modelParameters += [False] # Attractor to FAIR FOCALS
modelParameters += [False] # Attractor to IN OUT
modelParameters += [False] # Repelled away from ALL
modelParameters += [False] # Repelled away from FAIR FOCALS
modelParameters += [False] # Repelled away from IN - OUT
modelParameters += [False] # WINSTAY
# modelParameters += modelParameters # Replicate second player
modelParameters += [False] # Attractor to ALL
modelParameters += [False] # Attractor to NOTHING
modelParameters += [True] # Attractor to FAIR FOCALS
modelParameters += [False] # Attractor to IN OUT
modelParameters += [True] # Repelled away from ALL
modelParameters += [False] # Repelled away from FAIR FOCALS
modelParameters += [False] # Repelled away from IN - OUT
modelParameters += [False] # WINSTAY

RM.simulation_with_measures(gameParameters, modelParameters, '5')
