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

# RM.data_for_confusion_matrix()

# Create experiment
p = 0.5 # probability of there being a unicorn
pl = 2 # number of players
n = 8 # number of rows/columns in grid
rounds = 60 # number of rounds
dyads = 50 # number of dyads
gameParameters = [p, pl, n, rounds, dyads]


# Model Parameters FRA
modelParameters = [0.063, 0.035, 0.006, 0.002, \
                    99, 100, 30,\
                    1.5, 100, 0.978, 0.5, 0.5, 0] #PL1
modelParameters += modelParameters # Both players have equal parameters
RM.simulation_with_measures(gameParameters, modelParameters, '05')
