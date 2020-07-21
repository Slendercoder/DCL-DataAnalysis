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

# RM.data_for_confusion_matrix(gameParameters, 10)

# To simulate from chosen parameters
# # Model Parameters BIAS
# modelParameters = [0.1, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0] #PL1
# modelParameters += modelParameters # Both players have equal parameters
# RM.simulation_with_measures(gameParameters, modelParameters, '05')
#
# # Model Parameters WSLS
# modelParameters = [0.1, 0.1, 0.1, 0.1, 100, 30, 31, 0, 0, 0] #PL1
# modelParameters += modelParameters # Both players have equal parameters
# RM.simulation_with_measures(gameParameters, modelParameters, '05')
#
# # Model Parameters FRA
# modelParameters = [0.1, 0.1, 0.1, 0.1, 100, 30, 31, 100, 30, 0.7] #PL1
# modelParameters += modelParameters # Both players have equal parameters
# RM.simulation_with_measures(gameParameters, modelParameters, '05')

# To simulate fitted models
# Model Parameters MBiases
modelParameters = [0.12, 0.077,  0.058, 0.005, 0, 0, 0, 0, 0, 0] #PL1
modelParameters += modelParameters # Both players have equal parameters
RM.simulation_with_measures(gameParameters, modelParameters, '05')

# Model Parameters WSLS
modelParameters = [0.085, 0.042, 0.015, 0.002, 86.746, 100, 4.218, 0, 0, 0] #PL1
modelParameters += modelParameters # Both players have equal parameters
RM.simulation_with_measures(gameParameters, modelParameters, '05')

# Model Parameters FRA
modelParameters = [0.063, 0.035, 0.006, 0.002, 90.106, 100, 30, 0.485, 100, 0.978] #PL1
modelParameters += modelParameters # Both players have equal parameters
RM.simulation_with_measures(gameParameters, modelParameters, '05')
