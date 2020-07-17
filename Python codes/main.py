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


# Simulate data with fitter parameters
RM.simulate_with_parameter_fit(gameParameters)

# # Model Parameters FRA
# modelParameters = [0.043, 0.038, 0.003, 0.001, \
#                     10.4, 99, 30,\
#                     1.2, 99, 0.89, 0.28, 0.21, 0.05] #PL1
# modelParameters += modelParameters # Both players have equal parameters
# RM.simulation_with_measures(gameParameters, modelParameters, '05')
