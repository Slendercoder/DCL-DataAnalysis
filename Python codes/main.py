# Simulation of probabilistic heuristic for WSLS and FRA solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
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
gameParameters = [0.5, 2, 8, 60, 500]

# WSLS model2Recover
modelParameters = [0.1, 0.1, 0.05, 0.05, 150, 10, 31, 0, 0, 0, 0]
# # WSLS model recovered only absent
# modelParameters = [0.089, 0.099, 0.044, 0.042, 122.228, 160.825, 31.060, 0, 0, 0, 0]

# WSLS optim
# modelParameters = [0.14, 0.0674, 0.0123, 0.0009, 39, 405, 0.93, 0, 0, 0, 0] # optimos
modelParameters = [0.094, 0.078, 0.017, 0.005, 10.619, 495.681, 29.002, 0, 0, 0, 0] # optimos
# FRA optim
# modelParameters = [0.216, 48, 10, 31, 1.53, 0.94, 3, 1.1]

RM.standard_simulation(gameParameters, modelParameters)
