# Simulation of probabilistic heuristic for WSLS and FRA solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
# Run with Python 3

print('Importing packages...')
import run_model as RM
import os
print('Done!')


##########################################################################
#
#  Simulation starts here
#
##########################################################################

# Create experiment
gameParameters = [0.5, 2, 8, 60, 200]

# WSLS model2Recover
modelParameters = [0.0125, 0.0125, 0.0125, 0.0125, 150, 10, 31, 0, 0, 0, 0]
# # WSLS model recovered only absent
# modelParameters = [0.012, 0.011, 0.012, 0.013, 137.788, 258.525, 30.878, 0, 0, 0, 0]

# WSLS optim
# modelParameters = [0.14, 0.0674, 0.0123, 0.0009, 39, 405, 0.93, 0, 0, 0, 0] # optimos
# FRA optim
# modelParameters = [0.216, 48, 10, 31, 1.53, 0.94, 3, 1.1]

RM.standard_simulation(gameParameters, modelParameters)
