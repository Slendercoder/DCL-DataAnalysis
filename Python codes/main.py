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

# # Parameters WSLS from dyad 356-137
# modelParameters = [0, 0.15, 0, 0, 30, 499.886, 32, 0, 0, 0, 0]
# modelParameters += [0.15, 0.141, 0.07, 0.128, 200, 0, 30, 0, 0, 0, 0]
#
# # Parameters FRA from dyad 356-137
# modelParameters = [0.001, 0.150, 0.001, 0.001, 4.132, 23.942, 32.000, 13.563, 4.180, 0.900, 90.439]
# modelParameters += [0.150, 0.001, 0.150, 0.005, 200.000, 499.601, 8, 0.988, 100, 100, 100]

# Parameters FRA from dyad 435-261
modelParameters = [0.001, 0.050, 0.001, 0.001, 200.0000, 499.7581, 9.548736, 0.9015417, 99.85113, 99.99989, 99.99997]
modelParameters += [0.150, 0.001, 0.050, 0.001, 200.000, 499.967, 0, 15, 99.927, 100, 100]

RM.standard_simulation(gameParameters, modelParameters)
