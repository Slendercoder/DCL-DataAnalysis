# Simulation of probabilistic heuristic for WSLS and FRA solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
# Run with Python 3

print('Importing packages...')
import numpy as np
import pandas as pd
import EmergenceDCL as DL
print('Done!')

##########################################################################
# DEFINE FUNCTIONS
##########################################################################

def standard_simulation(gameParameters, modelParameters):

    print("****************************")
    print('Starting simulation')
    print("****************************")
    print('--- Model parameters ----')
    print('wALL: ', modelParameters[0])
    print('wNOTHING: ', modelParameters[0])
    print('wDOWN: ', modelParameters[0])
    print('wIN: ', modelParameters[0])
    print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print("\n")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    E = DL.Experiment(gameParameters, modelParameters)
    E.run_simulation()
    E.get_measures()
    E.df.to_csv('output.csv', index=False)
    print('Data saved to output.csv')

    # print(E.df)

def parameter_sweep1(gameParameters, modelParameters):

    # Sweep RS vs alpha

    print("****************************")
    print('Starting parameter sweep')
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    print('--- Fixed parameters ----')
    # print('Focal: ', modelParameters[0])
    print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    # print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print("\n")
    print("Sweeping Focal and Gamma parameters...")

    # Intervals for sweep
    # forFocal = np.arange(0.025, 0.05, 0.075)
    # forGamma = np.arange(0.95, 0.975, 0.99)
    forFocal = [0.025, 0.05, 0.075]
    forGamma = [0.95, 0.975, 0.99]

    print('--- Sweep parameters ----')
    print('Focal: ', forFocal)
    print('Gamma: ', forGamma)

    for i in list(forFocal):
        for j in list(forGamma):
            print('\n----------')
            print('Sweep ' + str(i) + ', ' + str(j))
            modelParameters[0] = i
            modelParameters[3] = j
            E = DL.Experiment(gameParameters, modelParameters)
            E.run_simulation()
            E.get_measures()
            E.df['Focal'] = [i]*len(E.df['Dyad'])
            E.df['Gamma'] = [j]*len(E.df['Dyad'])
            outputFile = 'out_Focal' + str(i) + '-Gamma' + str(j) + '.csv'
            E.df.to_csv(outputFile, index=False)
            print("Results saved to " + outputFile)

def parameter_sweep2(gameParameters, modelParameters):

    # Sweep RS vs alpha

    print("****************************")
    print('Starting parameter sweep')
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    print('--- Fixed parameters ----')
    # print('Focal: ', modelParameters[0])
    # print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print("\n")
    print("Sweeping Focal and alpha parameters...")

    # Intervals for sweep
    # forFocal = np.arange(0.025, 0.05, 0.075)
    # forGamma = np.arange(0.95, 0.975, 0.99)
    forFocal = [0.025, 0.05, 0.075]
    forAlpha = [0, 70, 150]

    print('--- Sweep parameters ----')
    print('Focal: ', forFocal)
    print('alpha: ', forAlpha)

    for i in list(forFocal):
        for j in list(forAlpha):
            print('\n----------')
            print('Sweep ' + str(i) + ', ' + str(j))
            modelParameters[0] = i
            modelParameters[1] = j
            E = DL.Experiment(gameParameters, modelParameters)
            E.run_simulation()
            E.get_measures()
            E.df['Focal'] = [i]*len(E.df['Dyad'])
            E.df['Alpha'] = [j]*len(E.df['Dyad'])
            outputFile = 'out_Focal' + str(i) + '-Alpha' + str(j) + '.csv'
            E.df.to_csv(outputFile, index=False)
            print("Results saved to " + outputFile)

def parameter_sweep3(gameParameters, modelParameters):

    # Sweep RS vs alpha

    print("****************************")
    print('Starting parameter sweep')
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    print('--- Fixed parameters ----')
    print('alpha: ', modelParameters[4])
    print('beta: ', modelParameters[5])
    print('gamma: ', modelParameters[6])
    print('epsilon: ', modelParameters[8])
    print("\n")

    # Intervals to sweep
    forDelta = np.arange(1.3, 1.8, 0.1)
    forZeta = np.arange(2.5, 3.75, 0.25)

    print('--- Sweep parameters ----')
    print('delta: ', forDelta)
    print('zeta: ', forZeta)

    aux = {}
    aux['delta'] = []
    aux['zeta'] = []
    aux['Av_DLL'] = []
    for i in list(forDelta):
        for j in list(forZeta):
            print('\n----------')
            print('Sweep ' + str(i) + ', ' + str(j))
            modelParameters[7] = i
            modelParameters[9] = j
            E = DL.Experiment(gameParameters, modelParameters)
            E.run_simulation()
            E.get_DLL()
            data = E.df.loc[E.df['Round'] > 50]

            aux['delta'].append(i)
            aux['zeta'].append(j)
            aux['Av_DLL'].append(data['DLIndex'].mean())

    data = pd.DataFrame.from_dict(aux)
    print("Sorting by delta, zeta ...")
    data = data.sort_values(['delta','zeta'], ascending=[True, True])
    outputFile = 'sweep3.csv'
    data.to_csv(outputFile, index=False)
    print("Results saved to " + outputFile)

##########################################################################
#
#  Simulation starts here
#
##########################################################################

# Create experiment
gameParameters = [0.5, 2, 8, 60, 45]
modelParameters = [0, 0, 500, 0.98, 0, 0, 1]

# standard_simulation(gameParameters, modelParameters)

parameter_sweep2(gameParameters, modelParameters)