# Simulation of probabilistic heuristic for WSLS and FRA solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
# Run with Python 3

print('Importing packages...')
import numpy as np
import pandas as pd
import EmergenceDCL as DL
import os
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
    print('eta: ', modelParameters[7])
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
    count = 0
    archivo = './output' + str(count) + '.csv'
    while os.path.isfile(archivo):
        count += 1
        archivo = './output' + str(count) + '.csv'
    E.df.to_csv(archivo, index=False)
    print('Data saved to' + archivo + '.csv')

    # print(E.df)

def parameter_sweep_alpha(gameParameters, modelParameters):

    # Sweep alpha

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
    print('Focal: ', modelParameters[0])
    # print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print('eta: ', modelParameters[7])
    print("\n")
    print("Sweeping alpha...")

    # Intervals for sweep
    forSweep = [0, 70, 150]

    print('--- Sweep parameters ----')
    print('alpha: ', forSweep)

    for i in list(forSweep):
        print('\n----------')
        print('Sweep alpha=' + str(i))
        modelParameters[1] = i
        E = DL.Experiment(gameParameters, modelParameters)
        E.run_simulation()
        E.get_measures()
        E.df['Alpha'] = [i]*len(E.df['Dyad'])
        outputFile = 'Sweeps/out_Alpha_' + str(i) + '.csv'
        E.df.to_csv(outputFile, index=False)
        print("Results saved to " + outputFile)

def parameter_sweep_Focal(gameParameters, modelParameters):

    # Sweep RS

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
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print('eta: ', modelParameters[7])
    print("\n")
    print("Sweeping RS...")

    # Intervals for sweep
    forSweep = [0, 0.05, 0.075]

    print('--- Sweep parameters ----')
    print('RS: ', forSweep)

    for i in list(forSweep):
        print('\n----------')
        print('Sweep RS=' + str(i))
        modelParameters[0] = i
        E = DL.Experiment(gameParameters, modelParameters)
        E.run_simulation()
        E.get_measures()
        E.df['RS'] = [i]*len(E.df['Dyad'])
        outputFile = 'Sweeps/out_RS_' + str(i) + '.csv'
        E.df.to_csv(outputFile, index=False)
        print("Results saved to " + outputFile)

def parameter_sweep_Zeta(gameParameters, modelParameters):

    # Sweep RS

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
    print('Focal: ', modelParameters[0])
    print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    # print('zeta: ', modelParameters[6])
    print('eta: ', modelParameters[7])
    print("\n")
    print("Sweeping Zeta...")

    # Intervals for sweep
    forSweep = [0, 1, 10]

    print('--- Sweep parameters ----')
    print('zeta: ', forSweep)

    for i in list(forSweep):
        print('\n----------')
        print('Sweep zeta=' + str(i))
        modelParameters[6] = i
        E = DL.Experiment(gameParameters, modelParameters)
        E.run_simulation()
        E.get_measures()
        E.df['Zeta'] = [i]*len(E.df['Dyad'])
        outputFile = 'Sweeps/out_Zeta_' + str(i) + '.csv'
        E.df.to_csv(outputFile, index=False)
        print("Results saved to " + outputFile)

def parameter_sweep_Delta(gameParameters, modelParameters):

    # Sweep RS

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
    print('Focal: ', modelParameters[0])
    print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    # print('delta: ', modelParameters[4])
    print('epsilon: ', modelParameters[5])
    print('zeta: ', modelParameters[6])
    print('eta: ', modelParameters[7])
    print("\n")
    print("Sweeping Delta...")

    # Intervals for sweep
    forSweep = [0, 10, 50]

    print('--- Sweep parameters ----')
    print('delta: ', forSweep)

    for i in list(forSweep):
        print('\n----------')
        print('Sweep delta=' + str(i))
        modelParameters[4] = i
        E = DL.Experiment(gameParameters, modelParameters)
        E.run_simulation()
        E.get_measures()
        E.df['Delta'] = [i]*len(E.df['Dyad'])
        outputFile = 'Sweeps/out_Delta_' + str(i) + '.csv'
        E.df.to_csv(outputFile, index=False)
        print("Results saved to " + outputFile)

def parameter_sweep_2(gameParameters, modelParameters):

    # Sweep epsilon vs zeta

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
    print('Focal: ', modelParameters[0])
    print('alpha: ', modelParameters[1])
    print('beta: ', modelParameters[2])
    print('gamma: ', modelParameters[3])
    print('delta: ', modelParameters[4])
    # print('epsilon: ', modelParameters[5])
    # print('zeta: ', modelParameters[6])
    print('eta: ', modelParameters[7])
    print('FOCAL: ', modelParameters[8])
    print("\n")
    print("Sweeping epsilon and zeta parameters...")

    # Intervals for sweep
    forEpsilon = [1]
    forZeta = [20]

    print('--- Sweep parameters ----')
    print('epsilon: ', forEpsilon)
    print('zeta: ', forZeta)

    for i in list(forEpsilon):
        for j in list(forZeta):
            print('\n----------')
            print('Sweep ' + str(i) + ', ' + str(j))
            modelParameters[5] = i
            modelParameters[6] = j
            E = DL.Experiment(gameParameters, modelParameters)
            E.run_simulation()
            E.get_measures()
            E.df['Epsilon'] = [i]*len(E.df['Dyad'])
            E.df['Zeta'] = [j]*len(E.df['Dyad'])
            outputFile = 'Sweeps/out_Epsilon' + str(i) + '-Zeta' + str(j) + '.csv'
            E.df.to_csv(outputFile, index=False)
            print("Results saved to " + outputFile)


def exploreSampleSizeEffect(gameParameters, modelParameters, lst):

    for l in lst:
        gameParameters = gameParameters[:4] + [l]
        print("****************************")
        print('Starting simulation', l)
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
        print('eta: ', modelParameters[7])
        print('FOCAL: ', modelParameters[8])
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
        f = 'Sweeps/output_' + str(l) + '.csv'
        E.df.to_csv(f, index=False)
        print('Data saved to', f)


##########################################################################
#
#  Simulation starts here
#
##########################################################################

# Create experiment
gameParameters = [0.5, 2, 8, 40, 2]
# WSLS optim
# modelParameters = [0.438, 22.5, 10, 31, 0, 0, 0, 0] # optimos
# modelParameters = [0.43*0.61, 8.33*95.91, 500, 0.98, 0, 0, 0, 0] # optimos corregidos
# FRA optim
# modelParameters = [0.022, 13.22, 500, 0.98, 0.00000007, 1, 1.72, 1.2]
# modelParameters = [0.022*0.19, 13.22*15.75, 500, 0.98, 0.00000007*1.29, 1, 1.72*1.35, 1.2]

# Para tofitWSLS
modelParameters = [1, 150, 10, 31, 0, 0, 0, 0]
# modelParameters = [0.8, 130, 10, 31, 0, 0, 0, 0] # full data
# modelParameters = [0.93, 13, 10, 31, 0, 0, 0, 0] # only unicorn absent

# Para tofitFRA
# modelParameters = [0.022, 150, 500, 0.98, 10, 1, 1.5, 1.2]
# modelParameters = [0.022, 150, 500, 0.98, 10, 1, 1.5, 1.2]

standard_simulation(gameParameters, modelParameters)

# # Sweep alpha
# modelParameters = [0.05, 0, 500, 0.98, 0, 0, 0, 0]
# parameter_sweep_alpha(gameParameters, modelParameters)

# # Sweep RS
# modelParameters = [0, 150, 500, 0.98, 0, 0, 0, 0]
# parameter_sweep_Focal(gameParameters, modelParameters)

# # Sweep Zeta
# modelParameters = [0.03, 150, 500, 0.98, 0, 1, 0, 1.2]
# parameter_sweep_Zeta(gameParameters, modelParameters)

# # Sweep Delta
# modelParameters = [0.03, 150, 500, 0.98, 0, 1, 1, 1.2]
# parameter_sweep_Delta(gameParameters, modelParameters)

# lst = [50, 100, 200, 300, 400, 500, 600, 700, 800, 900]
# exploreSampleSizeEffect(gameParameters, modelParameters, lst)
