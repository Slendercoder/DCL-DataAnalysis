import EmergenceDCL as DL
import Measures as M
import numpy as np
import pandas as pd
from random import uniform
import os

##########################################################################
# DEFINE FUNCTIONS RUN MODEL
##########################################################################

def standard_simulation(gameParameters, modelParameters):

    print("****************************")
    print('Starting simulation')
    print("****************************")
    print('--- Model parameters ----')
    print('--- Player 1 ----')
    print('softmax: ', modelParameters[0])
    print('wALL: ', modelParameters[1])
    print('wNOTHING: ', modelParameters[2])
    print('wLEFT: ', modelParameters[3])
    print('wIN: ', modelParameters[4])
    print('alpha: ', modelParameters[5])
    print('beta: ', modelParameters[6])
    print('gamma: ', modelParameters[7])
    print('delta: ', modelParameters[8])
    print('epsilon: ', modelParameters[9])
    print('zeta: ', modelParameters[10])
    print("\n")
    print('--- Player 2 ----')
    print('softmax: ', modelParameters[11])
    print('wALL: ', modelParameters[12])
    print('wNOTHING: ', modelParameters[13])
    print('wLEFT: ', modelParameters[14])
    print('wIN: ', modelParameters[15])
    print('alpha: ', modelParameters[16])
    print('beta: ', modelParameters[17])
    print('gamma: ', modelParameters[18])
    print('delta: ', modelParameters[19])
    print('epsilon: ', modelParameters[20])
    print('zeta: ', modelParameters[21])
    print("\n")
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    E = DL.Experiment(gameParameters, modelParameters)
    # Inicializa archivo temporal
    with open('temp.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy\n')
        dfile.close()
    with open('output_Prev.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy,Is_there_LEAD,Category,Category1,RegionGo\n')
        dfile.close()
    E.run_simulation()
    count = 0
    archivo = './output' + str(count) + '.csv'
    while os.path.isfile(archivo):
        count += 1
        archivo = './output' + str(count) + '.csv'
    E.df.to_csv(archivo, index=False)
    print('Data saved to' + archivo)

    return E.df

def simulation_with_measures(gameParameters, modelParameters, medidas):

    print("****************************")
    print('Starting simulation')
    print("****************************")
    print('--- Model parameters ----')
    print('--- Player 1 ----')
    print('wALL: ', modelParameters[0])
    print('wNOTHING: ', modelParameters[1])
    print('wLEFT: ', modelParameters[2])
    print('wIN: ', modelParameters[3])
    print('alpha: ', modelParameters[4])
    print('beta: ', modelParameters[5])
    print('gamma: ', modelParameters[6])
    print('delta: ', modelParameters[7])
    print('epsilon: ', modelParameters[8])
    print('zeta: ', modelParameters[9])
    print("\n")
    print('--- Player 2 ----')
    print('wALL: ', modelParameters[10])
    print('wNOTHING: ', modelParameters[11])
    print('wLEFT: ', modelParameters[12])
    print('wIN: ', modelParameters[13])
    print('alpha: ', modelParameters[14])
    print('beta: ', modelParameters[15])
    print('gamma: ', modelParameters[16])
    print('delta: ', modelParameters[17])
    print('epsilon: ', modelParameters[18])
    print('zeta: ', modelParameters[19])
    print("\n")
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    E = DL.Experiment(gameParameters, modelParameters)
    # Inicializa archivo temporal
    with open('temp.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy\n')
        dfile.close()
    with open('output_Prev.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy,Is_there_LEAD,Category,Category1,RegionGo\n')
        dfile.close()
    E.run_simulation()
    E.df = M.get_measures(E.df, medidas)
    count = 0
    archivo = '../Data/output' + str(count) + '.csv'
    while os.path.isfile(archivo):
        count += 1
        archivo = '../Data/output' + str(count) + '.csv'
    E.df.to_csv(archivo, index=False)
    print('Data saved to' + archivo)

    return E.df

def data_conf_mtrx(gameParameters, modelParameters, model, count):

    print("****************************")
    print('Starting simulation with model', model)
    print("****************************")
    print('--- Model parameters ----')
    print('--- Player 1 ----')
    print('wALL: ', modelParameters[0])
    print('wNOTHING: ', modelParameters[1])
    print('wLEFT: ', modelParameters[2])
    print('wIN: ', modelParameters[3])
    print('alpha: ', modelParameters[4])
    print('beta: ', modelParameters[5])
    print('gamma: ', modelParameters[6])
    print('delta: ', modelParameters[7])
    print('epsilon: ', modelParameters[8])
    print('zeta: ', modelParameters[9])
    print("\n")
    print('--- Player 2 ----')
    print('wALL: ', modelParameters[10])
    print('wNOTHING: ', modelParameters[11])
    print('wLEFT: ', modelParameters[12])
    print('wIN: ', modelParameters[13])
    print('alpha: ', modelParameters[14])
    print('beta: ', modelParameters[15])
    print('gamma: ', modelParameters[16])
    print('delta: ', modelParameters[17])
    print('epsilon: ', modelParameters[18])
    print('zeta: ', modelParameters[19])
    print("\n")
    print("****************************")
    print('--- Game parameters ---')
    print('Probabilit of a unicorn: ', gameParameters[0])
    print('Number of players: ', gameParameters[1])
    print('Grid size: ', str(gameParameters[2]) + ' x ' + str(gameParameters[2]))
    print('Number of rounds: ', gameParameters[3])
    print('Number of dyads: ', gameParameters[4])
    print("\n")

    E = DL.Experiment(gameParameters, modelParameters)
    # Inicializa archivo temporal
    with open('temp.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy\n')
        dfile.close()
    with open('output_Prev.csv', 'w') as dfile:
        dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy,Is_there_LEAD,Category,Category1,RegionGo\n')
        dfile.close()
    E.run_simulation()
    E.df = M.get_measures(E.df, '5')
    archivo = '../Data/Confusion/Simulations/' + str(model) + str(count) + '.csv'
    E.df.to_csv(archivo, index=False)
    print('Data saved to' + archivo)
    rel_data_sim = [count] + [str(model)] + modelParameters[:10]
    dfAux = pd.DataFrame([rel_data_sim])
    with open('../Data/Confusion/Simulations/sim_data_rel.csv', 'a') as f:
        dfAux.to_csv(f, header=False, index=False)

    return E.df

def random_pars_Bmodel():
    mParameters = []
    mParameters.append(uniform(0, 0.12)) # appending random wALL
    mParameters.append(uniform(0, 0.12)) # appending random wNOTHING
    mParameters.append(uniform(0, 0.12)) # appending random wLEFT
    mParameters.append(uniform(0, 0.12)) # appending random wIN
    mParameters.append(0) # appending Alpha
    mParameters.append(0) # appending Beta
    mParameters.append(0) # appending Gamma
    mParameters.append(0) # appending Delta
    mParameters.append(0) # appending Epsilon
    mParameters.append(0) # appending Zeta
    mParameters += mParameters
    return mParameters

def random_pars_WSLSmodel():
    mParameters = []
    mParameters.append(uniform(0, 0.12)) # appending random wALL
    mParameters.append(uniform(0, 0.12)) # appending random wNOTHING
    mParameters.append(uniform(0, 0.12)) # appending random wLEFT
    mParameters.append(uniform(0, 0.12)) # appending random wIN
    mParameters.append(uniform(0, 1000)) # appending random Alpha
    mParameters.append(1000) # appending Beta
    mParameters.append(uniform(0, 32)) # appending random Gamma
    mParameters.append(0) # appending Delta
    mParameters.append(0) # appending Epsilon
    mParameters.append(0) # appending Zeta
    mParameters += mParameters
    return mParameters

def random_pars_FRAmodel():
    mParameters = []
    mParameters.append(uniform(0, 0.12)) # appending random wALL
    mParameters.append(uniform(0, 0.12)) # appending random wNOTHING
    mParameters.append(uniform(0, 0.12)) # appending random wLEFT
    mParameters.append(uniform(0, 0.12)) # appending random wIN
    mParameters.append(uniform(0, 1000)) # appending random Alpha
    mParameters.append(1000) # appending Beta
    mParameters.append(uniform(0, 32)) # appending random Gamma
    mParameters.append(uniform(0, 1000)) # appending random Delta
    mParameters.append(1000) # appending Epsilon
    mParameters.append(uniform(0, 1)) # appending random Zeta
    mParameters += mParameters
    return mParameters

def data_for_confusion_matrix(gameParameters, N = 10):

    for n in range(N):
        modelParameters = random_pars_Bmodel()
        data_conf_mtrx(gameParameters, modelParameters, 'MB', n)
        modelParameters = random_pars_WSLSmodel()
        data_conf_mtrx(gameParameters, modelParameters, 'WS', n)
        modelParameters = random_pars_FRAmodel()
        data_conf_mtrx(gameParameters, modelParameters, 'FR', n)

    print("Done!")

def sample_variation(gameParameters, modelParameters, model, n_samples = 100):

    print('Obtaining', n_samples, 'samples (please be patient!)...')
    nombre = 'Simulations/Sample_size/' + model + '/sample'

    for i in range(n_samples):
        E = DL.Experiment(gameParameters, modelParameters)
        E.run_simulation()
        data = M.get_measures(E.df, '0')
        archivo = nombre + str(i + 1) + '.csv'
        data.to_csv(archivo, index=False)

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

def parameter_sweep_softmax(gameParameters, modelParameters):

    # Sweep softmax

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
    print('wALL: ', modelParameters[1])
    print('wNOTHING: ', modelParameters[2])
    print('wLEFT: ', modelParameters[3])
    print('wIN: ', modelParameters[4])
    print('alpha: ', modelParameters[5])
    print('beta: ', modelParameters[6])
    print('gamma: ', modelParameters[7])
    print('delta: ', modelParameters[8])
    print('epsilon: ', modelParameters[9])
    print('zeta: ', modelParameters[10])
    print("\n")
    print("Sweeping softmax...")

    # Intervals for sweep
    forSweep = [0.1, 1, 2, 3]

    print('--- Sweep parameters ----')
    print('softmax: ', forSweep)

    num = 1
    for i in forSweep:
        print('\n----------')
        print('Sweep softmax=' + str(i))
        modelParameters[0] = i
        E = DL.Experiment(gameParameters, modelParameters)
        # Inicializa archivo temporal
        with open('temp.csv', 'w') as dfile:
            dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy\n')
            dfile.close()
        with open('output_Prev.csv', 'w') as dfile:
            dfile.write('index,Dyad,Round,Player,Answer,Time,a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,a27,a28,a31,a32,a33,a34,a35,a36,a37,a38,a41,a42,a43,a44,a45,a46,a47,a48,a51,a52,a53,a54,a55,a56,a57,a58,a61,a62,a63,a64,a65,a66,a67,a68,a71,a72,a73,a74,a75,a76,a77,a78,a81,a82,a83,a84,a85,a86,a87,a88,Score,Joint,Is_there,where_x,where_y,Strategy,Is_there_LEAD,Category,Category1,RegionGo\n')
            dfile.close()
        E.run_simulation()
        E.df = M.get_measures(E.df, '5')
        archivo = '../Data/Sweeps/softmax/' + str(num) + '-Sweep-softmax-' + str(i) + '.csv'
        E.df.to_csv(archivo, index=False)
        print('Data saved to' + archivo)
        rel_data_sim = [num] + modelParameters[:11]
        num += 1
        dfAux = pd.DataFrame([rel_data_sim])
        with open('../Data/Sweeps/softmax/sim_data_rel.csv', 'a') as f:
            dfAux.to_csv(f, header=False, index=False)

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


def sensitivityModelRecovery(gameParameters, modelParameters, badApples):

    print("****************************")
    print('Starting simulation')
    print("****************************")
    print('--- Model parameters ----')
    print('w: ', modelParameters[0])
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
    print('Number of bad apples: ', badApples)
    print("\n")

    E = DL.Experiment(gameParameters, modelParameters)
    Total_Dyads = gameParameters[4]
    print("\n")
    print('Including ' + str(badApples) + ' bad apples...')
    for j in range(Total_Dyads - badApples):
        print("****************************")
        print("Running dyad no. ", j + 1, "--GOOD")
        print("****************************\n")
        E.run_dyad()

    for j in range(badApples):
        print("****************************")
        print("Running dyad no. ", j + 1 + Total_Dyads - badApples, "--BAD")
        print("****************************\n")
        E.run_dyad_with_parameters(0.1, 10)

    E.get_measures()
    f = 'Sweeps/sensitivity_' + str(badApples) + '.csv'
    E.df.to_csv(f, index=False)
    print('Data saved to', f)
