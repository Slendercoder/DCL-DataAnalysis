# Functions to implement the Maximum Likelihood Parameter Estimation
# Edgar Andrade-Lotero 2020
print("loading packages...")
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import gridspec
import FRA as F
from scipy.optimize import minimize
from scipy.stats import multinomial
print("Done!")

data = pd.DataFrame({'A': [1, 2]})

letras = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:'
Num_Loc = 8
cols = ['a' + str(i + 1) + str(j + 1)\
        for i in range(Num_Loc)\
        for j in range(Num_Loc)\
        ]
ordenRegions = ['RS', 'ALL', 'NOTHING',\
                'BOTTOM', 'TOP', 'LEFT', 'RIGHT', 'IN', 'OUT']
colsProbs = ['RS_e', 'A_e', 'N_e', 'B_e', 'T_e', 'L_e', 'R_e', 'I_e', 'O_e']
colsFreqs = [x + "_o" for x in ordenRegions]

def list_from_row(r, cols):

    lista = []
    for c in cols:
        lista.append(list(r[c])[0])

    return lista

def code_region(x):
    # Function to code a 64-bit list into letters
    x = list(x)[5:69]
    # print('x:', x)
    code = ""
    for i in range(len(x)):
        v = x[i]
        # print('Valor:', v)
        if v == 1:
            # print('Letra:', letras[i])
            code += letras[i]
            # print('code:', code)

    # print('code', code)
    return code

def trim_data(data, simplificar=False):
    print("Trimming data...")

    # Codifying region into letter code
    data['regionCoded'] = data.apply(code_region, axis=1)
    data['Region'] = data['Category']

    if simplificar:
        # Minimal data frame TO BE COMMENTED FOR USE IN FULL DATA
        parejas = list(data.Dyad.unique())[:2]
        rondas = list(data.Round.unique())[:2]
        data = data[data['Dyad'].isin(parejas)]
        data = data[data['Round'].isin(rondas)]

    # Keeping only relevant columns
    data = data[[#'Dyad',\
                 #'Round',\
                 #'Player',\
                 'Region',\
                 #'regionCoded',\
                 'Score',\
                 'RegionGo'\
                 ]]

    print("Done!")
    return data

def find_overlap(x, y, dict):
    tupla = (x, y)
    return dict[tupla]

def get_overlap(data):
    # Function to find overlapping tiles
    # Creating new column with Player1 or Player2
    print('Finding overlapping tiles...')
    dict = {}
    for dyad, grp in data[['Dyad', 'Player']].groupby('Dyad'):
        jugadores = list(grp.Player.unique())
        dict[jugadores[0]] = 'Player1'
        dict[jugadores[1]] = 'Player2'

    data['Players'] = data['Player'].map(dict)

    aux = pd.pivot_table(data, \
                         index=['Dyad', 'Round'],\
                         values='regionCoded',\
                         columns='Players',\
                         aggfunc='first')

    aux['Overlap'] = aux.apply(lambda x: set.intersection(set(x['Player1']), set(x['Player2'])), axis=1)
    aux['Overlap'] = aux['Overlap'].apply(lambda x: "".join(list(x)))
    # print("Pivot table:\n", aux)

    indices = aux.index
    dict ={}
    for i in range(len(indices)):
        llave = indices[i]
        valor = aux.loc[(indices[i]), :][2]
        # print("Llave:", llave)
        # print("Valor:", valor)
        dict[llave] = valor

    data['Overlap'] = data.apply(lambda x: find_overlap(x['Dyad'], x['Round'], dict), axis=1)
    del data['Players']
    print("Overlapping tiles found!")
    return data

def get_FRAsims(data):
    print("Obtainning FRASims...")

    regions, strategies = F.create_regions_and_strategies(Num_Loc)
    x = 0
    nombre = "FRASim" + F.nameRegion(x + 1)
    r = regions[x]
    data[nombre] = data.apply(lambda x: F.FRASim(F.code2Vector(F.lettercode2Strategy(x['regionCoded'], Num_Loc), Num_Loc), \
                                x['Overlap'],\
                                r,\
                                Num_Loc\
                                ),\
                            axis=1)
    return data

def get_freqs(data):

    data['Dummy'] = data['RegionGo'].apply(lambda x: F.numberRegion(x))
    df = pd.pivot_table(data,\
                        index=["Region", "Score"],\
                        columns="RegionGo",\
                        values="Dummy",\
                        aggfunc='count',\
                        fill_value=0).reset_index()
    # print(df.head())
    regiones = [x for x in ordenRegions if x in df.columns]
    df['Suma'] = df[regiones].apply('sum', axis=1)
    for c in regiones:
        df[c+'_o'] = df[c] / df['Suma']
    del df['Suma']
    return df

def graficar(df):
    fig = plt.figure()
    spec = gridspec.GridSpec(ncols=2, nrows=2)
    axes = []
    alpha = 0.5
    for c in ordenRegions:
        if c in df.Region.unique():
            grp = df.groupby('Region').get_group(c)
            if c == 'ALL':
                axes.append(fig.add_subplot(spec[0,0]))
                axes[-1].set_title('ALL')
                axes[-1].scatter(x=grp['Score'], y=grp['ALL_o'], alpha=alpha)
                axes[-1].scatter(x=grp['Score'], y=grp['A_e'], alpha=alpha, marker="x")
            if c == 'NOTHING':
                axes.append(fig.add_subplot(spec[0,1]))
                axes[-1].set_title('NOTHING')
                axes[-1].scatter(x=grp['Score'], y=grp['NOTHING_o'], alpha=alpha)
                axes[-1].scatter(x=grp['Score'], y=grp['N_e'], alpha=alpha, marker="x")
            if c == 'TOP':
                axes.append(fig.add_subplot(spec[1,0]))
                axes[-1].set_title('TOP')
                axes[-1].scatter(x=grp['Score'], y=grp['TOP_o'], alpha=alpha)
                axes[-1].scatter(x=grp['Score'], y=grp['T_e'], alpha=alpha, marker="x")
            if c == 'LEFT':
                axes.append(fig.add_subplot(spec[1,1]))
                axes[-1].set_title('LEFT')
                axes[-1].scatter(x=grp['Score'], y=grp['LEFT_o'], alpha=alpha, label="Observed")
                axes[-1].scatter(x=grp['Score'], y=grp['L_e'], alpha=alpha, marker="x", label="Estimated")

    handles, labels = axes[-1].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center')
    fig.subplots_adjust(hspace=0.5, wspace=0.5)
    plt.show()

def insert_WSLS(df, params):
    assert(len(params) == 7), "Error: incorrect number of parameters!" + str(len(params))
    df['Probs'] = df.apply(lambda x: F.probabilities_WSLS([0]*64,\
                                            F.numberRegion(x['Region']),\
                                            x['Score'], \
                                            [0]*64, \
                                            0, \
                                            params,\
                                            Num_Loc), axis=1)
    df['RS_e'] = df['Probs'].apply(lambda x: x[0])
    df['A_e'] = df['Probs'].apply(lambda x: x[1])
    df['N_e'] = df['Probs'].apply(lambda x: x[2])
    df['B_e'] = df['Probs'].apply(lambda x: x[3])
    df['T_e'] = df['Probs'].apply(lambda x: x[4])
    df['L_e'] = df['Probs'].apply(lambda x: x[5])
    df['R_e'] = df['Probs'].apply(lambda x: x[6])
    df['I_e'] = df['Probs'].apply(lambda x: x[7])
    df['O_e'] = df['Probs'].apply(lambda x: x[8])
    del df['Probs']
    return df

def logMultinom(row):
    probs = [row[c] for c in colsProbs]
    # print('probs:', probs)
    freqs = [row[c] for c in colsFreqs]
    # print('freqs:', freqs)
    n = np.sum(freqs)
    rv = multinomial(n,probs)
    dev = -2*rv.logpmf(freqs)
    # print('dev:', dev)
    return dev

def likelihoodFunction_WSLS(params):

    global data

    data = insert_WSLS(data, params)
    data['Dev'] = data.apply(lambda x: logMultinom(x), axis=1)
    return data['Dev'].sum()

def main():

    global data

    # Opens the file with data from DCL experiment into a Pandas DataFrame
    archivo = "../Data/Confusion/Simulations/WS2.csv"
    # archivo = "../Data/humans_only_absent.csv"
    print("Reading data...")
    data = pd.read_csv(archivo, index_col=False)
    print("Done!")
    print(data.head())

    data = trim_data(data, False)
    # data = get_overlap(data)
    # data = get_FRAsims(data)
    # print(data.head())
    data = get_freqs(data)
    print(data.head())

    WSLSParameters = [0.01, 0.01, 0.01, 0.01, 500, 1000, 31.5]
    dev = likelihoodFunction_WSLS(WSLSParameters)
    print(dev)

    x0 = np.array(WSLSParameters)
    res = minimize(likelihoodFunction_WSLS,\
                    x0,\
                    method='nelder-mead',\
                    options={'xatol': 1e-8, 'disp': True}
                    )

    # graficar(data)



main()
