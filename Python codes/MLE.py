# Functions to implement the Maximum Likelihood Parameter Estimation
# Edgar Andrade-Lotero 2020
print("loading packages...")
import pandas as pd
import FRA as F
print("Done!")

letras = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:'
Num_Loc = 8
cols = ['a' + str(i + 1) + str(j + 1)\
        for i in range(Num_Loc)\
        for j in range(Num_Loc)\
        ]

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

def trim_data(data):
    print("Trimming data...")

    # Codifying region into letter code
    data['regionCoded'] = data.apply(code_region, axis=1)

    # Keeping only relevant columns
    data = data[['Dyad',\
                 'Round',\
                 'Player',\
                 'Category',\
                 'regionCoded',\
                 'Score',\
                 'RegionGo'\
                 ]]

    # Minimal data frame TO BE COMMENTED FOR USE IN FULL DATA
    parejas = list(data.Dyad.unique())[:2]
    rondas = list(data.Round.unique())[:2]
    data = data[data['Dyad'].isin(parejas)]
    data = data[data['Round'].isin(rondas)]
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

def main():
    # Opens the file with data from DCL experiment into a Pandas DataFrame
    archivo = "../Data/Confusion/Simulations/MB0.csv"
    print("Reading data...")
    data = pd.read_csv(archivo, index_col=False)
    print("Done!")
    print(data.head())

    data = trim_data(data)
    data = get_overlap(data)
    data = get_FRAsims(data)

    print(data.head(3))


main()
