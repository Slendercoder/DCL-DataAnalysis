# Get measures from simulation
# Measures are: accumulated score, normalized score, number of tiles visited,
# consistency, total of tiles visited dyad, difference in consistency, DLIndex,
# distance to closest focal path, and fairness.

# --------------------------------------------------
# Parameters (Global variables)
# --------------------------------------------------
Num_Loc = 8
CLASIFICAR = False

CONTINUO = False
CONTADOR = 1

regionsCoded = ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:', # ALL
                  '', # NOTHING
                  'GHIJKLMNOPQRSTUVWXYZ0123456789;:', # DOWN
                  'abcdefghijklmnopqrstuvwxyzABCDEF', # UP
                  'abcdijklqrstyzABGHIJOPQRWXYZ4567', # LEFT
                  'efghmnopuvwxCDEFKLMNSTUV012389;:', # RIGHT
                  'jklmnorstuvwzABCDEHIJKLMPQRSTUXYZ012', # IN
                  'abcdefghipqxyFGNOVW3456789;:' # OUT
                  ]


regions = ['RS', \
           'ALL', \
           'NOTHING', \
           'DOWN', \
           'UP', \
           'LEFT', \
           'RIGHT', \
           'IN', \
           'OUT']

letras = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:')

def nameRegion(r):
	if r == 0 or r == 9:
		return 'RS'
	elif r == 1:
		return 'ALL'
	elif r == 2:
		return 'NOTHING'
	elif r == 3:
		return 'DOWN'
	elif r == 4:
		return 'UP'
	elif r == 5:
		return 'LEFT'
	elif r == 6:
		return 'RIGHT'
	elif r == 7:
		return 'IN'
	elif r == 8:
		return 'OUT'

def obtainPresentBlocks(x):

    global CONTADOR

    valor = CONTADOR

    if x['Is_there'] == 'Unicorn_Present' and x['Is_there_LEAD'] == 'Unicorn_Absent':
        CONTADOR += 1

    if pd.isna(x['Is_there_LEAD']):
        CONTADOR += 1

    if x['Is_there'] == 'Unicorn_Present':
        return valor
    else:
        return 0

def nextScore(si, siLead, s, sLEAD):
    if si == 'Unicorn_Absent' and siLead == 'Unicorn_Present' and s == 32:
        return sLEAD
    else:
        return s

def calcula_consistencia(x, y):
    joint = np.multiply(x,y)
    total_visited = np.add(x,y)
    total_visited = total_visited.astype(float)
    total_visited = total_visited * 0.5
    total_visited = np.ceil(total_visited)
    j = np.sum(joint)
    t = np.sum(total_visited)
    if t != 0:
        return j/t
    else:
        return 1

def dibuja_region(reg, Num_Loc):
	assert(len(reg) == Num_Loc * Num_Loc), "Incorrect region size!"

	fig4, axes4 = plt.subplots()
	axes4.get_xaxis().set_visible(False)
	axes4.get_yaxis().set_visible(False)
	step = 1. / Num_Loc
	tangulos = []
	for j in range(0, Num_Loc * Num_Loc):
		x = int(j) % Num_Loc
		y = (int(j) - x) / Num_Loc
		# print("x: " + str(x + 1))
		# print("y: " + str(y + 1))
		by_x = x * step
		by_y = 1 - (y + 1) * step
		#     # print("by_x: " + str(by_x))
		#     # print("by_y: " + str(by_y))
		if reg[j] == 1:
			tangulos.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="black", alpha=1))

	for t in tangulos:
		axes4.add_patch(t)

	plt.show()

def code2Vector(cadena):
    # Returns the vector of 0s and 1s from a given code
    # Input: cadena, which is a string with the code in characters
    # Output: vector of 0s and 1s of lenght 64

    v = [0] * len(letras)
    cadena = list(cadena)
    for i in range(0, len(cadena)):
        index = np.min(np.where(np.array(letras) == cadena[i]))
        v[index] = 1

    return(v)

def dist(k, i):
    # Returns the distance between regions k and i
    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
    #        i, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing the distance between k and i

    k = np.array(k)
    i = np.array(i)
    dif = np.subtract(k, i)
    squares = np.multiply(dif, dif)
    distancia = np.sqrt(np.sum(squares))
    return(distancia)

def minDist2Focal(r):
    # Returns closest distance to focal region
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing the closest distance

    distances = [0] * 8
    contador = 0

    for k in regionsCoded:
        kV = code2Vector(k)
        # print('k:\n', k)
        distances[contador] = dist(list(r), kV)
        contador = contador + 1

    valor = np.min(np.array(distances))
    indiceMin = np.argmin(np.array(distances))
    # print('argmin:', indiceMin, 'vale?:', valor < np.sqrt(3))

    reg = regions[indiceMin + 1]
    if valor < np.sqrt(3):
        if reg == 'NOTHING' and valor > 0:
            return('RS')
        else:
            return(reg)
    elif valor < np.sqrt(4):
        if CLASIFICAR:
            dibuja_region(list(r), 8)
            print('Guess:', reg)
            correcto = raw_input('Correct? (1=YES/0=No): ')
            if correcto == '1':
                return(reg)
            else:
                print('Please input number of region:')
                regInput = raw_input('RS=0, ALL=1, NOTHING=2, DOWN=3, UP=4, LEFT=5, RIGHT=6, IN=7, OUT=8: ')
                regInput = int(regInput)
                assert(regInput in range(9)), "Incorrect region number!"
                return(regions[regInput])
        else:
            if reg == 'NOTHING':
                return('RS')
            else:
                return(reg)
    else:
        return('RS')

def simil(k, i, o):
    # Returns similarity between regions k and i
    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
    #        i, which is a region coded as a vector of 0s and 1s of length 64
    #        o, which is a parameter for the exponential
    # Output: number representing the similarity between k and i

    distance = dist(k, i)
    return(np.exp(- o * distance))

def maxSim2Focal(r):
    # Returns maximum similarity to focal region
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing the closest distance

    similarities = [0] * 8
    contador = 0

    for k in regionsCoded:
        kV = code2Vector(k)
        similarities[contador] = simil(r, kV, 1.2)
        contador = contador + 1

    valor = np.max(np.array(similarities))
    return(valor)

def simINDataFrame(x):
    return simil(x[0], x[1], 1)


######################################
######################################
######################################
######################################

print("loading packages...")
from sys import argv
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
print("Done!")

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")

script, data_archivo = argv

data = pd.read_csv(data_archivo, index_col=False)
print("Done!")
# print(data)

print("Sorting by Dyad, Player, Round...")
data = data.sort_values(['Dyad', 'Player', 'Round'], ascending=[True, True, True]).reset_index(drop=True)
# data.to_csv('output_Prev.csv', index=False)
data['Is_there_LEAD'] = data.groupby(['Dyad', 'Player'])['Is_there'].transform('shift', periods=-1)

# --------------------------------------------------
# Classify region per round, per player
# --------------------------------------------------
print("Classifying regions...")

# Deterimining list of columns
cols1 = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]
data['Category'] = data.apply(lambda x: minDist2Focal(x[cols1]), axis=1)

# --------------------------------------------------
# Correcting scores
# --------------------------------------------------
print('Correcting scores...')
data['Score'] = data['Score'].apply(int)
# print(data[['Dyad','Player','Round', 'Is_there', 'Score','Category']][:5])

# 1. Create column of indexes
data = data.reset_index()
data['indice'] = data.index

# # 2. Indices de comienzo de jugador
# indiceJugador = list(data.groupby('Player')['indice'].first())
# indiceJugador.sort()
# print('indiceJugador', indiceJugador)

# 2. Obtain indices of blocks of Unicorn_Present
data['Cambio'] = data.apply(obtainPresentBlocks, axis=1)
# print('List of blocks\n', data[['Player', 'Is_there', 'Cambio']][:30])

# 3. Obtain average score per group of Unicorn_Present
data['avScGrpUniPresent'] = data.groupby('Cambio')['Score'].transform('mean')
data['avScGrpUniPresent_LEAD'] = data.groupby(['Dyad', 'Player'])['avScGrpUniPresent'].transform('shift', -1)
# print('List of blocks\n', data[['Player', 'Is_there', 'Score', 'avScGrpUniPresent']][:30])

# 4. Correct score from last round absent to average score next block present
data['Score'] = data.apply(lambda x: nextScore(x['Is_there'], x['Is_there_LEAD'], x['Score'], x['avScGrpUniPresent_LEAD']), axis=1)
# print('List of blocks\n', data[['Player', 'Is_there', 'Score', 'Category', 'RegionGo']][:30])

# 5. Keep only rounds with Unicorn_Absent
data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent'))#.reset_index()
# print('List of blocks\n', data[['Player', 'Is_there', 'Score', 'Category', 'RegionGo']][:30])
print('Done!')

# --------------------------------------------------
# Obtaining measures from players' performance
# --------------------------------------------------
data['Score'] = data['Score'].map(lambda x: int(x), na_action='ignore')
# Find the accumulated score
print("Finding accumulated score...")
data['Ac_Score'] = data.sort_values(['Dyad','Player']).groupby('Player')['Score'].cumsum()
#
# Dyads = data.Dyad.unique()
#
# Find the normalized score
max_score = 32
min_score = -64 - 64
print("Finding normalized score...")
data['Norm_Score'] = (data['Score'] - min_score) / (max_score - min_score)
# print data
#
# Find Size_visited
print("Finding Size_visited...")
cols = ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
# print('cols: ', cols)
data['Size_visited'] = data[cols].sum(axis=1)
# print(data[['Player', 'Round', 'Size_visited', 'Joint']][:10])
# assert(all(data['Size_visited'] >= data['Joint']))
# #
# print("Sorting by Player...")
# data = data.sort_values(['Player', 'Round'], \
#                 ascending=[True, True])
#
# Find consistency
print("Finding consistency...")
# # print data[:10]
cols2 = ['a' + str(i + 1) + str(j + 1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
data['Vector'] = data.apply(lambda x: np.array(x[cols]), axis=1)
data['VectorLAG1'] = data.groupby(['Dyad', 'Player'])['Vector'].transform('shift', 1)
data = data.dropna()
data['Consistency'] = data.apply(lambda x: calcula_consistencia(x['Vector'], x['VectorLAG1']), axis=1)
del data['VectorLAG1']

# Find difference in consistency and Total_visited_dyad ----
print("Finding difference in consistency and Total_visited_dyad...")
cols = ['Dyad','Player','Consistency','Round','Joint','Size_visited']
total = {}
dif_cons = {}
for key, grp in data[cols].groupby(['Dyad']):
	Players = grp.Player.unique()
	# print "The players in dyad " + str(key) + " are: " + str(Players)
	Grp_player = grp.groupby(['Player'])
	aux1 = pd.DataFrame(Grp_player.get_group(Players[0])).reset_index()
	# print "aux1: \n", aux1
	aux2 = pd.DataFrame(Grp_player.get_group(Players[1])).reset_index()
	# print "aux2: \n", aux2
	# print "len(aux1)", len(aux1)
	assert(len(aux1) == len(aux2)), "Something wrong with players!"
	assert(all(aux1['Joint'] == aux2['Joint'])), "Something wrong with players!"
	aux3 = pd.DataFrame({'Dyad':aux1['Dyad'],\
	'Round':aux1['Round'],\
	'C1':aux1['Consistency'],\
	'C2':aux2['Consistency'],\
	'V1':aux1['Size_visited'],\
	'V2':aux2['Size_visited'],\
	'Joint':aux1['Joint']})
	aux3['total_visited'] = aux3.apply(lambda x: x['V1'] + x['V2'] - x['Joint'], axis=1)
	aux3['Dif_consist'] = aux3.apply(lambda x: np.abs(x['C1'] - x['C2']), axis=1)
	aux3['Pair'] = aux3.apply(lambda x: tuple([x['Dyad'], x['Round']]), axis=1)
	total1 = dict(zip(aux3.Pair, aux3.total_visited))
	total = {**total, **total1}
	dif_cons1 = dict(zip(aux3.Pair, aux3.Dif_consist))
	dif_cons = {**dif_cons, **dif_cons1}

data['Pair'] = data.apply(lambda x: tuple([x['Dyad'], x['Round']]), axis=1)
data['Total_visited_dyad'] = data['Pair'].map(total)
data['Dif_consist'] = data['Pair'].map(dif_cons)
del data['Vector']
del data['Pair']

# Division of labor Index (Goldstone)
data['DLIndex'] = (data['Total_visited_dyad'] - data['Joint'])/(Num_Loc*Num_Loc)
assert(all(data['DLIndex'] >= 0))

# --------------------------------------------------
# Finding distance to closest focal region per round, per player
# --------------------------------------------------
print("Finding distances to focal paths...")

# Deterimining list of columns
cols1 = ['a' + str(i) + str(j) \
for i in range(1, Num_Loc + 1) \
for j in range(1, Num_Loc + 1) \
]

data['Similarity'] = data.apply(lambda x: maxSim2Focal(x[cols1]), axis=1)

# --------------------------------------------------
# Finding the lag and lead variables
# --------------------------------------------------
LAG = 1

print("Finding the lag variables...")
data['Norm_Score_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Norm_Score'].transform('shift', LAG)
data['Consistency_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Consistency'].transform('shift', LAG)
data['Dif_consist_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Dif_consist'].transform('shift', LAG)
data['Joint_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Joint'].transform('shift', LAG)
data['RegionGo'] = data.groupby(['Dyad', 'Player'])\
                            ['Category'].transform('shift', -LAG)
data['Similarity_LAG1'] = data.groupby(['Dyad', 'Player'])\
                        ['Similarity'].transform('shift', LAG)

outputFile = 'output.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
