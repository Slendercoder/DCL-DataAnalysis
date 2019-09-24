# Get measures from simulation
# Measures are: accumulated score, normalized score, number of tiles visited,
# consistency, total of tiles visited dyad, difference in consistency, DLIndex,
# distance to closest focal path, and fairness.

CLASIFICAR = False

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

ifDistances = 1
ifClassify = 1

data = pd.read_csv(data_archivo, index_col=False)
print("Done!")
# print(data)

# --------------------------------------------------
# Parameters
# --------------------------------------------------
Num_Loc = 8

# --------------------------------------------------
# Obtaining measures from players' performance
# --------------------------------------------------
# Find the accumulated score
print("Finding accumulated score...")
data['Ac_Score'] = data.sort_values(['Dyad','Player']).groupby('Player')['Score'].cumsum()
# print(data)

# --------------------------------------------------
# Working only with trials with "Unicorn_Absent"
# --------------------------------------------------
print('Working only with trials with Unicorn_Absent')
try:
    data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent')).reset_index()
except:
    data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent'))

# print(data)

# --------------------------------------------------
# Continue obtaining measures
# --------------------------------------------------
Dyads = data.Dyad.unique()

# Find the normalized score
max_score = 32
min_score = -64 - 64
print("Finding normalized score...")
data['Norm_Score'] = data['Score'].apply(lambda x: (x - min_score) / (max_score - min_score))
# print(data)

# Find Size_visited
print("Finding Size_visited...")
cols = ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
# print('cols: ', cols)
data['Size_visited'] = data[cols].sum(axis=1)
# print(data[['Player', 'Round', 'Size_visited', 'Joint']][:10])
assert(all(data['Size_visited'] >= data['Joint']))
print("Sorting by Player...")
data = data.sort_values(['Player', 'Round'], \
                ascending=[True, True])
# print(data)

# Find consistency
print("Finding consistency...")
cols2 = ['a' + str(i + 1) + str(j + 1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
cols2Prima = ['b' + str(i + 1) + str(j + 1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
dts = []
for key, grp in data[cols2 + ['Player']].groupby(['Player']):
    # print("Processing player: ", key)
    aux1 = pd.DataFrame(grp[cols2])
    # print(aux1)
    aux11 = aux1.apply(lambda x: x.tolist(), axis=1)
    # print(aux11)
    aux2 = pd.DataFrame(grp[cols2].shift(1))
    # print(aux2)
    aux21 = aux2.apply(lambda x: x.tolist(), axis=1)
    # print(aux21)
    AAAA = pd.concat([aux11, aux21], axis=1)
    AAAA.columns = ['N', 'NSig']
    # print(AAAA)
    AAAA['Consistency'] = AAAA.apply(simINDataFrame, axis=1)
    # print(AAAA['Consistency'])
    dts.append(AAAA['Consistency'])

# rerer = pd.merge(dts[0], dts[1], left_index=True, right_index=True, how='outer')
rerer = pd.concat(dts, axis=1)
# print rerer.columns.values
columnas = []
nombres = list(rerer.columns.values)
# print nombres
for i in range(len(nombres)):
	columnas.append(str(i))
# print columnas
rerer.columns = columnas
# print rerer.columns.values
# print rerer.shape
rerer['Consistency'] = rerer['0']
for i in range(1, len(nombres)):
	rerer['Consistency'] = rerer['Consistency'].combine_first(rerer[str(i)])

data['Consistency'] = rerer['Consistency']
# print data

# Filling NA in Consistency
print("Filling NA in Consistency")
data['Consistency'] = data['Consistency'].fillna(1)


print("Sorting by Dyad, Player, Round...")
data = data.sort_values(['Dyad','Player','Round'], \
                ascending=[True, True, True])
# Find difference in consistency and Total_visited_dyad ----
print("Finding difference in consistency and Total_visited_dyad...")
# Find the tiles visited by each player ----
cols = ['Dyad','Player','Consistency','Round','Joint','Size_visited']
cols += ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
total = []
dif_cons = []
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
    # a = [np.where(aux1['a' + str(i + 1) + str(j + 1)] + aux2['a' + str(i + 1) + str(j + 1)] >= 1, 1, 0) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
    # a = [np.where(aux1['a' + str(i + 1) + str(j + 1)] + aux2['a' + str(i + 1) + str(j + 1)] >= 1, 1, 0) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
    # aux3 = sum(a)
    # assert(len(aux3) == len(aux1)), "Something wrong with the sums!"
    # for j in aux3:
    #     # print "j: " + str(j) + " comp. dist.: " + str(1-float(j)/Num_Loc)
    #     total.append(j)
    #     total.append(j)
    aux1['Total'] = aux1['Size_visited'] + aux2['Size_visited'] - aux1['Joint']
    assert(all(aux1['Joint'] == aux2['Joint']))
    assert(all(aux1['Size_visited'] >= aux1['Joint']))
    assert(all(aux2['Size_visited'] >= aux1['Joint']))
    assert(all(aux1['Total'] >= aux1['Size_visited']))
    # print "aux1['Total']: \n", aux1['Total']
    total += list(aux1['Total']) + list(aux1['Total'])
    # Finding difference between consistencies
    # print "La consistencia de uno es " + str(aux1['Consistency'])
    # print "La consistencia de otro es " + str(aux2['Consistency'])
    aux4 = np.absolute(aux1['Consistency'] - aux2['Consistency'])
    # print "La dif consistencia es " + str(aux4)
    # print "El total de locaciones visitas por los dos jugadores en" + \
    # "la pareja " + str(d) + " es: " + str(aux3)
    # preparing to add dif_cons
    for j in aux4:
        dif_cons.append(j)
        dif_cons.append(j)

print(str(len(data)) + " " + str(len(total)))
assert(len(total) == len(data)), "Something wrong with finding Size_visited!"
data['Total_visited_dyad'] = total
# #print data[:3]
assert(len(dif_cons) == len(data)), "Something wrong with finding dif_cons!"
data['Dif_consist'] = dif_cons
# print data['Dif_consist'][:3]

# Division of labor Index (Goldstone)
data['DLIndex'] = (data['Total_visited_dyad'] - data['Joint'])/(Num_Loc*Num_Loc)
assert(all(data['DLIndex'] >= 0))
data['DLIndex_Mean'] = data['DLIndex'].groupby(data['Dyad']).transform('mean')

if ifDistances == 1:
    # --------------------------------------------------
    # Finding max similarity per round, per player
    # --------------------------------------------------
    print("Finding max similarity to focal paths...")

    # Deterimining list of columns
    cols1 = ['a' + str(i) + str(j) \
    for i in range(1, Num_Loc + 1) \
    for j in range(1, Num_Loc + 1) \
    ]
    data['Similarity'] = data.apply(lambda x: maxSim2Focal(x[cols1]), axis=1)

if ifClassify == 1:
    # --------------------------------------------------
    # Classify region per round, per player
    # --------------------------------------------------
    print("Classifying regions...")

    # Deterimining list of columns
    cols1 = ['a' + str(i) + str(j) \
    for i in range(1, Num_Loc + 1) \
    for j in range(1, Num_Loc + 1) \
    ]

    data['Category'] = data.apply(lambda x: minDist2Focal(x[cols1]), axis=1)

# --------------------------------------------------
# Finding the lag variables
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
data['Category_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Category'].transform('shift', LAG)
if ifDistances == 1:
    data['Similarity_LAG1'] = data.groupby(['Dyad', 'Player'])\
                                ['Similarity'].transform('shift', LAG)


print("Sorting by Dyad, Player, Round...")
data = data.sort_values(['Dyad', 'Player', 'Round'], \
                ascending=[True, True, True])#.reset_index(drop=True)


outputFile = 'output.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
