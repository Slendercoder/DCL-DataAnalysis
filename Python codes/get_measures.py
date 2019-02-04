# Get measures from simulation
# Measures are: accumulated score, normalized score, number of tiles visited,
# consistency, total of tiles visited dyad, difference in consistency, DLIndex,
# distance to closest focal path, and fairness.

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
        distances[contador] = dist(r, kV)
        contador = contador + 1

    valor = np.min(np.array(distances))
    return(valor)

def simil(k, i, o):
    # Returns similarity between regions k and i
    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
    #        i, which is a region coded as a vector of 0s and 1s of length 64
    #        o, which is a parameter for the exponential
    # Output: number representing the similarity between k and i

    distance = dist(k, i)
    return(np.exp(- o * distance))

def classifyRegion(r, o, d):
    # Returns the code of the region that is closer to r
    # Input: r, region coded as a vector of 0s and 1s of length 64
    #        o, which is a parameter for the similarity function
    #        d, which is a parameter for threshold
    # Output: string, which is a region code

    similarities = [0] * 8
    contador = 0

    for k in regionsCoded:
        kV = code2Vector(k)
        similarities[contador] = simil(r, kV, o)
        contador = contador + 1

    # print('similarities')
    # print(similarities)
    suma = np.sum(similarities)
    if (any(np.array(similarities) > d)):
        probs = [x/suma for x in similarities]
        # print('probs')
        # print(probs)
        # print('max:', np.argmax(np.array(probs)))
        index = np.argmax(np.array(probs))
        # print('index: ', index)
        chosen = regions[index + 1]
        # print('chosen: ', chosen)
        return(chosen)
    else:
        return('RS')

print("loading packages...")
from sys import argv
import numpy as np
import pandas as pd
print("Done!")

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")

script, data_archivo = argv

ifDistances = input("Do you want to calculate distance to closest focal path? (1=Yes/0=No): ")
ifDistances = int(ifDistances)
# ifDistances = 1

ifSimulation = input("Data comes from simulation? (1=Yes/0=No): ")
ifSimulation = int(ifSimulation)
# ifSimulation = 0 # 1 if data comes from computer model

data = pd.read_csv(data_archivo, index_col=False)
print("Data read!")

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
# print data

# --------------------------------------------------
# Working only with trials with "Unicorn_Absent"
# --------------------------------------------------
try:
    data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent')).reset_index()
except:
    data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent'))

# --------------------------------------------------
# Continue obtaining measures
# --------------------------------------------------
Dyads = data.Dyad.unique()

lenghtExps = []
try:
    for key, grp in data[['Exp', 'Dyad']].groupby(['Exp']):
        lenghtExps.append(len(grp.Dyad.unique()))

    assert(len(set(lenghtExps)) == 1), "Some dyads are missing!"
except:
    print("No Exp column. Data may come from human performances, but check anyway!")


# Find the normalized score
max_score = 32
min_score = -64 - 64
print("Finding normalized score...")
data['Norm_Score'] = (data['Score'] - min_score) / (max_score - min_score)
# print data

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
# Find consistency
print("Finding consistency...")
# print data[:10]
cols2 = ['a' + str(i + 1) + str(j + 1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
dts = []
cols22 = ['Inters-' + c for c in cols2]
# print data[cols22]
cols222 = ['Unions-' + c for c in cols2]
# print data[cols222]
for key, grp in data[cols2 + ['Player']].groupby(['Player']):
	# print "Processing player: ", key
	aux1 = pd.DataFrame(np.floor(grp[cols2].rolling(2).mean()))
	aux2 = pd.DataFrame(np.ceil(grp[cols2].rolling(2).mean()))
	AAAA = pd.concat([aux1, aux2], axis=1)
	AAAA.columns = cols22 + cols222
	AAAA['Inters'] = AAAA[cols22].sum(axis=1)
	AAAA['Unions'] = AAAA[cols222].sum(axis=1)
	AAAA['Consistency'] = AAAA['Inters']/AAAA['Unions']
	# print AAAA['Consistency']
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
data['Consistency'] = data['Consistency'].fillna(1)
# data['Consistency'] = data['Consistency'].fillna(0)
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

# Find fairness between players' split of the grid ----
print("Finding fairness...")
# Find the squares visited by both players ----
cols = ['Dyad','Player','Size_visited']
Grps_Dyad = data[cols].groupby(['Dyad'])
Dyads = data.Dyad.unique()
data['Fairness'] = np.nan
print("Data primeros 3\n", data[:3])
for d in Dyads:
	grp = Grps_Dyad.get_group(d)
	Players = grp.Player.unique()
	# print "The players in dyad " + str(d) + " are: " + str(Players)
	Grp_player = grp.groupby(['Player'])
	aux1 = pd.DataFrame(Grp_player['Size_visited'].get_group(Players[0]))
	aux2 = pd.DataFrame(Grp_player['Size_visited'].get_group(Players[1]))
	# print "Here we find the fairness"
	aux1['Size_visited'] = aux1.Size_visited.astype(float)
	# print "aux1\n", aux1['Size_visited'][:3]
	# print "aux2\n", aux2['Size_visited'][:3]
	se = pd.Series(list(aux2['Size_visited'])) # capturo los valores del otro jugador
	aux1['Fairness'] = 1 - np.absolute(aux1['Size_visited'] - se.values)/(Num_Loc * Num_Loc)
	# print "aux1\n", aux1
	se = pd.Series(list(aux1['Fairness'])) # duplico los valores de fairnes del primer jugador
	aux2['Fairness'] = se.values # y se los pego al segundo
	# print "aux2\n", aux2
	fairness = pd.concat([aux1, aux2], axis=1)
	columnas = ['a', 'f1', 'b', 'f2']
	fairness.columns = columnas
	# print "fairness primeros 3\n", fairness[:3]
	fairness['f1'] = fairness['f1'].combine_first(fairness['f2'])
	# print "fairness primeros 3\n", fairness[:3]
	# print "fairness 60 a 63", fairness[60:63]
	# print fairness.shape
	data['Fairness'] = data['Fairness'].combine_first(fairness['f1'])
	# print "Data\n", data['Fairness']

if ifDistances == 1:
    # --------------------------------------------------
    # Finding distance per round, per player
    # --------------------------------------------------
    print("Finding distances to focal paths...")

    # Deterimining list of columns
    cols1 = ['a' + str(i) + str(j) \
            for i in range(1, Num_Loc + 1) \
            for j in range(1, Num_Loc + 1) \
            ]
    cols = cols1 + ['Player', 'Round']

    print("Sorting by Player, Round...")
    try:
        data = data.sort_values(['Player', 'Round'], \
                    ascending=[True, True]).reset_index()
    except:
        data = data.sort_values(['Player', 'Round'], \
                    ascending=[True, True])

    distancias = []
    estrategias = []
    for player, Grp in data[cols].groupby(['Player']):
        print("Working with player " + str(player) + "...")
        for ronda, grp in Grp.groupby(['Round']):
            # print("Obtaining path from round " + str(ronda) + "...")
            path = [int(list(grp[c])[0]) for c in cols1]
            minDist = round(minDist2Focal(path), 2)
            distancias.append(minDist)
            strat = classifyRegion(path, 0.3, 0.55)
            estrategias.append(strat)

    data['Distancias'] = distancias

if ifSimulation == 1:
    dictionary = dict((i,j) for i,j in enumerate(regions))
    data['Strategy'] = data['Strategy'].map(dictionary)
else:
    print(ifSimulation)
    data['Strategy'] = estrategias

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
data['Distancias_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Distancias'].transform('shift', LAG)
data['Strategy_LAG1'] = data.groupby(['Dyad', 'Player'])\
                            ['Strategy'].transform('shift', LAG)

print("Sorting by Dyad, Player, Round...")
data = data.sort_values(['Dyad', 'Player', 'Round'], \
                ascending=[True, True, True])#.reset_index(drop=True)


outputFile = 'output.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
