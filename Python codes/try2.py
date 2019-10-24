import numpy as np
import pandas as pd

Num_Loc = 8
SUM_SCORE = 0
N_OBS = 0
AV_SCORE = 0
CONTINUO = False
indicesIncluir = []

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

def nextRegion(si, s, r, i):
	global SUM_SCORE
	global N_OBS
	global AV_SCORE
	global CONTINUO
	global indicesIncluir
	if si == 'Unicorn_Present':
		SUM_SCORE += s
		N_OBS += 1
		AV_SCORE = SUM_SCORE/N_OBS
		CONTINUO = True
		return r
	else:
		if CONTINUO:
			# print('r', r)
			# print('AV_SCORE', AV_SCORE)
			# print('i', i)
			# print('SUM_SCORE', SUM_SCORE)
			if AV_SCORE > 30:
				SUM_SCORE = 0
				N_OBS = 0
				indicesIncluir.append([i, r])
				CONTINUO = False
				return r
			else:
				SUM_SCORE = 0
				N_OBS = 0
				indicesIncluir.append([i, 'RS'])
				CONTINUO = False
				return r
		else:
			return r

# Function to insert row in the dataframe
def Insert_row(row_number, df, row_value):
    # Starting value of upper half
    start_upper = 0

    # End value of upper half
    end_upper = row_number

    # Start value of lower half
    start_lower = row_number

    # End value of lower half
    end_lower = df.shape[0]

    # Create a list of upper_half index
    upper_half = [*range(start_upper, end_upper, 1)]

    # Create a list of lower_half index
    lower_half = [*range(start_lower, end_lower, 1)]

    # Increment the value of lower half by 1
    lower_half = [x.__add__(1) for x in lower_half]

    # Combine the two lists
    index_ = upper_half + lower_half

    # Update the index of the dataframe
    df.index = index_

    # Insert a row at the end
    df.loc[row_number] = row_value

    # Sort the index labels
    df = df.sort_index()

    # return the dataframe
    return df

# data = pd.read_csv('output1.csv')
data = pd.read_csv('output_Prev.csv')
data = data.dropna()

print(data[:3])

print('Correcting scores...')
# # 1. Create column of indexes
# data = data.reset_index()
# data['indice'] = data.index
# # 2. Obtain number of columns for Is_there and Category
# columnas = list(data.columns)
# Is_there_index = columnas.index('Is_there')
# # print('Is_there_index', Is_there_index)
# Strategy_index = columnas.index('Category')
# # print('Is_there_index', Strategy_index)
#
# # Indices de comienzo de jugador
# indiceJugador = []
# for key, grp in data.groupby('Player'):
# 	indiceJugador.append(list(grp['indice'])[0])
# 	# print(grp[['indice', 'Is_there', 'Score', 'Category']])
# 	# 3. Obtain indices from Unicorn_Absent after block of Unicorn_Present
# 	# 4. Estimate region based on average score
# 	grp.apply(lambda x: nextRegion(x['Is_there'], x['Score'], x['Category'], x['indice']), axis=1)
# 	# print('List of blocks', indicesIncluir)
#
# # print('indiceJugador', indiceJugador)
#
# # 5. Include new row of Unicorn_Absent with estimated region and previous score
# for k in range(len(indicesIncluir)):
#     c = indicesIncluir[len(indicesIncluir) - k - 1]
#     if c[0] not in indiceJugador:
#         row_number = c[0]
#         row_value = [x for x in data.loc[row_number - 1]]
#         # print(row_value)
#         row_value[Is_there_index] = 'Unicorn_Absent'
#         row_value[Strategy_index] = c[1]
#         # print(row_value)
#         data = Insert_row(row_number, data, row_value)

# 6. Obtaining score from previous round
data['lagScore'] = data.groupby(['Dyad', 'Player'])\
                            ['Score'].transform('shift', periods=1)
# # print(data[['indice', 'Is_there', 'Score', 'Category', 'lagScore']])
# 7. Keep only rounds with Unicorn_Absent
data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent')).reset_index()
# finding corrected scores (part 2)
# 8. Obtaining lagScore from next round
data['Score'] = data.groupby(['Dyad', 'Player'])\
                            ['lagScore'].transform('shift', periods=-1)
# print(data[['Is_there', 'Score', 'Category']])
data = data.dropna()
print('Done!')

# --------------------------------------------------
# Obtaining measures from players' performance
# --------------------------------------------------
data['Score'] = data['Score'].map(lambda x: int(x))
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
#
print("Sorting by Player...")
data = data.sort_values(['Player', 'Round'], \
                ascending=[True, True])

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
	# print("The players in dyad " + str(key) + " are: " + str(Players))
	Grp_player = grp.groupby(['Player'])
	aux1 = pd.DataFrame(Grp_player.get_group(Players[0])).reset_index()
	# print("aux1: \n", aux1)
	aux2 = pd.DataFrame(Grp_player.get_group(Players[1])).reset_index()
	# print("aux2: \n", aux2)
	# print("len(aux1)", len(aux1), "len(aux2)", len(aux2))
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

# # --------------------------------------------------
# # Finding distance to closest focal region per round, per player
# # --------------------------------------------------
# print("Finding distances to focal paths...")
#
# # Deterimining list of columns
# cols1 = ['a' + str(i) + str(j) \
# for i in range(1, Num_Loc + 1) \
# for j in range(1, Num_Loc + 1) \
# ]
#
# data['Similarity'] = data.apply(lambda x: self.maxSim2Focal(x[cols1], self.regions, 1), axis=1)
# #
# # # cols = cols1 + ['Player', 'Round']
# # #
# # # print("Sorting by Player, Round...")
# # # data = data.sort_values(['Player', 'Round'], \
# # # ascending=[True, True]).reset_index()
# # #
# # # distancias = []
# # # for player, Grp in data[cols].groupby(['Player']):
# # # 	print("Working with player " + str(player) + "...")
# # # 	for ronda, grp in Grp.groupby(['Round']):
# # # 		# print "Obtaining path from round " + str(ronda) + "..."
# # # 		path = [int(list(grp[c])[0]) for c in cols1]
# # # 		# print path
# # # 		# print "finding region..."
# # # 		minDist = self.maxSim2Focal(path, self.regions, 0.3)
# # # 		# print "Min: " + str(minDist)
# # # 		distancias.append(minDist)
# # #
# # # print('len distancias', len(distancias))
# # # print('len data[Distacias]', len(data))
# # # data['Similarity'] = distancias
# #
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
                            ['Category'].transform('shift', -1)
# data['Similarity_LAG1'] = data.groupby(['Dyad', 'Player'])\
#                         ['Similarity'].transform('shift', LAG)


data.to_csv('output1.csv', index=False)
