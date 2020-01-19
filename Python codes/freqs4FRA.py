# Data analysis for "Seeking the unicorn" task
# This code obtains the database to use for parameter recovery
# for the FRA model
# Edgar Andrade-Lotero 2020
# Run with Python 3

print('Importing packages...')
import pandas as pd
import numpy as np
import FRA
from sys import argv
print("Done!")

###########################################################
# GLOBAL VARIABLES
###########################################################

Num_Loc = 8
regions, strategies = FRA.create_regions_and_strategies(Num_Loc)

###########################################################
# FUNCTIONS
###########################################################

def find_FRAsim(x):
	# Finds the similarity to RegionGo

	global regions

	categoria_nombre = x['RegionGo']
	# print('categoria', categoria_nombre)
	categoria_numero = FRA.numberRegion(categoria_nombre)
	# print('categoria', categoria_numero)

	if categoria_numero == None:
		return np.nan

	cols = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]
	reg = list(x[cols])
	# print('Region')
	# FRA.imprime_region(reg)

	joint = x['JointRegion']
	joint = FRA.lettercode2Strategy(joint, Num_Loc)
	joint = FRA.code2Vector(joint, Num_Loc)
	# print('Joint')
	# FRA.imprime_region(joint)

	if categoria_nombre == 'RS':
		return float(round(FRA.maxFRASim(reg, joint), 4))
	else:
		regFocal = regions[categoria_numero - 1]
		# FRA.imprime_region(regFocal)
		sim = float(round(FRA.FRASim(reg, joint, regFocal), 4))
		return sim

def vector2Code(v):
	# Returns the coded vector out of a 64-bite region

	# letras = [chr(x) for x in range(65, 129)]
	letras = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:')
	a = ''
	assert(len(v) == 64), 'Incorrect argument! Must be a 64-bite region'
	for x in range(len(v)):
		# print('x', x, 'v[x]', v[x])
		if v[x] == 1:
			# print('letras[x]', letras[x])
			a += letras[x]

	return a

###########################################################
# INSTRUCTIONS
###########################################################

print("Reading data...")
script, data_archivo = argv
dataFULL = pd.read_csv(data_archivo, index_col=False)
dataFULL['Dyad'] = dataFULL.apply(lambda x: str(x['Dyad']), axis=1)
del dataFULL['index']
dataFULL['index'] = dataFULL.index
print(dataFULL[['index', 'Dyad', 'Player', 'Round']][:10])
print("Done!")

cols = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]

# Code Region
print('Coding regions (please be patient!)...')
dataFULL['RegionCoded'] = dataFULL.apply(lambda x: vector2Code(x[cols]), axis=1)
print("Done!")

# Obtain joint region
dict = {}
print('Working per dyad...')
for dyad, data in dataFULL.groupby('Dyad'):
	# print(data[['index', 'Player', 'Round']][:3])
	# Obtain joint region
	# print('Obtaining joint region dyad', dyad)
	players = data.Player.unique()
	# print('Players', players)
	PL1 = players[0]
	PL2 = players[1]
	for key, grp in data.groupby('Round'):
		grp1 = grp.groupby('Player').get_group(PL1)
		grp2 = grp.groupby('Player').get_group(PL2)
		reg1 = FRA.list_from_row(grp1, cols)
		reg2 = FRA.list_from_row(grp2, cols)
		# print('\nRonda', key)
		# print('Region 1')
		# FRA.imprime_region(reg1)
		# print('Region 2')
		# FRA.imprime_region(reg2)
		joint = [reg1[i]*reg2[i] for i in range(len(reg1))]
		# FRA.imprime_region(joint)
		joint = vector2Code(joint)
		# print(joint)
		dict[grp1.index[0]] = joint
		dict[grp2.index[0]] = joint

# print(dict)
dataFULL['JointRegion'] = dataFULL['index'].map(dict)
# print(data[['index', 'Player', 'JointRegion']][:10	])
print('Done!')

data = dataFULL[['Dyad', 'Player', 'Round', 'Category', 'RegionCoded', 'RegionGo', 'Score', 'JointRegion']]
print(data[:3])

outputFile = 'freqs4FRA.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)
