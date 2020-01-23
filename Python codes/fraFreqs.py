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

FRA.imprime_region(regions[5])

###########################################################
# FUNCTIONS
###########################################################

def find_FRAsimRIGHT(x):
	# Finds the similarity to RIGHT

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

	regFocal = regions[5]
	# FRA.imprime_region(regFocal)
	sim = float(round(FRA.FRASim(reg, joint, regFocal, Num_Loc), 4))
	return sim

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
# dataFULL = pd.read_csv('humans.csv')
print('Data loaded!')
# print(data[['Dyad', 'Player', 'Score']][:3])

# dyad = '435-261'
# data = pd.DataFrame(data.groupby('Dyad').get_group(dyad)).reset_index()
print('Including indexes...')
del dataFULL['index']
dataFULL['index'] = dataFULL.index
dict = {}
cols = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]
print('Working per dyad to find joint regions...')
for dyad, data in dataFULL.groupby('Dyad'):
	# print(data[['index', 'Player', 'Round']][:10])
	# Obtain joint region
	Players = list(data.Player.unique())
	# print('Players in dyad', dyad, ':', Players)
	print('Obtaining joint region dyad', dyad)
	for key, grp in data.groupby('Round'):
		grp1 = grp.groupby('Player').get_group(Players[0])
		grp2 = grp.groupby('Player').get_group(Players[1])
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

print('Finding similarity to RIGHT...')
dataFULL['FRASim'] = dataFULL.apply(lambda x: find_FRAsimRIGHT(x), axis=1)
# print(dataPL1[['index', 'SimFocalGO']])

data = dataFULL[['Dyad', 'Player', 'Round', 'Category', 'RegionGo', 'Score', 'FRASim']]
print(data[:3])

# print(dataPL1.columns)

outputFile = 'fraFreqs.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
