print('Importing packages...')
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches

###########################################################
# GLOBAL VARIABLES
###########################################################

gameParameters = []
modelParameters = []
regionsCoded = []
strategies = []
Num_Loc = 8

DEB = True
IMPR = False

###########################################################
# FUNCTIONS
###########################################################

def imprime_region(r):

	print(r[0:8])
	print(r[8:16])
	print(r[16:24])
	print(r[24:32])
	print(r[32:40])
	print(r[40:48])
	print(r[48:56])
	print(r[56:64])

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

def numberRegion(r):
	if r == 'RS':
		return 0
	elif r == 'ALL':
		return 1
	elif r == 'NOTHING':
		return 2
	elif r == 'DOWN':
		return 3
	elif r == 'UP':
		return 4
	elif r == 'LEFT':
		return 5
	elif r == 'RIGHT':
		return 6
	elif r == 'IN':
		return 7
	elif r == 'OUT':
		return 8

def create_regions_and_strategies(Num_Loc):
	size = int(Num_Loc * Num_Loc)
	half_size = int(Num_Loc * Num_Loc / 2)
	half_Num_Loc = int(Num_Loc / 2)

	# ALL and NOTHING
	all = [1] * size
	nothing = [0] * size
	# print('ALL ', all)
	# print('NOTHING ', nothing)

	# UP and DOWN
	up = [1] * half_size + [0] * half_size
	down = [1 - i for i in up]
	# print('DOWN ', down)
	# print('UP ', up)

	# LEFT and RIGHT
	right = []
	for i in range(0, Num_Loc):
		right += [0] * half_Num_Loc + [1] * half_Num_Loc

	left = [1 - i for i in right]
	# print('LEFT ', left)
	# print('RIGHT ', right)

	# IN and OUT
	In = [0] * Num_Loc
	for i in range(Num_Loc - 2):
		In += [0] + [1] * (Num_Loc - 2) + [0]

	In += [0] * Num_Loc

	out = [1 - i for i in In]

	# print('IN ', In)
	# print('OUT ', out)

	# Create a set of n pairwise disjoint paths in the board

	# Define the strategies
	UP = []
	DOWN = []
	LEFT = []
	RIGHT = []
	IN = []
	OUT = []
	ALL = []
	NOTHING = []

	for i in range(int(Num_Loc * Num_Loc)):
		if up[i] == 1:
			UP.append(i)
		if down[i] == 1:
			DOWN.append(i)
		if left[i] == 1:
			LEFT.append(i)
		if right[i] == 1:
			RIGHT.append(i)
		if all[i] == 1:
			ALL.append(i)
		if nothing[i] == 1:
			NOTHING.append(i)
		if In[i] == 1:
			IN.append(i)
		if out[i] == 1:
			OUT.append(i)

	strategies = {}

	strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
	while len(strategies[0]) < 2 or len(strategies[0]) > 62:
	       strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

	strategies[1] = ALL
	strategies[2] = NOTHING
	strategies[3] = DOWN
	strategies[4] = UP
	strategies[5] = LEFT
	strategies[6] = RIGHT
	strategies[7] = IN
	strategies[8] = OUT
	strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
	while len(strategies[9]) < 2 or len(strategies[9]) > 62:
	       strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

	return [all, nothing, down, up, left, right, In, out], strategies

def dibuja_region(reg, Num_Loc):

	assert(len(reg) == Num_Loc * Num_Loc), "Incorrect region size!"

	print(reg)

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

def dibuja_regiones(reg1, reg2, Num_Loc, titulo):
	assert(len(reg1) == Num_Loc * Num_Loc), "Incorrect region size 1!"
	assert(len(reg2) == Num_Loc * Num_Loc), "Incorrect region size 2!"

	fig4, axes4 = plt.subplots(1,2)
	for a in axes4:
		a.get_xaxis().set_visible(False)
		a.get_yaxis().set_visible(False)
	step = 1. / Num_Loc
	tangulos1 = []
	tangulos2 = []
	for j in range(0, Num_Loc * Num_Loc):
		x = int(j) % Num_Loc
		y = (int(j) - x) / Num_Loc
		# print("x: " + str(x + 1))
		# print("y: " + str(y + 1))
		by_x = x * step
		by_y = 1 - (y + 1) * step
		#     # print("by_x: " + str(by_x))
		#     # print("by_y: " + str(by_y))
		if reg1[j] == 1:
			tangulos1.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="black", alpha=1))
		if reg2[j] == 1:
			tangulos2.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="black", alpha=1))
		if reg1[j] == 1 and reg2[j] == 1:
			tangulos1.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="red", alpha=1))
			tangulos2.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="red", alpha=1))

	for t in tangulos1:
		axes4[0].add_patch(t)

	for t in tangulos2:
		axes4[1].add_patch(t)

	fig4.suptitle(titulo)
	plt.show()

# define attractiveness and choice functions
def sigmoid(x, beta, gamma):
	return 1. / (1 + np.exp(-beta * (x - gamma)))

def code2Vector(strategy):

    global gameParameters

    Num_Loc = gameParameters[2]
    size = int(Num_Loc * Num_Loc)
    v = [0] * size

    for i in range(size):
        if i in strategy:
            v[i] = 1

    return v

def simil(k, i, o):
    # Returns similarity between regions k and i
    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
    #        i, which is a region coded as a vector of 0s and 1s of length 64
    #        o, which is a parameter for the exponential
    # Output: number representing the similarity between k and i

    # print('k')
    # imprime_region(k)
    # print('i')
    # imprime_region(i)

    k = np.array(k)
    i = np.array(i)
    dif = np.subtract(k, i)
    # print('dif', dif)
    squares = np.multiply(dif, dif)
    # print('squares', squares)
    distance = np.sqrt(np.sum(squares))
    # distance = np.sum(squares)
    # print('distance', distance)

    return(np.exp(- o * distance))

def dist(k, i):
    # Returns similarity between regions k and i
    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
    #        i, which is a region coded as a vector of 0s and 1s of length 64
    #        o, which is a parameter for the exponential
    # Output: number representing the similarity between k and i

    k = np.array(k)
    i = np.array(i)
    dif = np.subtract(k, i)
    squares = np.multiply(dif, dif)
    return(np.sqrt(np.sum(squares)))

def sim_consist(v1, v2):
	# Returns the similarity based on consistency
	# v1 and v2 are two 64-bit coded regions

	assert(len(v1) == 64), 'v1 must be a 64-bit coded region!'
	assert(len(v2) == 64), 'v2 must be a 64-bit coded region!'

	joint = [v1[x] * v2[x] for x in range(len(v1))]
	union = [v1[x] + v2[x] for x in range(len(v1))]
	union = [x/x for x in union if x != 0]
	j = np.sum(np.array(joint))
	u = np.sum(np.array(union))
	return float(j)/u

def maxSim2Focal(r, regionsCoded, eta):
    # Returns closest distance to focal region
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing the closest distance

    distances = [0] * 8
    contador = 0

    for k in regionsCoded:
        # kV = self.code2Vector(k)
        distances[contador] = self.simil(r, k, eta)
        contador = contador + 1

    valor = np.max(np.array(distances))
    return(valor)

def minDist2Focal(r, regionsCoded):
	# Returns closest distance to focal region
	# Input: r, which is a region coded as a vector of 0s and 1s of length 64
	# Output: number representing the closest distance

	distances = [0] * 8
	contador = 0

	# print('r:\n', list(r))
	for k in regionsCoded:
		# kV = self.code2Vector(k)
		# print('k:\n', k)
		distances[contador] = self.dist(list(r), k)
		contador = contador + 1

	# dist_print = ["%.3f" % v for v in distances]
	# print('distancias:\n', dist_print)

	valor = np.min(np.array(distances))
	indiceMin = np.argmin(np.array(distances))
	# print('argmin:', indiceMin, 'vale?:', valor < np.sqrt(3))

	if valor < np.sqrt(TOLERANCIA):
		reg = nameRegion(indiceMin + 1)
		return(reg)
	else:
		return('RS')

    # valor = np.min(np.array(distances))
    # return(valor)

def probabilities(iV, i, score, j, pl):

	global modelParameters

	if pl == 0:
		wALL = float(modelParameters[0])
		wNOTHING = float(modelParameters[1])
		wDOWN = float(modelParameters[2])
		wUP = float(modelParameters[2])
		wLEFT = float(modelParameters[2])
		wRIGHT = float(modelParameters[2])
		wIN = float(modelParameters[3])
		wOUT = float(modelParameters[3])
		alpha = float(modelParameters[4]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[5]) # amplitude of the WSLS sigmoid function
		gamma = float(modelParameters[6]) # position of the WSLS sigmoid function
		delta = float(modelParameters[7]) # for how much the added FRA similarities augments attractiveness
		epsilon = float(modelParameters[8]) # amplitude of the FRA sigmoid function
		zeta = float(modelParameters[9]) # position of the FRA sigmoid function
	else:
		wALL = float(modelParameters[10])
		wNOTHING = float(modelParameters[11])
		wDOWN = float(modelParameters[12])
		wUP = float(modelParameters[12])
		wLEFT = float(modelParameters[12])
		wRIGHT = float(modelParameters[12])
		wIN = float(modelParameters[13])
		wOUT = float(modelParameters[13])
		alpha = float(modelParameters[14]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[15]) # amplitude of the WSLS sigmoid function
		gamma = float(modelParameters[16]) # position of the WSLS sigmoid function
		delta = float(modelParameters[17]) # for how much the added FRA similarities augments attractiveness
		epsilon = float(modelParameters[18]) # amplitude of the FRA sigmoid function
		zeta = float(modelParameters[19]) # position of the FRA sigmoid function

	wRS = 1 - np.sum(np.array([wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]))
	assert(wRS > 0), "Incorrect biases!"
	bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
	# biasPrint = ["%.3f" % v for v in bias]
	# print('bias: ', biasPrint)

	# regionsCoded = regions
	# strategies = strategies

	# iV = code2Vector(strategies[i])
	# print('iV')
	# imprime_region(iV)
	# print('i', i)
	# if i==9: i = 0

	attractiveness = [x for x in bias] # start from bias
	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('attractiveness before WS and FRA\n', attactPrint)

	# n = (score + 128) / 160 # normalizing score
	n = score

	# Adding 'Win Stay'
	if i != 0:
	          attractiveness[i] += alpha * sigmoid(n, beta, gamma)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('attractiveness with WS\n', attactPrint)

	# Calculating similarity to region
	simils1 = [0] * 9
	for k in range(1,9): # do not consider 'rs'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		# similarity = simil(iV, kCoded, eta)
		similarity = sim_consist(iV, kCoded)
		# print('Similarity to', nameRegion(k), similarity)
		simils1[k] = similarity
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils1]
		print('Similarity to region\n', similsPrint)

	# Adding similarity to complement
	# jV = code2Vector(j)
	jV = j
	# print('Intersection:')
	# imprime_region(jV)
	simils2 = [0] * 9
	for k in range(2,9): # do not consider 'rs' or 'all'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		kComp = [1 - x for x in kCoded]
		# similarity = simil(jV, kComp, epsilon)
		similarity = sim_consist(jV, kComp)
		# print('Similarity to complement of', nameRegion(k), similarity)
		simils2[k] = similarity
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils2]
		print('Similarity to complement\n', similsPrint)

	simils = np.add(simils1, simils2)
	if DEB:
		similsPrint = ["%.3f" % v for v in simils]
		print('FRA Similarity\n', similsPrint)

	simils = [delta * sigmoid(x, epsilon, zeta) for x in simils]
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils]
		print('Delta * sigmoid of FRA similarity\n', similsPrint)

	attractiveness = np.add(attractiveness, simils)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('final attractiveness\n', attactPrint)

	sum = np.sum(attractiveness)
	probs = [x/sum for x in attractiveness]

	return probs

def chooseStrategy(iV, i, s, j, pl):
	# Returns the next region according to biases
	# Input: i, which is the region that the player is in
	#		 s, which is the player's score
	#		 j, which is the overlapping region with the other player

	# get the probability vector
	probs = probabilities(iV, i, s, j, pl)

	if DEB:
		probsPrint = ["%.3f" % v for v in probs]
		print('probs\n', probsPrint)
	# print('Suma: ', np.sum(probs))

	# get the selected strategy
	# newStrategy = np.random.choice(range(9), p=probs)
	newStrategy = np.argmax(probs)
	# print('newStrategy', newStrategy)

	if IMPR:
		probsPrint = ["%.3f" % v for v in probs]
		with open('fileFreqs.csv', 'a') as data_file:
			data_file.write('AA,')
			data_file.write('A,')
			data_file.write(str(nameRegion(i)) + ',' + str(s) + ',')
			for p in probsPrint:
				data_file.write(str(p) + ',')
			data_file.write(str(nameRegion(newStrategy)) + '\n')
		data_file.close()

	return newStrategy

def list_from_row(r, cols):

    lista = []
    for c in cols:
        lista.append(list(r[c])[0])

    return lista

#########################################################################
# INSTRUCTIONS
#########################################################################

modelParameters = [0.1, 0.1, 0.1, 0.1, 200, 500, 32, 200, 500, 0.7] #PL1
# modelParameters = [100, 1, 1, 20, 1, 200, 500, 32, 100, 0.2, 100, 0.2, 100] #PL2

data = pd.read_csv('humans.csv')
print('Data loaded!')
# print(data[['Dyad', 'Player', 'Score']][:3])

dyad = '435-261'

data = pd.DataFrame(data.groupby('Dyad').get_group(dyad))

# print(data[:10])

regionsCoded, strategies = create_regions_and_strategies(Num_Loc)

# for i in range(len(regionsCoded)):
# 	print(nameRegion(i + 1))
# 	imprime_region(regionsCoded[i])

desde = 1
hasta = 2

player = dyad + 'PL1'
# player = dyad + 'PL2'
dataPL1 = pd.DataFrame(data.groupby('Player').get_group(player))
dataPL1 = dataPL1[desde:hasta]

player = dyad + 'PL2'
# player = dyad + 'PL1'
dataPL2 = pd.DataFrame(data.groupby('Player').get_group(player))
dataPL2 = dataPL2[desde:hasta]

cols = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]

for key, grp in dataPL1.groupby('Round'):
    print('\nThis is the end of round', key)
    region = list(grp['Category'])[0]
    categoria = numberRegion(region)
    print('The visited region was', str(region), '(Category', str(categoria) + ')')
    grp_otro = dataPL2.groupby('Round').get_group(key)
    regionPL1 = list_from_row(grp, cols)
    regionPL2 = list_from_row(grp_otro, cols)
    joint = [regionPL1[i] * regionPL2[i] for i in range(len(regionPL1))]
    # print(joint)
    imprime_region(regionPL1)
    score = list(grp['Score'])[0]
    print('The score was', score)
    n = chooseStrategy(regionPL1, categoria, score, joint, 0)
    print('The new strategy is', str(n), '(' + str(nameRegion(n)) + ')')
