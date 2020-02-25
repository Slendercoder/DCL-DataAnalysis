import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches

###########################################################
# GLOBAL VARIABLES
###########################################################

TOLERANCIA = 1
DEB = False
IMPR = False

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

###########################################################
# FUNCTIONS
###########################################################

def new_random_strategy(Num_Loc):
    # Creates a new random strategy to explore grid
    # The size of this new strategy is determined by
    # a normal distribution with mean = m and s.d. = sd

    # sd = 4
    # m = 48
    m = 32
    sd = 8
    n = int(np.random.normal(m, sd))
    while n < 2 or n > 62:
        n = int(np.random.normal(m, sd))

    return list(np.random.choice(Num_Loc * Num_Loc, n))

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

def lettercode2Strategy(coded, Num_Loc):

	letras = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:')

	v = []
	for c in coded:
		v.append(letras.index(c))

	return v

def code2Vector(strategy, Num_Loc):

    size = int(Num_Loc * Num_Loc)
    v = [0] * size

    for i in range(size):
        if i in strategy:
            v[i] = 1

    return v

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

def sim_consist(v1, v2):
	# Returns the similarity based on consistency
	# v1 and v2 are two 64-bit coded regions

	if type(v1) == type(np.nan) or type(v2) == type(np.nan):
	       return np.nan
	else:
	       assert(len(v1) == 64), 'v1 must be a 64-bit coded region!'
	       assert(len(v2) == 64), 'v2 must be a 64-bit coded region!'
	       joint = [v1[x] * v2[x] for x in range(len(v1))]
	       union = [v1[x] + v2[x] for x in range(len(v1))]
	       union = [x/x for x in union if x != 0]
	       j = np.sum(np.array(joint))
	       u = np.sum(np.array(union))
	       if u != 0:
	              return float(j)/u
	       else:
	              return 1

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

def maxSim2Focal(r, Num_Loc):
    # Returns maximum similarity (BASED ON CONSISTNECY) to focal region
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing the highest similarity

    # imprime_region(r)
    # print('\n')

    similarities = [0] * 8
    contador = 0

    for k in regionsCoded:
        reg = lettercode2Strategy(k, Num_Loc)
        kV = code2Vector(reg, Num_Loc)
        # imprime_region(kV)
        # finding similarity to COMPLEMENT
        kComp = [1 - x for x in kV]
        sss = sim_consist(r, kComp)
        # print('Similarity to Comp Region', contador, ' is:', sss)
        similarities[contador] = sss
        contador = contador + 1

    # simPrint = ["%.3f" % v for v in similarities]
    # print('maxSim2Focal', simPrint)
    valor = np.max(np.array(similarities))
    return(valor)

def minDist2Focal(r, regionsCoded):
	# Returns closest distance to focal region
	# Input: r, which is a region coded as a vector of 0s and 1s of length 64
	# Output: number representing the closest distance

	global TOLERANCIA

	distances = [0] * 8
	contador = 0

	# print('r:\n', list(r))
	for k in regionsCoded:
		# kV = self.code2Vector(k)
		# print('k:\n', k)
		distances[contador] = dist(list(r), k)
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

def FRASim(r, joint, focal, Num_Loc):
    # Returns FRA similarity
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    #        joint, which is a region coded as a vector of 0s and 1s of length 64
    #        focal, which is a focal region coded as a vector of 0s and 1s of length 64
    # Output: number representing FRA similarity

    # print('Region')
    # imprime_region(r)
    # print('Joint')
    # imprime_region(joint)
    # print('Focal region')
    # imprime_region(focal)

    # finding similarity between r and focal
    sss1 = sim_consist(r, focal)
    # print('Similarity to Focal Region is:', sss1)

    # finding similarity between Joint and Complement to focal
	# first check whether focal is ALL (should not add similarity to complement here)
    aux = [x for x in focal if x == 0]
    if len(aux) == 0:
    	# print('Ignore focal region ALL for similarity to complement')
    	sss2 = 0
    else:
    	kComp = [1 - x for x in focal]
    	sss2 = sim_consist(joint, kComp)
    	# print('Similarity to Comp Focal Region is:', sss2)

    return sss1 + sss2

def maxFRASim(r, joint, Num_Loc):
    # Returns maximum FRA similarity
    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
    #        joint, which is a region coded as a vector of 0s and 1s of length 64
    # Output: number representing maximum FRA similarity

    # print('Region')
    # imprime_region(r)
    # print('Joint')
    # imprime_region(joint)

    similarities = [0] * 8
    contador = 0

    for k in regionsCoded:
        reg = lettercode2Strategy(k, Num_Loc)
        kV = code2Vector(reg, Num_Loc)
        similarities[contador] = FRASim(r, joint, kV)
        contador = contador + 1

    # simPrint = ["%.3f" % v for v in similarities]
    # print('maxSim2Focal', simPrint)
    valor = np.max(np.array(similarities))
    return(valor)

def probabilities(iV, i, score, j, pl, modelParameters, Num_Loc):

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

	# biasPrint = ["%.3f" % v for v in [wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]]
	# print('bias: ', biasPrint)
	wRS = 1 - np.sum(np.array([wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]))
	assert(wRS > 0), "Incorrect biases!"
	bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
	# biasPrint = ["%.3f" % v for v in bias]
	# print('bias: ', biasPrint)

	# regionsCoded = regions
	# strategies = strategies

	# print('iV')
	# imprime_region(iV)
	# print('i', i)
	if i==9: i = 0

	attractiveness = [x for x in bias] # start from bias
	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('Player', pl)
		print('attractiveness before WS and FRA\n', attactPrint)

	# Adding 'Win Stay'
	if i != 0:
	          attractiveness[i] += alpha * sigmoid(score, beta, gamma)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('attractiveness with WS\n', attactPrint)

	# Calculating similarity to region
	simils1 = [0] * 9
	for k in range(1,9): # do not consider 'rs'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		kCoded = lettercode2Strategy(kCoded, Num_Loc)
		kCoded = code2Vector(kCoded, Num_Loc)
		# print('kCoded')
		# imprime_region(kCoded)
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
		kCoded = lettercode2Strategy(kCoded, Num_Loc)
		kCoded = code2Vector(kCoded, Num_Loc)
		# print('kCoded')
		# imprime_region(kCoded)
		kComp = [1 - x for x in kCoded]
		# print('kComp')
		# imprime_region(kComp)
		# similarity = simil(jV, kComp, epsilon)
		similarity = sim_consist(jV, kComp)
		# print('Similarity to complement of', nameRegion(k), similarity)
		simils2[k] = similarity
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils2]
		print('Similarity to complement\n', similsPrint)

	simils = np.add(simils1, simils2)
	simils = [delta * sigmoid(x, epsilon, zeta) for x in simils]
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils]
		print('FRA similarity\n', similsPrint)

	attractiveness = np.add(attractiveness, simils)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('final attractiveness\n', attactPrint)

	sum = np.sum(attractiveness)
	probs = [x/sum for x in attractiveness]
	# sum = np.sum([x**10 for x in attractiveness])
	# probs = [x**10/sum for x in attractiveness]

	return probs

def chooseStrategy(iV, i, s, j, pl, modelParameters, Num_Loc):
	# Returns the next region according to biases
	# Input: iV (64-bit list), the region the player is in
	#		 i, the strategy number
	#		 s, the player's score
	#		 j (64-bit list), the overlapping region with the other player

	# get the probability vector
	probs = probabilities(iV, i, s, j, pl, modelParameters, Num_Loc)

	if DEB:
		probsPrint = ["%.3f" % v for v in probs]
		print('probs\n', probsPrint)
	# print('Suma: ', np.sum(probs))

	# get the selected strategy
	newStrategy = np.random.choice(range(9), p=probs)
	# newStrategy = np.argmax(probs)
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

def nas(x, y):
	if x == 'Unicorn_Present':
		return np.nan
	else:
		return y


#############################################################
# PREVIOUS VERSIONS
#############################################################
def maxSim2FocalPREVIOUS(r):
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

def probabilitiesPREVIOUS(iV, i, score, j, pl):

	global modelParameters

	if pl == 0:
		wRS = float(modelParameters[0])
		wALL = float(modelParameters[1])
		wNOTHING = float(modelParameters[2])
		wDOWN = float(modelParameters[3])
		wUP = float(modelParameters[3])
		wLEFT = float(modelParameters[3])
		wRIGHT = float(modelParameters[3])
		wIN = float(modelParameters[4])
		wOUT = float(modelParameters[4])
		alpha = float(modelParameters[5]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[6]) # amplitude of the sigmoid function
		gamma = float(modelParameters[7]) # position of the sigmoid function
		delta = float(modelParameters[8]) # for how much the similarity to complement augments attractiveness
		epsilon = float(modelParameters[9]) # for the steepness of the similarity to complementary focal region
		zeta = float(modelParameters[10]) # for how much the similarity to focal region augments attractiveness
		eta = float(modelParameters[11]) # for the steepness of the similarity to focal region
		iota = float(modelParameters[12]) # for how much the winner takes all
	else:
		wRS = float(modelParameters[13])
		wALL = float(modelParameters[14])
		wNOTHING = float(modelParameters[15])
		wDOWN = float(modelParameters[16])
		wUP = float(modelParameters[16])
		wLEFT = float(modelParameters[16])
		wRIGHT = float(modelParameters[16])
		wIN = float(modelParameters[17])
		wOUT = float(modelParameters[17])
		alpha = float(modelParameters[18]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[19]) # amplitude of the sigmoid function
		gamma = float(modelParameters[20]) # position of the sigmoid function
		delta = float(modelParameters[21]) # for how much the similarity to complement augments attractiveness
		epsilon = float(modelParameters[22]) # for the steepness of the similarity to complementary focal region
		zeta = float(modelParameters[23]) # for how much the similarity to focal region augments attractiveness
		eta = float(modelParameters[24]) # for the steepness of the similarity to focal region
		iota = float(modelParameters[25]) # for how much the winner takes all

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

	# Adding similarity to region
	simils = [0] * 9
	for k in range(1,9): # do not consider 'rs'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		similarity = simil(iV, kCoded, eta)
		# print('Similarity to', nameRegion(k), similarity)
		simils[k] = similarity
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils]
		print('Similarity to region\n', similsPrint)

	attractiveness += np.multiply(zeta, simils)
	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('Attractiveness to region\n', attactPrint)

	# Adding similarity to complement
	# jV = code2Vector(j)
	jV = j
	# print('Intersection:')
	# imprime_region(jV)
	simils = [0] * 9
	for k in range(2,9): # do not consider 'rs' or 'all'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		kComp = [1 - x for x in kCoded]
		similarity = simil(jV, kComp, epsilon)
		# print('Similarity to complement of', nameRegion(k), similarity)
		simils[k] = similarity
	#
	if DEB:
		similsPrint = ["%.3f" % v for v in simils]
		print('Similarity to complement\n', similsPrint)

	attractiveness += np.multiply(delta, simils)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('final attractiveness\n', attactPrint)


	attractiveness = [x**iota for x in attractiveness]
	sum = np.sum(attractiveness)
	probs = [x/sum for x in attractiveness]

	return probs
