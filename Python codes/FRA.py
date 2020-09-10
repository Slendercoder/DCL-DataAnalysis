import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib import gridspec
from random import choice, uniform, random, sample, randint

###########################################################
# GLOBAL VARIABLES
###########################################################

TOLERANCIA = 1
DEB = False
IMPR = False

regionsCoded = ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:', # ALL
                  '', # NOTHING
                  'GHIJKLMNOPQRSTUVWXYZ0123456789;:', # BOTTOM
                  'abcdefghijklmnopqrstuvwxyzABCDEF', # TOP
                  'abcdijklqrstyzABGHIJOPQRWXYZ4567', # LEFT
                  'efghmnopuvwxCDEFKLMNSTUV012389;:', # RIGHT
                  'jklmnorstuvwzABCDEHIJKLMPQRSTUXYZ012', # IN
                  'abcdefghipqxyFGNOVW3456789;:' # OUT
                  ]

regions = ['RS', \
           'ALL', \
           'NOTHING', \
           'BOTTOM', \
           'TOP', \
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

    # m = 32
    # sd = 8
    # n = int(np.random.normal(m, sd))
    # while n < 2 or n > 62:
    #     n = int(np.random.normal(m, sd))

    # return list(np.random.choice(Num_Loc * Num_Loc, n))
    n = randint(2,Num_Loc * Num_Loc - 2)
    return list(np.random.choice(Num_Loc * Num_Loc, n)) # Probando absolutamente random

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
		return 'BOTTOM'
	elif r == 4:
		return 'TOP'
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
	elif r == 'BOTTOM':
		return 3
	elif r == 'TOP':
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

	# TOP and BOTTOM
	up = [1] * half_size + [0] * half_size
	bottom = [1 - i for i in up]
	# print('BOTTOM ', bottom)
	# print('TOP ', up)

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
	TOP = []
	BOTTOM = []
	LEFT = []
	RIGHT = []
	IN = []
	OUT = []
	ALL = []
	NOTHING = []

	for i in range(int(Num_Loc * Num_Loc)):
		if up[i] == 1:
			TOP.append(i)
		if bottom[i] == 1:
			BOTTOM.append(i)
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
	strategies[3] = BOTTOM
	strategies[4] = TOP
	strategies[5] = LEFT
	strategies[6] = RIGHT
	strategies[7] = IN
	strategies[8] = OUT
	strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
	while len(strategies[9]) < 2 or len(strategies[9]) > 62:
	       strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

	return [all, nothing, bottom, up, left, right, In, out], strategies

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

def dibuja_ronda(reg1, sco1, reg2, sco2, Num_Loc, modelParameters, focals, titulo):

    assert(len(reg1) == Num_Loc * Num_Loc), "Incorrect region size 1!"
    assert(len(reg2) == Num_Loc * Num_Loc), "Incorrect region size 2!"

    # Initializing Plot
    fig = plt.figure()
    spec = gridspec.GridSpec(ncols=2, nrows=2)#, height_ratios=[3, 1, 1, 1])
    fig.subplots_adjust(left=0.1, bottom=0.05, right=0.9, top=0.95, wspace=0.1, hspace=0.2)

    ax0 = fig.add_subplot(spec[0,0])
    ax1 = fig.add_subplot(spec[0,1])
    ax2 = fig.add_subplot(spec[1,0])
    ax3 = fig.add_subplot(spec[1,1])

    ax0.set_title('Player 1')
    ax1.set_title('Player 2')
    ax0.get_xaxis().set_visible(False)
    ax1.get_xaxis().set_visible(False)
    ax0.get_yaxis().set_visible(False)
    ax1.get_yaxis().set_visible(False)
    ax2.set_yticklabels([])
    ax2.set_ylabel('Attracted\n to', fontsize=8)
    ax3.yaxis.tick_right()
    # ax2.get_xaxis().set_visible(False)
    # ax3.get_xaxis().set_visible(False)

    # Ploting regions
    step = 1. / Num_Loc
    tangulos1 = []
    tangulos2 = []
    for j in range(0, Num_Loc * Num_Loc):
        x = int(j) % Num_Loc
        y = (int(j) - x) / Num_Loc
        by_x = x * step
        by_y = 1 - (y + 1) * step
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
        ax0.add_patch(t)

    for t in tangulos2:
        ax1.add_patch(t)

    # Plot attractiveness
    regions_names = ['RS','A','N','B','T','L','R','I','O']
    overlap = np.multiply(reg1, reg2).tolist()
    frasPL1 = attractiveness(reg1, sco1, overlap, 0, modelParameters, Num_Loc, focals)
    frasPL2 = attractiveness(reg2, sco2, overlap, 1, modelParameters, Num_Loc, focals)
    ax2.set_ylim(0,max(1,max(frasPL1)))
    ax3.set_ylim(0,max(1,max(frasPL1)))
    ax2.bar(regions_names, frasPL1)
    ax3.bar(regions_names, frasPL2)

    threshold = frasPL1[0]
    ax2.axhline(y=threshold, linewidth=1, color='k')
    ax3.axhline(y=threshold, linewidth=1, color='k')

    fig.suptitle(titulo)
    plt.show()

def sigmoid(x, beta, gamma):
    # define attractiveness and choice functions
	return 1. / (1 + np.exp(-beta * (x - gamma)))

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
    # if (len(aux) == 0) or (len(aux) == Num_Loc*Num_Loc):
    	# print('Ignore focal regions ALL and NOTHING for similarity to complement')
    if (len(aux) == 0):
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
		wBOTTOM = float(modelParameters[2])
		wTOP = float(modelParameters[2])
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
		wBOTTOM = float(modelParameters[12])
		wTOP = float(modelParameters[12])
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

	# biasPrint = ["%.3f" % v for v in [wALL, wNOTHING, wBOTTOM, wTOP, wLEFT, wRIGHT, wIN, wOUT]]
	# print('bias: ', biasPrint)
	wRS = 1 - np.sum(np.array([wALL, wNOTHING, wBOTTOM, wTOP, wLEFT, wRIGHT, wIN, wOUT]))
	assert(wRS > 0), "Incorrect biases!"
	bias = [wRS, wALL, wNOTHING, wBOTTOM, wTOP, wLEFT, wRIGHT, wIN, wOUT]
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

	return probs

def attractiveness(region, score, overlap, pl, modelParameters, Num_Loc, focals, DEB=False):

	if pl == 0:
		wALL = float(modelParameters[0])
		wNOTHING = float(modelParameters[1])
		wBOTTOM = float(modelParameters[2])
		wTOP = float(modelParameters[2])
		wLEFT = float(modelParameters[2])
		wRIGHT = float(modelParameters[2])
		wIN = float(modelParameters[3])
		wOUT = float(modelParameters[3])
		alpha = float(modelParameters[4]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[5]) # amplitude of the WSLS sigmoid function
		gamma = float(modelParameters[6]) # position of the WSLS sigmoid function
		delta = float(modelParameters[7]) # for how much the added similarities augments attractiveness
		epsilon = float(modelParameters[8]) # amplitude of the similarity sigmoid function
		zeta = float(modelParameters[9]) # position of the similarity sigmoid function
		eta = float(modelParameters[10]) # for how much the added complement similarities augments attractiveness
		theta = float(modelParameters[11]) # amplitude of the complement similarity sigmoid function
		iota = float(modelParameters[12]) # position of the complement similarity sigmoid function
	else:
		wALL = float(modelParameters[13])
		wNOTHING = float(modelParameters[14])
		wBOTTOM = float(modelParameters[15])
		wTOP = float(modelParameters[15])
		wLEFT = float(modelParameters[15])
		wRIGHT = float(modelParameters[15])
		wIN = float(modelParameters[16])
		wOUT = float(modelParameters[16])
		alpha = float(modelParameters[17]) # for how much the focal region augments attractiveness
		beta = float(modelParameters[18]) # amplitude of the WSLS sigmoid function
		gamma = float(modelParameters[19]) # position of the WSLS sigmoid function
		delta = float(modelParameters[20]) # for how much the added FRA similarities augments attractiveness
		epsilon = float(modelParameters[21]) # amplitude of the FRA sigmoid function
		zeta = float(modelParameters[22]) # position of the FRA sigmoid function
		eta = float(modelParameters[23]) # for how much the added complement similarities augments attractiveness
		theta = float(modelParameters[24]) # amplitude of the complement similarity sigmoid function
		iota = float(modelParameters[25]) # position of the complement similarity sigmoid function

	attractiveness = [wALL, wNOTHING, wBOTTOM, wTOP, wLEFT, wRIGHT, wIN, wOUT]
	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('Player', pl)
		print('biases\n', attactPrint)

	# Adding 'Win Stay'
	WinStay = [alpha * sigmoid(score, beta, gamma) * sigmoid(sim_consist(x, region), 100, 0.95) for x in focals]
	attractiveness = np.add(attractiveness, WinStay)

	if DEB:
		attactPrint = ["%.3f" % v for v in WinStay]
		print('win stay\n', attactPrint)

	# Adding similarity to region
	simils = [delta * sim_consist(x, region) for x in focals]
	attractiveness = np.add(attractiveness, simils)

	if DEB:
		attactPrint = ["%.3f" % v for v in simils]
		print('similarity to region\n', attactPrint)

	# Adding similarity to complement
	complements = [[1 - x for x in sublist] for sublist in focals]
	simils = [eta * sigmoid(sim_consist(x, overlap), theta, iota) for x in complements]
	simils[0] = 0 # region NOTHING is not attracted by overlap
	attractiveness = np.add(attractiveness, simils).tolist()

	if DEB:
		# print('eta, theta, iota', eta, theta, iota)
		# print('overlap:')
		# imprime_region(overlap)
		# x = complements[3]
		# print('sim', eta * sigmoid(sim_consist(x, overlap), theta, iota))
		attactPrint = ["%.3f" % v for v in simils]
		print('similarity to complement\n', attactPrint)

	if DEB:
		attactPrint = ["%.3f" % v for v in attractiveness]
		print('final attractiveness\n', attactPrint)

	wRS = 1 - np.sum(np.array([wALL, wNOTHING, wBOTTOM, wTOP, wLEFT, wRIGHT, wIN, wOUT]))
#	assert(wRS > 0), "Incorrect biases!"
	attractiveness = [wRS] + attractiveness
	attractiveness = [np.round(x, 3) for x in attractiveness]

	return attractiveness

def get_strategy(sims):
    sims = [float(x) for x in sims]
    a = sims[0]
    b = sims[2]
    t = sims[3]
    l = sims[4]
    r = sims[5]
    A = int(np.ceil(64 * a))
    B = int(b * (A + 32) / (1 + b))
    T = int(t * (A + 32) / (1 + t))
    L = int(l * (A + 32) / (1 + l))
    R = int(r * (A + 32) / (1 + r))
#    print(a, b, t, l, r)
#    print("A = ", A)
#    print("A int B =", B)
#    print("A int T =", T)
#    print("A int L =", L)
#    print("A int R =", int(r * (A + 32) / (1 + r)))
    Ma = min(T, L, 16, (16 + T + L - A))
    Mi = max(0, T - R, L - 16, T - 16)
#    print("Min(T, L, 16, (16 + T + L - A)) =", Ma)
#    print("Max(0, T - R, L - 16, T - 16) =", Mi)
    x = randint(Mi, Ma)
    y = T - x
    u = L - x
    v = A - x - y - u
#    print("x =", x)
#    print("y =", y)
#    print("u =", u)
#    print("v =", v)
    X = list(range(0,4)) + list(range(8,12)) + list(range(16,20)) + list(range(24, 28))
    Y = list(range(4,8)) + list(range(12,16)) + list(range(20,24)) + list(range(28, 32))
    U = list(range(32,36)) + list(range(40,44)) + list(range(48,52)) + list(range(56, 60))
    V = list(range(36,40)) + list(range(44,48)) + list(range(52,56)) + list(range(60, 64))
    X = list(np.random.choice(X, x, replace=False)) if x > 0 else []
    Y = list(np.random.choice(Y, y, replace=False)) if y > 0 else []
    U = list(np.random.choice(U, u, replace=False)) if u > 0 else []
    V = list(np.random.choice(V, v, replace=False)) if v > 0 else []
    strategy = X + Y + U + V
#    print(strategy)
    return strategy

def shaky_hand(strategy, p=2):
    outs = np.random.choice(strategy, p) if len(strategy) > 0 else []
    complement = [i for i in range(64) if i not in strategy]
    ins = np.random.choice(complement, p) if len(complement) > 0 else []
    strategy = [i for i in strategy if i not in outs] + list(ins)
    return [i for i in strategy]

def mean_strategy():
    mean_sims = [0.491, 0.079, 0.335, 0.322, 0.331, 0.328, 0.335, 0.276]
    mean_strategy = get_strategy(mean_sims)
    return shaky_hand(mean_strategy)

def mean_region():
    mean_sims = [0.491, 0.079, 0.335, 0.322, 0.331, 0.328, 0.335, 0.276]
    mean_strategy = get_strategy(mean_sims)
    strategy = shaky_hand(mean_strategy)
    return code2Vector(strategy, 8)

def chooseStrategy(region, score, overlap, pl, modelParameters, Num_Loc, focals, estrategias, DEB=False, random=False):
	# Returns the next region according to attractiveness
	# Input: region (64-bit list), the region explored on the previous round
	#		 score, the player's score
	#		 overlap (64-bit list), the overlapping region with the other player

	# get the attractiveness vector
	attract = attractiveness(region, score, overlap, pl, modelParameters, Num_Loc, focals)

	if DEB:
		probsPrint = ["%.3f" % v for v in attract]
		print('probs\n', probsPrint)

	# get the strategy corresponding to the highest ranking attractiveness
	selected = np.argmax(attract)
	if selected > 0:
            if random:
	              newStrategy = shaky_hand(estrategias[np.argmax(attract) + 1])
            else:
                newStrategy = estrategias[np.argmax(attract) + 1]
	else:
	       if random:
	              newStrategy = mean_strategy()
	       else:
    		# Seleccionando una estrategia media para todos los casos
	              newStrategy = [10, 2, 16, 0, 24, 27, 26, 11, 19, 15, 14, 5, 7, 30, 31, 35, 49, 41, 43, 34, 48, 45, 60, 55, 62, 37, 44, 61, 63, 38, 52, 47]

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
