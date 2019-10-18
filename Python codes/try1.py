import numpy as np
import pandas as pd
from random import choice, uniform, random, sample, randint

DEB = False
IMPR = False

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

def sigmoid(x, beta, gamma):
	return 1. / (1 + np.exp(-beta * (x - gamma)))

def code2Vector(strategy):
    strategy = [int(x) for x in strategy]
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

    k = np.array(k)
    i = np.array(i)
    # print('k', k)
    # print('i', i)
    dif = np.subtract(k, i)
    # print('dif', dif)
    squares = np.multiply(dif, dif)
    # print('squares', squares)
    distance = np.sqrt(np.sum(squares))
    # print('distance', distance)
    # print('o', o)
    p = np.exp(- o * distance)
    # print(p)
    return(p)


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
	strategies[1] = ALL
	strategies[2] = NOTHING
	strategies[3] = DOWN
	strategies[4] = UP
	strategies[5] = LEFT
	strategies[6] = RIGHT
	strategies[7] = IN
	strategies[8] = OUT
	strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

	return [all, nothing, down, up, left, right, In, out], strategies

def probabilities(i, score, j):

    wALL = 0.1 * float(modelParameters[0])
    wNOTHING = 0.15 * float(modelParameters[0])
    wDOWN = 0.1 * float(modelParameters[0])
    wUP = 0.1 * float(modelParameters[0])
    wLEFT = 0.09 * float(modelParameters[0])
    wRIGHT = 0.09 * float(modelParameters[0])
    wIN = 0.01 * float(modelParameters[0])
    wOUT = 0.01 * float(modelParameters[0])
    bias = [0, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
    # print('biases:', bias)
    # print('sum', np.sum(bias))
    wRS = 1 - np.sum(bias)
    # print('wRS', wRS)
    assert(wRS >= 0), "Error: wRS is negative! Incorrect parameters provided."
    bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
    # biasPrint = ["%.2f" % v for v in bias]
    # print('bias: ', biasPrint)
    alpha = modelParameters[1] # for how much the focal region augments attractiveness
    beta = modelParameters[2] # amplitude of the sigmoid function
    gamma = modelParameters[3] # position of the sigmoid function
    delta = modelParameters[4] # for how much the similarity to complement augments attractiveness
    epsilon = modelParameters[5] # for similarity to complementary focal region
    zeta = modelParameters[6] # for the steepness of the similarity to focal region
    eta = modelParameters[7] # for the steepness of the similarity to complementary focal region

    global regions
    global strategies

    regionsCoded = regions
    strategies = strategies

    iV = code2Vector(strategies[i])
    if i==9: i = 0

    attractiveness = [x for x in bias] # start from bias
    if DEB:
        attactPrint = ["%.3f" % v for v in attractiveness]
        print('attractiveness before WS and FRA\n', attactPrint)

    n = (score + 128) / 160 # normalizing score

    # Adding 'Win Stay'
    attractiveness[i] += alpha * sigmoid(n, beta, gamma)

    if DEB:
        attactPrint = ["%.3f" % v for v in attractiveness]
        print('attractiveness with WS\n', attactPrint)

    jV = code2Vector(j)
    # print('jV ', jV)
    simils = [0] * 9
    for k in range(2,9): # do no consider 'rs' or 'all'
        kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
        kComp = [1 - x for x in kCoded]
        # print('kComp', kComp)
        sii = simil(jV, kComp, eta)
        # print('Region', nameRegion(k), 'simil', sii)
        simils[k] = sii
        #
    if DEB:
        similsPrint = ["%.3f" % v for v in simils]
        print('Similarity to complement\n', similsPrint)

    attractiveness += np.multiply(delta, simils)

    if DEB:
        attactPrint = ["%.3f" % v for v in attractiveness]
        print('Attractiveness to complement\n', attactPrint)

    # print('jV ', jV)
    simils = [0] * 9
    for k in range(1,9): # do no consider 'rs'
        kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
        # print('Considering region: ', k)
        simils[k] = simil(iV, kCoded, epsilon)
    #
    if DEB:
        similsPrint = ["%.3f" % v for v in simils]
        print('Similarity to region\n', similsPrint)

    attractiveness += np.multiply(zeta, simils)
    if DEB:
        attactPrint = ["%.3f" % v for v in attractiveness]
        print('final attractiveness\n', attactPrint)

    # sum = np.sum(np.exp(attractiveness/0.4))
    # probs = [np.exp(x/0.4)/sum for x in attractiveness]
    sum = np.sum(attractiveness)
    probs = [x/sum for x in attractiveness]

    return probs

def chooseStrategy(i, s, j):
	# Returns the next region according to biases
	# Input: i, which is the region that the player is in
	#		 s, which is the player's score
	#		 j, which is the overlapping region with the other player

	# get the probability vector
	probs = probabilities(i, s, j)

	if IMPR:
		probsPrint = ["%.3f" % v for v in probs]
		print('probs\n', probsPrint)
	# print('Suma: ', np.sum(probs))

	# get the selected strategy
	newStrategy = np.random.choice(range(9), p=probs)
	# print('newStrategy', newStrategy)

	# Determines if should not randomize RS
	if newStrategy == 0:
		# print('Determines if should randomize RS')
		n = (s + 128) / 160 # normalizing score
		beta = modelParameters[2] # amplitude of the sigmoid function
		gamma = modelParameters[3] # position of the sigmoid function
		if uniform(0,1) > sigmoid(n, beta, gamma):
			# print('It should')
			return newStrategy, False
		else:
			# print('It should not')
			return newStrategy, True
	else:
		return newStrategy, False
	# return newStrategy, False

gameParameters = [0.5, 2, 8, 60, 1]
modelParameters = [1, 150, 500, 0.98, 0, 0, 0, 0]

Num_Loc = gameParameters[2]
regions, strategies = create_regions_and_strategies(Num_Loc)

i = 5
s = 30
j = ''
probs = probabilities(i, s, j)
probsPrint = ["%.3f" % v for v in probs]
print('probs\n', probsPrint)

lista = []
for k in range(100):
    lista.append(chooseStrategy(i, s, j)[0])

df = pd.DataFrame({'A': lista})
df1 = pd.DataFrame(df.A.value_counts())
df1.columns= ['Freq']
df1['relFreq'] = df1['Freq']/df1['Freq'].sum()
print(df1)
