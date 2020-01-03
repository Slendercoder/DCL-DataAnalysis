# Class definition for solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
# Run with Python 3
# Run from main.py

from random import choice, uniform, random, sample, randint
from math import floor
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches

DEB = False
IMPR = False
TOLERANCIA = 1
TO_FILE = True

CONTINUO = False
CONTADOR = 1
TOLERANCIA = 1

############################################################
# Define function that initializes regions and strategies
############################################################

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
    if si == 'Unicorn_Absent' and siLead == 'Unicorn_Present' and s > 29 and s > sLEAD:
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

def nas(x, y):
	if x == 'Unicorn_Present':
		return np.nan
	else:
		return y

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

###########################
# Define player objects
###########################

# Define players
class player(object):
	'''Object defining a player. Has the following properties:
		Ready; Decision; Strategy; where; score'''
	def __init__(self, Ready, Decision, Strategy, Where, Score, Accuracy, Name):
		self.ready = Ready
		self.decision = Decision
		self.strategy = Strategy
		self.where = Where
		self.score = Score
		self.accuracy = Accuracy
		self.name = Name

###########################
# Define Experiment Object
###########################
class Experiment(object):
	'''Object defining the experiment and simulation with the following properties:
		gameParameters, modelParameters'''

	def __init__(self, gameParameters, modelParameters):
		assert(len(gameParameters) == 5), "Game parameters incorrect length!"
		assert(len(modelParameters) == 11), "Model parameters incorrect length!"
		self.gameParameters = gameParameters
		self.modelParameters = modelParameters

		# Create regions and strategies
		Num_Loc = gameParameters[2]
		regions, strategies = create_regions_and_strategies(Num_Loc)
		# for r in regions:
		# 	dibuja_region(r, Num_Loc)

		self.regions = regions
		self.strategies = strategies

		# Create data frame
		cols = ['Dyad', 'Round', 'Player', 'Answer', 'Time']
		cols += ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
		cols += ['Score', 'Joint', 'Is_there', 'where_x', 'where_y', 'Strategy']
		self.df = pd.DataFrame(columns=cols)

	# define attractiveness and choice functions
	def sigmoid(self, x, beta, gamma):
		return 1. / (1 + np.exp(-beta * (x - gamma)))

	def code2Vector(self, strategy):
		Num_Loc = self.gameParameters[2]
		size = int(Num_Loc * Num_Loc)
		v = [0] * size
		for i in range(size):
			if i in strategy:
				v[i] = 1

		return v

	def simil(self, k, i, o):
	    # Returns similarity between regions k and i
	    # Input: k, which is a region coded as a vector of 0s and 1s of length 64
	    #        i, which is a region coded as a vector of 0s and 1s of length 64
	    #        o, which is a parameter for the exponential
	    # Output: number representing the similarity between k and i

	    k = np.array(k)
	    i = np.array(i)
	    dif = np.subtract(k, i)
	    squares = np.multiply(dif, dif)
	    distance = np.sqrt(np.sum(squares))
	    return(np.exp(- o * distance))

	def dist(self, k, i):
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

	def maxSim2Focal(self, r, regionsCoded, eta):
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

	def minDist2Focal(self, r, regionsCoded):
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

	def probabilities(self, i, score, j):

		wALL = float(self.modelParameters[0])
		wNOTHING = float(self.modelParameters[1])
		wDOWN = float(self.modelParameters[2])
		wUP = float(self.modelParameters[2])
		wLEFT = float(self.modelParameters[2])
		wRIGHT = float(self.modelParameters[2])
		wIN = float(self.modelParameters[3])
		wOUT = float(self.modelParameters[3])
		bias = [0, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
		# print('biases:', bias)
		# print('sum', np.sum(bias))
		wRS = 1 - np.sum(bias)
		# print('wRS', wRS)
		assert(wRS >= 0), "Error: wRS is negative! Incorrect parameters provided."
		bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
		# biasPrint = ["%.2f" % v for v in bias]
		# print('bias: ', biasPrint)
		alpha = self.modelParameters[4] # for how much the focal region augments attractiveness
		beta = self.modelParameters[5] # amplitude of the sigmoid function
		gamma = self.modelParameters[6] # position of the sigmoid function
		delta = self.modelParameters[7] # for how much the similarity to complement augments attractiveness
		epsilon = self.modelParameters[8] # for similarity to complementary focal region
		zeta = self.modelParameters[9] # for the steepness of the similarity to focal region
		eta = self.modelParameters[10] # for the steepness of the similarity to complementary focal region

		regionsCoded = self.regions
		strategies = self.strategies

		iV = self.code2Vector(strategies[i])
		if i==9: i = 0

		attractiveness = [x for x in bias] # start from bias
		if DEB:
			attactPrint = ["%.2f" % v for v in attractiveness]
			print('attractiveness before WS and FRA\n', attactPrint)

		# n = (score + 128) / 160 # normalizing score
		n = score

		# Adding 'Win Stay'
		if i != 0:
		          attractiveness[i] += alpha * self.sigmoid(n, beta, gamma)

		if DEB:
			attactPrint = ["%.3f" % v for v in attractiveness]
			print('attractiveness with WS\n', attactPrint)

		jV = self.code2Vector(j)
		# print('jV ', jV)
		simils = [0] * 9
		for k in range(2,9): # do no consider 'rs' or 'all'
			kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
			kComp = [1 - x for x in kCoded]
			simils[k] = self.simil(jV, kComp, eta)
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
			simils[k] = self.simil(iV, kCoded, epsilon)
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

	def chooseStrategy(self, i, s, j):
		# Returns the next region according to biases
		# Input: i, which is the region that the player is in
		#		 s, which is the player's score
		#		 j, which is the overlapping region with the other player

		# get the probability vector
		probs = self.probabilities(i, s, j)

		if DEB:
			probsPrint = ["%.3f" % v for v in probs]
			print('probs\n', probsPrint)
		# print('Suma: ', np.sum(probs))

		# get the selected strategy
		newStrategy = np.random.choice(range(9), p=probs)
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

		# # Determines if should not randomize RS
		# if newStrategy == 0:
		# 	# print('Determines if should randomize RS')
		# 	n = (s + 128) / 160 # normalizing score
		# 	beta = self.modelParameters[2] # amplitude of the sigmoid function
		# 	gamma = self.modelParameters[3] # position of the sigmoid function
		# 	if uniform(0,1) > self.sigmoid(n, beta, gamma):
		# 		# print('It should')
		# 		return newStrategy, False
		# 	else:
		# 		# print('It should not')
		# 		return newStrategy, True
		# else:
		# 	return newStrategy, False
		# # return newStrategy, False

	def run_dyad(self):

		p = self.gameParameters[0] # probability of there being a unicorn (usually 0.5)
		Pl = self.gameParameters[1] # number of players (usually 2)
		Num_Loc = self.gameParameters[2] # number of locations (squares in a row in the grid; usually 8)
		N = self.gameParameters[3] # number of iterations per experiment

		# Create players
		Players = []
		for k in range(0, Pl):
			strat = 0 if k == 0 else 9
			Players.append(player(False, "", strat, [], 0, False, int(uniform(0, 1000000))))
			# print "Player " + str(k) + " chose strategy " + strat

		# Create dyad name
		dyad = str(Players[0].name)[:5] + str(Players[1].name)[:5]

		# Start the rounds
		for i in range(0, N):

			# print "----------------------------"
			# print "Now playing round " + str(i)
			# print "----------------------------"

			#Initializing the players for the round
			for pl in Players:
				pl.decision = ""
				pl.where = []
				pl.ready = False
				pl.score = 0
				pl.accuracy = False

			# Initializing the board
			Board = [0 for l in range(0, Num_Loc * Num_Loc)]

			# Determine whether there is a unicorn and where
			place = -1
			if uniform(0, 1) > p:
				place = int(floor(uniform(0, Num_Loc * Num_Loc - 1)))
				Board[place] = 1

			# Start searchging for the unicorn
			for j in range(0, Num_Loc * Num_Loc + 1):
				# print("\nRunning iteration " + str(j))
				for k in range(0, Pl):
					# See if other player said present. If so, do the same
					if Players[1 - k].decision == "Present":
						Players[k].decision = "Present"
						# print("Player " + str(k) + " said Present")
						Players[k].ready = True
						break
					# If the other player did not say Present, and
					# current player is not ready, then...
					elif not Players[k].ready:
						# ...look at the location determined by the strategy
		#				print("Player " + str(k) + " is using strategy: " + \
		#					str(Players[k].strategy))
		#				print("He is looking on location: " + str(strategies[Players[k].strategy]))
						# See if the strategy is not over...
						if j<len(self.strategies[Players[k].strategy]):
							search_place = self.strategies[Players[k].strategy][j]
							Players[k].where.append(search_place)
							# print("Player " + str(k) + " is searching at " + str(search_place))
							if Board[search_place] == 1:
								Players[k].decision = "Present"
								# print("Player " + str(k) + " said Present")
								Players[k].ready = True
							# else: print("Player " + str(k) + " found no unicorn")
						# Otherwise, say Absent
						else:
							# The strategy is over, so bet for Absent
							Players[k].decision = "Absent"
							# print("Player " + str(k) + " said Absent")
							Players[k].ready = True
					# Chechk if both players are ready. If so, stop search
					elif Players[1-k].ready == True:
						break
				else: continue
				break

			# print("\n")

			# Determine locations visited by both players
			# both = [x for x in Players[0].where if x in Players[1].where]
			both = list(set(Players[0].where).intersection(set(Players[1].where)))
			# print("Locations checked by both players: " + str(both))
			# print("The players checked on the same locations " + str(len(both)) + " times")

			# Create row of data as dictionary
			row_of_data = {}

			# Save data per player
			for k in range(0, Pl):

				# Determine individual scores
				if place == -1:
					# print("There was NO unicorn")
					if Players[k].decision == "Absent":
						# print("Player " + str(k) + "\'s answer is Correct!")
						Players[k].accuracy = True
						Players[k].score = Num_Loc*Num_Loc/2 - len(both)
						# print("Player " + str(k) + "\'s score this round is: " + \
						# 	str(Players[k].score))
					else:
						# print("Player " + str(k) + "\'s answer is Incorrect!")
						Players[k].accuracy = False
						Players[k].score = -Num_Loc*Num_Loc - len(both)
						# print("Player " + str(k) + "\'s score this round is: " + \
						# 	str(Players[k].score))
				else:
					# print("There was a unicorn")
					if Players[k].decision == "Present":
						# print("Player " + str(k) + "\'s answer is Correct!")
						Players[k].accuracy = True
						Players[k].score = Num_Loc*Num_Loc/2 - len(both)
						# print("Player " + str(k) + "\'s score this round is: " + \
						# 	str(Players[k].score))
					else:
						# print("Player " + str(k) + "\'s answer is Incorrect!")
						Players[k].accuracy = False
						Players[k].score = -Num_Loc*Num_Loc - len(both)
						# print("Player " + str(k) + "\'s score this round is: " + \
						# 	str(Players[k].score))

				row_of_data['Dyad'] = [dyad]
				row_of_data['Round'] = [i + 1]
				row_of_data['Player'] = [Players[k].name]
				row_of_data['Answer'] = [Players[k].decision]
				row_of_data['Time'] = [len(Players[k].where)]
				colA = ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
				for l in range(0, Num_Loc * Num_Loc):
					if l in Players[k].where:
						row_of_data[colA[l]] = [1]
					else:
						row_of_data[colA[l]] = [0]
				row_of_data['Score'] = [Players[k].score]
				row_of_data['Joint'] = [len(both)]
				if place == -1:
					row_of_data['Is_there'] = ["Unicorn_Absent"]
					row_of_data['where_x'] = [-1]
					row_of_data['where_y'] = [-1]
				else:
					row_of_data['Is_there'] = ["Unicorn_Present"]
					x = place % Num_Loc
					y = (place - x) / Num_Loc
					row_of_data['where_x'] = [x]
					row_of_data['where_y'] = [y]

				row_of_data['Strategy'] = [Players[k].strategy]

				# Add data to dataFrame
				dfAux = pd.DataFrame.from_dict(row_of_data)
				# print(dfAux)
				# print(dfAux.columns)
				# Keeping the order of columns
				dfAux = dfAux[['Dyad','Round','Player','Answer','Time','a11','a12','a13','a14','a15','a16','a17','a18','a21','a22','a23','a24','a25','a26','a27','a28','a31','a32','a33','a34','a35','a36','a37','a38','a41','a42','a43','a44','a45','a46','a47','a48','a51','a52','a53','a54','a55','a56','a57','a58','a61','a62','a63','a64','a65','a66','a67','a68','a71','a72','a73','a74','a75','a76','a77','a78','a81','a82','a83','a84','a85','a86','a87','a88','Score','Joint','Is_there','where_x','where_y','Strategy']]
				# print(dfAux)

				if TO_FILE:
				                with open('temp.csv', 'a') as f:
				                                dfAux.to_csv(f, header=False)
				else:
				                self.df = self.df.append(dfAux, ignore_index = True)

				# print(self.df)
				# print("Data from player " + str(k) + " has been saved")

			reg1 = self.code2Vector(self.strategies[Players[0].strategy])
			reg2 = self.code2Vector(self.strategies[Players[1].strategy])
			j = self.code2Vector(both)
			# Players determine their next strategy
			a = []
			sc = []
			a.append(Players[0].strategy)
			sc.append(Players[0].score)
			# newStrategy, sameRS = self.chooseStrategy(Players[0].strategy, Players[0].score, both)
			newStrategy = self.chooseStrategy(Players[0].strategy, Players[0].score, both)
			Players[0].strategy = newStrategy
			# print('newStrategy:', newStrategy)
			self.strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			while len(self.strategies[0]) < 2 or len(self.strategies[0]) > 62:
                            self.strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

			# if not sameRS:
			# 	self.strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# 	while len(self.strategies[0]) < 2 or len(self.strategies[0]) > 62:
			# 	                self.strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# else:
			# 	print('Do not randomize for player 0')

			a.append(Players[1].strategy)
			sc.append(Players[1].score)
			# newStrategy, sameRS = self.chooseStrategy(Players[1].strategy, Players[1].score, both)
			newStrategy = self.chooseStrategy(Players[1].strategy, Players[1].score, both)
			# print('newStrategy:', newStrategy)
			if newStrategy == 0:
				newStrategy = 9
			Players[1].strategy = newStrategy
			self.strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			while len(self.strategies[9]) < 2 or len(self.strategies[9]) > 62:
                            self.strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

			# if not sameRS:
			# 	self.strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# 	while len(self.strategies[9]) < 2 or len(self.strategies[9]) > 62:
			# 	                self.strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# else:
			# 	print('Do not randomize for player 1')

			if DEB:
				print('-----------------')
				print('both', len(both))
				print('scores: p0: ', sc[0], ' p1: ', sc[1])
				print('Player 0 from region ', nameRegion(a[0]), 'to region ', nameRegion(Players[0].strategy))
				print('Player 1 from region ', nameRegion(a[1]), 'to region ', nameRegion(Players[1].strategy))
				print('End summary round ', i)
				print('-----------------')
				# dibuja_region(j, 8)
				dibuja_regiones(reg1, reg2, 8, 'Ok')


	def run_simulation(self):

		IT = self.gameParameters[4] # number of experiments in a set

		for h in range(0, IT):
			print("****************************")
			print("Running dyad no. ", h + 1)
			print("****************************\n")
			self.run_dyad()

	def run_dyad_with_parameters(self, w, alpha):

            self.modelParameters = [w, alpha] + self.modelParameters[2:]
            self.run_dyad()

	def get_measures(self):

        # if TO_FILE:
        #     data = pd.read_csv("temp.csv")
        # else:
        #     data = self.df

		if TO_FILE:
		          data = pd.read_csv("temp.csv")
		else:
		          data = self.df


		Num_Loc = self.gameParameters[2]
		regions = ['RS', \
		           'ALL', \
		           'NOTHING', \
		           'DOWN', \
		           'UP', \
		           'LEFT', \
		           'RIGHT', \
		           'IN', \
		           'OUT']

		print("Sorting by Dyad, Player, Round...")
		data = data.sort_values(['Dyad', 'Player', 'Round'], ascending=[True, True, True]).reset_index(drop=True)
		# data.to_csv('output_Prev.csv', index=False)
		data['Is_there_LEAD'] = data.groupby(['Dyad', 'Player'])['Is_there'].transform('shift', periods=-1)

		# --------------------------------------------------
		# Classify region per round, per player
		# --------------------------------------------------
		print("Classifying regions (please be patient)...")

		# Deterimining list of columns
		cols1 = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]
		data['Category'] = data.apply(lambda x: self.minDist2Focal(x[cols1], self.regions), axis=1)
		data['Category1'] = data.apply(lambda x: nameRegion(x['Strategy']), axis=1)
		data['RegionGo'] = data.groupby(['Dyad', 'Player'])['Category1'].transform('shift', -1)
		data.to_csv('output_Prev.csv', index=False)

		if IMPR:
			f = './output_Prev.csv'
			data.to_csv(f, index=False)

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
		# situations = data.Is_there.unique()
		# print(situations)
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

		# Find consistency
		print("Finding consistency...")
		# # print data[:10]
		cols2 = ['a' + str(i + 1) + str(j + 1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
		data['Vector'] = data.apply(lambda x: np.array(x[cols]), axis=1)
		data['VectorLAG1'] = data.groupby(['Dyad', 'Player'])['Vector'].transform('shift', 1)
		# data = data.dropna()
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
		# #
		# --------------------------------------------------
		# Finding distance to closest focal region per round, per player
		# --------------------------------------------------
		print("Finding distances to focal paths (please be patient)...")

		# Deterimining list of columns
		cols1 = ['a' + str(i) + str(j) \
		for i in range(1, Num_Loc + 1) \
		for j in range(1, Num_Loc + 1) \
		]

		data['Similarity'] = data.apply(lambda x: self.maxSim2Focal(x[cols1], self.regions, 1), axis=1)
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
		                            ['Category'].transform('shift', -LAG)
		data['Similarity_LAG1'] = data.groupby(['Dyad', 'Player'])\
	                            ['Similarity'].transform('shift', LAG)

		self.df = data

		# ifSave = input("Do you want to save data to a file? (1=Yes/0=No): ")
		# ifSave = 1
		# if ifSave == 1:
		# 	outputFile = 'output.csv'
		# 	data.to_csv(outputFile, index=False)
		# 	print("Results saved to: " + outputFile)
		#
		# print("Done!")
