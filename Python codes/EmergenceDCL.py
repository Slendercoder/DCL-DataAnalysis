# Class definition for solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2019
# Run with Python 3
# Run from run_model.py

from random import choice, uniform, random, sample, randint
from math import floor
import numpy as np
import pandas as pd
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.patches as patches

############################################################
# Define function that initializes regions and strategies
############################################################

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

		fig4.show()

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
		assert(len(modelParameters) == 8), "Model parameters incorrect length!"
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

	def minDist2Focal(self, r, regionsCoded, eta):
	    # Returns closest distance to focal region
	    # Input: r, which is a region coded as a vector of 0s and 1s of length 64
	    # Output: number representing the closest distance

	    distances = [0] * 8
	    contador = 0

	    for k in regionsCoded:
	        kV = self.code2Vector(k)
	        distances[contador] = self.simil(r, kV, eta)
	        contador = contador + 1

	    valor = np.min(np.array(distances))
	    return(valor)

	def probabilities(self, i, score, j):

		wALL = float(self.modelParameters[0])
		wNOTHING = float(self.modelParameters[0])
		wDOWN = float(self.modelParameters[0])
		wUP = float(self.modelParameters[0])
		wLEFT = float(self.modelParameters[0])
		wRIGHT = float(self.modelParameters[0])
		wIN = float(self.modelParameters[0])
		wOUT = float(self.modelParameters[0])
		bias = [0, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
		# print('biases:', bias)
		# print('sum', np.sum(bias))
		wRS = 1 - np.sum(bias)
		# print('wRS', wRS)
		assert(wRS >= 0), "Error: wRS is negative! Incorrect parameters provided."
		bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
		# biasPrint = ["%.2f" % v for v in bias]
		# print('bias: ', biasPrint)
		alpha = self.modelParameters[1] # for how much the focal region augments attractiveness
		beta = self.modelParameters[2] # amplitude of the sigmoid function
		gamma = self.modelParameters[3] # position of the sigmoid function
		delta = self.modelParameters[4] # for how much the similarity to complement augments attractiveness
		epsilon = self.modelParameters[5] # for similarity to complementary focal region
		zeta = self.modelParameters[6] # for the steepness of the similarity to focal region
		eta = self.modelParameters[7] # for the steepness of the similarity to complementary focal region

		regionsCoded = self.regions
		strategies = self.strategies

		iV = self.code2Vector(strategies[i])
		if i==9: i = 0

		attractiveness = [x for x in bias] # start from bias
		# attactPrint = ["%.2f" % v for v in attractiveness]
		# print('attractiveness before WS and FRA\n', attactPrint)

		n = (score + 128) / 160 # normalizing score

		# print('sigmoid: ', sigmoid(n))
		# print('alpha * sigmoid: ', alpha * sigmoid(n))

		attractiveness[i] += alpha * self.sigmoid(n, beta, gamma) # Adding 'Win Stay'

		# attactPrint = ["%.2f" % v for v in attractiveness]
		# print('attractiveness with WS and stubbornness\n', attactPrint)

		jV = self.code2Vector(j)
		# print('jV ', jV)
		simils = [0] * 9
		for k in range(2,9): # do no consider 'rs' or 'all'
			kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
			kComp = [1 - x for x in kCoded]
			# print('Considering region: ', k)
			simils[k] = self.simil(jV, kComp, eta)
			# simils[k] = simil(jV, kCoded, epsilon)
		#
		# similsPrint = ["%.2f" % v for v in simils]
		# print('Similarity to complement\n', similsPrint)
		attractiveness += np.multiply(delta, simils)

		# attactPrint = ["%.2f" % v for v in attractiveness]
		# print('final attractiveness\n', attactPrint)

		# print('jV ', jV)
		simils = [0] * 9
		for k in range(1,9): # do no consider 'rs'
			kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
			# print('Considering region: ', k)
			simils[k] = self.simil(iV, kCoded, epsilon)
		#
		# similsPrint = ["%.2f" % v for v in simils]
		# print('Similarity to complement\n', similsPrint)
		attractiveness += np.multiply(zeta, simils)

		# attactPrint = ["%.2f" % v for v in attractiveness]
		# print('final attractiveness\n', attactPrint)

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
		# probsPrint = ["%.3f" % v for v in probs]
		# print('probs\n', probsPrint)
		# print('Suma: ', np.sum(probs))
		# get the selected strategy
		newStrategy = np.random.choice(range(9), p=probs)
		# print('newStrategy', newStrategy)
		# Determines if should not randomize RS
		if newStrategy == 0:
			n = (s + 128) / 160 # normalizing score
			beta = self.modelParameters[2] # amplitude of the sigmoid function
			gamma = self.modelParameters[3] # position of the sigmoid function
			if uniform(0,1) > self.sigmoid(n, beta, gamma):
				return newStrategy, True
			else:
				return newStrategy, False
		else:
			return newStrategy, False
		# return newStrategy, False

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
		dyad = str(Players[0].name)[:4] + str(Players[1].name)[:4]

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
				self.df = self.df.append(dfAux, ignore_index = True)
				# print(self.df)
				# print("Data from player " + str(k) + " has been saved")

			# Players determine their next strategy
			a = []
			sc = []
			a.append(Players[0].strategy)
			sc.append(Players[0].score)
			newStrategy, sameRS = self.chooseStrategy(Players[0].strategy, Players[0].score, both)
			Players[0].strategy = newStrategy
			# print('newStrategy:', newStrategy)
			if not sameRS:
				self.strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# else:
			# 	print('Do not randomize for player 0')

			a.append(Players[1].strategy)
			sc.append(Players[1].score)
			newStrategy, sameRS = self.chooseStrategy(Players[1].strategy, Players[1].score, both)
			# print('newStrategy:', newStrategy)
			if newStrategy == 0:
				newStrategy = 9
			Players[1].strategy = newStrategy
			if not sameRS:
				self.strategies[9] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))
			# else:
			# 	print('Do not randomize for player 1')

			# print('-----------------')
			# print('both', len(both))
			# print('scores: p0: ', sc[0], ' p1: ', sc[1])
			# print('Player 0 from region ', a[0], 'to region ', Players[0].strategy)
			# print('Player 1 from region ', a[1], 'to region ', Players[1].strategy)
			# print('End summary round ', i)
			# print('-----------------')

	def run_simulation(self):

		IT = self.gameParameters[4] # number of experiments in a set

		for h in range(0, IT):
			print("****************************")
			print("Running dyad no. ", h + 1)
			print("****************************\n")
			self.run_dyad()

	def get_DLL(self):

		data = self.df
		Num_Loc = self.gameParameters[2]

		# --------------------------------------------------
		# Working only with trials with "Unicorn_Absent"
		# --------------------------------------------------
		data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent')).reset_index()
		cols = ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
		data['Size_visited'] = data[cols].sum(axis=1)

		print("Sorting by Dyad, Player, Round...")
		data = data.sort_values(['Dyad','Player','Round'], \
		                ascending=[True, True, True])
		# Find difference in consistency and Total_visited_dyad ----
		print("Finding difference in consistency and Total_visited_dyad...")
		# Find the tiles visited by each player ----
		cols = ['Dyad','Player','Round','Joint','Size_visited']
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

		# print(str(len(data)) + " " + str(len(total)))
		assert(len(total) == len(data)), "Something wrong with finding Size_visited!"
		data['Total_visited_dyad'] = total

		# Division of labor Index (Goldstone)
		data['DLIndex'] = (data['Total_visited_dyad'] - data['Joint'])/(Num_Loc*Num_Loc)
		assert(all(data['DLIndex'] >= 0))

		self.df = data

	def get_measures(self, ifDistances, ifClassify):

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

		# --------------------------------------------------
		# Obtaining measures from players' performance
		# --------------------------------------------------
		# Find the accumulated score
		print("Finding accumulated score...")
		data['Score'] = data['Score'].map(lambda x: int(x))
		data['Ac_Score'] = data.sort_values(['Dyad','Player']).groupby('Player')['Score'].cumsum()
		# print data

		# --------------------------------------------------
		# Working only with trials with "Unicorn_Absent"
		# --------------------------------------------------
		data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent')).reset_index()
		# data = pd.DataFrame(data.groupby('Is_there').get_group('Unicorn_Absent'))


		# --------------------------------------------------
		# Continue obtaining measures
		# --------------------------------------------------
		Dyads = data.Dyad.unique()

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

		# print(str(len(data)) + " " + str(len(total)))
		assert(len(total) == len(data)), "Something wrong with finding Size_visited!"
		data['Total_visited_dyad'] = total
		# #print data[:3]
		assert(len(dif_cons) == len(data)), "Something wrong with finding dif_cons!"
		data['Dif_consist'] = dif_cons
		# print data['Dif_consist'][:3]

		# Division of labor Index (Goldstone)
		data['DLIndex'] = (data['Total_visited_dyad'] - data['Joint'])/(Num_Loc*Num_Loc)
		assert(all(data['DLIndex'] >= 0))

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
		    cols = cols1 + ['Player', 'Round']

		    print("Sorting by Player, Round...")
		    data = data.sort_values(['Player', 'Round'], \
		                    ascending=[True, True]).reset_index()

		    categoria = []
		    for player, Grp in data[cols].groupby(['Player']):
		        print("Working with player " + str(player) + "...")
		        for ronda, grp in Grp.groupby(['Round']):
		            # print "Obtaining path from round " + str(ronda) + "..."
		            path = [int(list(grp[c])[0]) for c in cols1]
		            # print path
		            # print "finding region..."
		            regionClassified = classifyRegion(path, 0.3, 0.55)
		            # print "Min: " + str(regionClassified)
		            categoria.append(regionClassified)

		dictionary = dict((i,j) for i,j in enumerate(regions))
		data['Category'] = data['Strategy'].map(dictionary)

		# --------------------------------------------------
		# Finding distance to closest focal region per round, per player
		# --------------------------------------------------
		if ifDistances == 1:
		    print("Finding distances to focal paths...")

		    # Deterimining list of columns
		    cols1 = ['a' + str(i) + str(j) \
		            for i in range(1, Num_Loc + 1) \
		            for j in range(1, Num_Loc + 1) \
		            ]
		    cols = cols1 + ['Player', 'Round']

		    print("Sorting by Player, Round...")
		    data = data.sort_values(['Player', 'Round'], \
		                    ascending=[True, True]).reset_index()

		    distancias = []
		    for player, Grp in data[cols].groupby(['Player']):
		        print("Working with player " + str(player) + "...")
		        for ronda, grp in Grp.groupby(['Round']):
		            # print "Obtaining path from round " + str(ronda) + "..."
		            path = [int(list(grp[c])[0]) for c in cols1]
		            # print path
		            # print "finding region..."
		            minDist = self.minDist2Focal(path, self.regions, 0.3)
		            # print "Min: " + str(minDist)
		            distancias.append(minDist)

		    data['Distancias'] = distancias

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

		print("Sorting by Dyad, Player, Round...")
		data = data.sort_values(['Dyad', 'Player', 'Round'], \
		                ascending=[True, True, True])#.reset_index(drop=True)

		self.df = data

		# ifSave = input("Do you want to save data to a file? (1=Yes/0=No): ")
		# ifSave = 0
		# if ifSave == 1:
		# 	outputFile = 'output.csv'
		# 	data.to_csv(outputFile, index=False)
		# 	print("Results saved to " + outputFile)

		print("Done!")
