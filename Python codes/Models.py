# Simulation of WSLS-probabilistic heuristic
# Edgar Andrade-Lotero 2018

print("Loading packages...")
from random import choice, uniform, random, sample, randint
from math import floor
# import matplotlib.pyplot as plt
# import string
import numpy as np
# import pandas as pd
from pathlib import Path
# import sys
# import matplotlib.pyplot as plt
print("Loaded!")

###########################
# Define model parameters
###########################
p = 0.5 # probability of there being a unicorn
Pl = 2 # number of players
Num_Loc = 8 # number of locations (squares in a row in the grid)
N = 60 # number of iterations per experiment
IT = 100 # number of experiments in a set
# theta = [0.14, 0.0674, 0.0123, 0.0009, 39, 405, 0.933, 0, 0, 0, 0] # WSLS LIKELIHOOD FIT
theta = [0.076, 0.05, 0, 0, 48, 398, 0.99, 1.53, 0.94, 3, 1.1] # FRA FIT TO DATA
wALL = float(theta[0])
wNOTHING = float(theta[1])
wDOWN = float(theta[2])
wUP = float(theta[2])
wLEFT = float(theta[2])
wRIGHT = float(theta[2])
wIN = float(theta[3])
wOUT = float(theta[3])
bias = [0, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
wRS = 1 - np.sum(bias)
assert(wRS >= 0), "Error: wRS is negative! Incorrect parameters provided."
bias = [wRS, wALL, wNOTHING, wDOWN, wUP, wLEFT, wRIGHT, wIN, wOUT]
biasPrint = ["%.2f" % v for v in bias]
print('bias: ', biasPrint)
alpha = theta[4] # for how much the focal region augments attractiveness
beta = theta[5] # amplitude of the sigmoid function
gamma = theta[6] # position of the sigmoid function
delta = theta[7] # for how much the similarity to complement augments attractiveness
epsilon = theta[8] # for similarity to complementary focal region
zeta = theta[9] # for the stubbornness
eta = theta[10] # for similarity to focal region

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

regionsCoded =[all, nothing, down, up, left, right, In, out]

###########################
# Define functions
###########################

# Define de players
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

# define attractiveness and choice functions
def sigmoid(x):
	return 1. / (1 + np.exp(-beta * (x - gamma)))

def code2Vector(strategy):
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
    dif = np.subtract(k, i)
    squares = np.multiply(dif, dif)
    distance = np.sqrt(np.sum(squares))
    return(np.exp(- o * distance))

def probabilities(i, score, j):

	attractiveness = [x for x in bias] # start from bias
	# attactPrint = ["%.2f" % v for v in attractiveness]
	# print('attractiveness before WS and FRA\n', attactPrint)

	n = (score + 128) / 160 # normalizing score

	# print('sigmoid: ', sigmoid(n))
	# print('alpha * sigmoid: ', alpha * sigmoid(n))
	if ((i != 0) and (i != 9)):
		attractiveness[i] += alpha * sigmoid(n) # Adding 'Win Stay'

	# attactPrint = ["%.2f" % v for v in attractiveness]
	# print('attractiveness with WS and stubbornness\n', attactPrint)

	jV = code2Vector(j)
	# print('jV ', jV)
	simils = [0] * 9
	for k in range(2,9): # do no consider 'rs' or 'all'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		kComp = [1 - x for x in kCoded]
		# print('Considering region: ', k)
		simils[k] = simil(jV, kComp, epsilon)
		# simils[k] = simil(jV, kCoded, epsilon)
	#
	# similsPrint = ["%.2f" % v for v in simils]
	# print('Similarity to complement\n', similsPrint)
	attractiveness += np.multiply(delta, simils)

	# attactPrint = ["%.2f" % v for v in attractiveness]
	# print('final attractiveness\n', attactPrint)

	iV = code2Vector(strategies[i])
	# print('jV ', jV)
	simils = [0] * 9
	for k in range(1,9): # do no consider 'rs'
		kCoded = regionsCoded[k - 1] # regionsCoded does not have 'RS'
		# print('Considering region: ', k)
		simils[k] = simil(iV, kCoded, eta)
	#
	# similsPrint = ["%.2f" % v for v in simils]
	# print('Similarity to complement\n', similsPrint)
	attractiveness += np.multiply(zeta, simils)

	# attactPrint = ["%.2f" % v for v in attractiveness]
	# print('final attractiveness\n', attactPrint)

	sum = np.sum(attractiveness)
	probs = [x/sum for x in attractiveness]

	return probs

def chooseStrategy(i, s, j):
	# Returns the next region according to biases
	# Input: i, which is the region that the player is in
	#		 s, which is the player's score
	#		 j, which is the overlapping region with the other player

#	print('Region: ', i)
	# get the probability vector
	probs = probabilities(i, s, j)
	# probsPrint = ["%.2f" % v for v in probs]
	# print('probs\n', probsPrint)
	# print('Suma: ', np.sum(probs))
	# get the selected strategy
	newStrategy = np.random.choice(letters, p=probs)
	# print('newStrategy', newStrategy)

	return newStrategy

# This is the code to run an entire experiment
# The experiment requires a set of strategies
def experiment(strategies):

	Exp = 'WSLS'

	# The variables that return player's scores and joint thorughout the rounds
	Jugador0_score = []
	Jugador1_score = []
	Joints = []

	# vector with starting probabilities
	probs = [0.67, 0.25, 0.08, 0, 0, 0, 0, 0, 0]
	probsPrint = ["%.2f" % v for v in probs]
	# print('Starting probabilities: ', probsPrint)

	# Create players
	Players = []
	for k in range(0, Pl):
		strat = np.random.choice(letters, p=probs)
		Players.append(player(False, "", strat, [], 0, False, int(uniform(0, 1000000))))
		# print "Player " + str(k) + " chose strategy " + strat

	# Create dyad name
	# dyad = gen_word(2, 4)
	dyad = str(Players[0].name)[:4] + str(Players[1].name)[:4]
	# dyad = "Autobots" + str(uniform(0,100))

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
		if uniform(0, 1) > 0.5:
			place = int(floor(uniform(0, Num_Loc * Num_Loc - 1)))
			Board[place] = 1
			# print "There is a unicorn at " + str(place)
		# else:
			# print "There is NO unicorn"

		# Start searchging for the unicorn
		for j in range(0, Num_Loc * Num_Loc + 1):
			# print "\nRunning iteration " + str(j)
			for k in range(0, Pl):
				# See if other player said present. If so, do the same
				if Players[1 - k].decision == "Present":
					Players[k].decision = "Present"
					# print "Player " + str(k) + " said Present"
					Players[k].ready = True
					break
				# If the other player did not say Present, and
				# current player is not ready, then...
				elif not Players[k].ready:
					# ...look at the location determined by the strategy
	#				print "Player " + str(k) + " is using strategy: " + \
	#					str(Players[k].strategy)
	#				print "He is looking on location: " + str(strategies[Players[k].strategy])
					# See if the strategy is not over...
					if j<len(strategies[Players[k].strategy]):
						search_place = strategies[Players[k].strategy][j]
						Players[k].where.append(search_place)
						# print "Player " + str(k) + " is searching at " + str(search_place)
						if Board[search_place] == 1:
							Players[k].decision = "Present"
							# print "Player " + str(k) + " said Present"
							Players[k].ready = True
						# else: print "Player " + str(k) + " found no unicorn"
					# Otherwise, say Absent
					else:
						# The strategy is over, so bet for Absent
						Players[k].decision = "Absent"
						# print "Player " + str(k) + " said Absent"
						Players[k].ready = True
				# Chechk if both players are ready. If so, stop search
				elif Players[1-k].ready == True:
					break
			else: continue
			break

		# print "\n"

		# Determine locations visited by both players
		# both = [x for x in Players[0].where if x in Players[1].where]
		both = list(set(Players[0].where).intersection(set(Players[1].where)))
		# print "Locations checked by both players: " + str(both)
		# print "The players checked on the same locations " + str(len(both)) + " times"

		# Determine individual scores
		for k in range(0, Pl):
			f.write(Exp + ",") # Exp, which is the value of the parameter, that is the number of complementary focal regions
			f.write(dyad + ",") # Dyad
			f.write(str(i + 1) + ",") # Round

			# print "Player " + str(k) + " checked " + str(len(Players[k].where)) + " locations"
			# print "Player " + str(k) + "\'s answer was: " + Players[k].decision
			if place == -1:
				# print "There was NO unicorn"
				if Players[k].decision == "Absent":
					# print "Player " + str(k) + "\'s answer is Correct!"
					Players[k].accuracy = True
					Players[k].score = Num_Loc*Num_Loc/2 - len(both)
					# print "Player " + str(k) + "\'s score this round is: " + \
					# 	str(Players[k].score)
				else:
					# print "Player " + str(k) + "\'s answer is Incorrect!"
					Players[k].accuracy = False
					Players[k].score = -Num_Loc*Num_Loc - len(both)
					# print "Player " + str(k) + "\'s score this round is: " + \
					# 	str(Players[k].score)
			else:
				# print "There was a unicorn"
				if Players[k].decision == "Present":
					# print "Player " + str(k) + "\'s answer is Correct!"
					Players[k].accuracy = True
					Players[k].score = Num_Loc*Num_Loc/2 - len(both)
					# print "Player " + str(k) + "\'s score this round is: " + \
					# 	str(Players[k].score)
				else:
					# print "Player " + str(k) + "\'s answer is Incorrect!"
					Players[k].accuracy = False
					Players[k].score = -Num_Loc*Num_Loc - len(both)
					# print "Player " + str(k) + "\'s score this round is: " + \
					# 	str(Players[k].score)
			f.write(str(Players[k].name) + ",") # Player
			f.write(Players[k].decision + ",") # Answer
			f.write(str(len(Players[k].where)) + ",") # Time
			for l in range(0, Num_Loc * Num_Loc):
				if l in Players[k].where:
					f.write("1,") # If visited location
				else:
					f.write("0,") # If not visited location
			for l in range(0, Num_Loc * Num_Loc):
				if l in Players[k].where:
					f.write(str(Players[k].where.index(l) + 1) + ",") # The time when he visited location
				else:
					f.write("0,") # If not visited location
			f.write(str(Players[k].score) + ",") # Score
			f.write(str(len(both)) + ",") # Joint
			if place == -1:
				f.write("Unicorn_Absent" + ",") # Is_there
				f.write("-1" + ",") # where_x
				f.write("-1" + ",") # where_y
			else:
				f.write("Unicorn_Present" + ",") # Is_there
				x = place % Num_Loc
				y = (place - x) / Num_Loc
				f.write(str(x) + ",") # where_x
				f.write(str(y) + ",") # where_y

			f.write(str(Players[k].strategy) + "\n") # player's strategy and End of line

			# print "Data from player " + str(k) + " has been saved to file"

		# Save score for players
		Jugador0_score.append(Players[0].score)
		Jugador1_score.append(Players[1].score)
		Joints.append(len(both))
		# print "----"
		# print "Player0's score is:" + str(Players[0].score)
		# print "Player0's accumulated score is:" + str(np.sum(Jugador0_score))
		# print "Player1's score is:" + str(Players[1].score)
		# print "Player1's accumulated score is:" + str(np.sum(Jugador1_score))
		# print "----"

		# The players determine their next strategy
		a = []
		sc = []
		for k in range(0, Pl):
			a.append(Players[k].strategy)
			sc.append(Players[k].score)
			newStrategy = chooseStrategy(Players[k].strategy, Players[k].score, both)
			Players[k].strategy = newStrategy
			strategies[0] = list(np.random.choice(Num_Loc * Num_Loc, np.random.randint(Num_Loc * Num_Loc)))

		# # print('-----------------')
		# print('both', len(both))
		# print('scores: p0: ', sc[0], ' p1: ', sc[1])
		# print('Player 0 from region ', a[0], 'to region ', Players[0].strategy)
		# print('Player 1 from region ', a[1], 'to region ', Players[1].strategy)
		# print('End summary round ', i)
		# print('-----------------')


# Create a set of n pairwise disjoint paths in the board
# Returns a dictionary with keys and paths
def create_strategies():
	# Define the strategies

	# size = int(Num_Loc * Num_Loc)
	# half_size = int(Num_Loc * Num_Loc / 2)
	# half_Num_Loc = int(Num_Loc / 2)
	#
	# # First pair of complementary paths -- UD
	# up = [1] * half_size + [0] * half_size
	# down = [1 - i for i in up]
	# # Second pair of complementary paths -- LR
	# right = []
	# for i in range(0, Num_Loc):
	# 	right += [0] * half_Num_Loc + [1] * half_Num_Loc
	#
	# left = [1 - i for i in right]
	# # Third pair of complementary paths -- AN
	# all = [1] * size
	# nothing = [0] * size
	# # Fourth pair of complementary paths -- IO
	# In = [0] * Num_Loc
	# for i in range(Num_Loc - 2):
	# 	In += [0] + [1] * (Num_Loc - 2) + [0]
	#
	# In += [0] * Num_Loc
	#
	# out = [1 - i for i in In]

	UP = []
	DOWN = []
	LEFT = []
	RIGHT = []
	IN = []
	OUT = []
	ALL = []
	NOTHING = []
	for i in range(size):
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

	return strategies

##########################################################################
#
#  Simulation starts here
#
##########################################################################

counter = 1
frames = []

# Open files to save data

archivo = "Simulated_data_raw"

my_file = Path(archivo + ".csv")

if my_file.exists():
	for i in range(1, 150):
		my_file = Path(archivo + str(i) + ".csv")
		if my_file.exists():
			print("Searching new index...")
		else:
			archivo += str(i)
			break

f = open(archivo + ".csv", 'w')

# Initialize files with headers
cols = 'Exp,Dyad,Round,Player,Answer,Time,'
for i in range(0, Num_Loc):
	for j in range(0, Num_Loc):
		cols += 'a' + str(i+1) + str(j+1) + ','
for i in range(0, Num_Loc):
	for j in range(0, Num_Loc):
		cols += 'b' + str(i+1) + str(j+1) + ','
cols += 'Score,Joint,Is_there,where_x,where_y,Strategy\n'
f.write(cols)

strategies = create_strategies()
letters = range(0, 9)

print("****************************")
print('Starting simulation with parameters: ')
print('wALL: ', theta[0])
print('wDOWN: ', theta[1])
print('wIN: ', theta[2])
print('alpha: ', alpha)
print('beta: ', beta)
print('gamma: ', gamma)
print('delta: ', delta)
print('epsilon: ', epsilon)
print('zeta: ', zeta)
print('eta: ', eta)

for h in range(0, IT):
	print("****************************")
	print("Running simulation no. ", h + 1)
	print("****************************\n")
	experiment(strategies)

f.close()
print ("Data saved in " + archivo + ".csv!")
