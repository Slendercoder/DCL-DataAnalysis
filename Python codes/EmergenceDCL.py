# Class definition for solving "Seeking the unicorn" task
# Edgar Andrade-Lotero 2020
# Run with Python 3
# Run from main.py
# Requires FRA.py

from random import choice, uniform, random, sample, randint
from math import floor
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import FRA

DEB = False
IMPR = False
TO_FILE = True

CONTINUO = False
CONTADOR = 1
TOLERANCIA = 1

p_change = 0 # Include some random variation in region picked for the round

#################################
# FUNCTIONS
################################

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

# Define Experiment Object
class Experiment(object):
	'''Object defining the experiment and simulation with the following properties:
		gameParameters, modelParameters'''

	def __init__(self, gameParameters, modelParameters):
		assert(len(gameParameters) == 5), "Game parameters incorrect length!"
		assert(len(modelParameters) == 26), "Model parameters incorrect length!"
		self.gameParameters = gameParameters
		self.modelParameters = modelParameters

		# Create regions and strategies
		Num_Loc = gameParameters[2]
		regions, strategies = FRA.create_regions_and_strategies(Num_Loc)
		# for r in regions:
		# 	dibuja_region(r, Num_Loc)

		self.regions = regions
		self.strategies = strategies

		# Create data frame
		cols = ['Dyad', 'Round', 'Player', 'Answer', 'Time']
		cols += ['a' + str(i+1) + str(j+1) for i in range(0, Num_Loc) for j in range(0, Num_Loc)]
		cols += ['Score', 'Joint', 'Is_there', 'where_x', 'where_y', 'Strategy']
		self.df = pd.DataFrame(columns=cols)

	def run_dyad(self, TO_FILE=True):

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
		#				print("He is looking at location: " + str(strategies[Players[k].strategy]))
						# See if the strategy is not over...
						if (Players[k].strategy == 0) or (Players[k].strategy == 9):
							estrat = FRA.mean_strategy()
						else:
							estrat = FRA.shaky_hand(self.strategies[Players[k].strategy], 0)
						if j<len(estrat):
							search_place = estrat[j]
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

			reg1 = FRA.code2Vector(Players[0].where, Num_Loc)
			reg2 = FRA.code2Vector(Players[1].where, Num_Loc)
			j = FRA.code2Vector(both, Num_Loc)
			# Players determine their next strategy
			a = []
			sc = []
			a.append(Players[0].strategy)
			sc.append(Players[0].score)
			s = Players[0].score
			if place == -1:
				attract = FRA.attractiveness(reg1, s, j, 0, self.modelParameters, Num_Loc, self.regions)#, True)
				winner = np.argwhere(attract == np.amax(attract)).tolist()
				winner = [x[0] for x in winner]
				# print('attract:', attract)
				# print('winner:', winner)
				Players[0].strategy = choice(winner)
				# print('newStrategy:', Players[0].strategy)

			a.append(Players[1].strategy)
			sc.append(Players[1].score)
			s = Players[1].score
			if place == -1:
				attract = FRA.attractiveness(reg2, s, j, 1, self.modelParameters, Num_Loc, self.regions)#, True)
				winner = np.argwhere(attract == np.amax(attract)).tolist()
				winner = [x[0] for x in winner]
				Players[1].strategy = choice(winner)
				# print('newStrategy:', Players[1].strategy)

			if DEB:
				Is_there = " Absent" if place == -1 else " Present"
				print('-----------------')
				print('Unicorn ' + Is_there)
				print('both', len(both))
				print('scores: p0: ', sc[0], ' p1: ', sc[1])
				print('Player 0 from region ', FRA.nameRegion(a[0]), 'to region ', FRA.nameRegion(Players[0].strategy))
				print('Player 1 from region ', FRA.nameRegion(a[1]), 'to region ', FRA.nameRegion(Players[1].strategy))
				print('End summary round ', i)
				print('-----------------')
				FRA.dibuja_ronda(reg1, sc[0], reg2, sc[1], Num_Loc,self. modelParameters, self.regions, "Round: " + str(i) + Is_there)


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
