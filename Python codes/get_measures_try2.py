# Get measures from simulation
# Measures are: accumulated score, normalized score, number of tiles visited,
# consistency, total of tiles visited dyad, difference in consistency, DLIndex,
# distance to closest focal path, and fairness.

print("loading packages...")
from sys import argv
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import FRA
import Measures as M
print("Done!")

# --------------------------------------------------
# Parameters (Global variables)
# --------------------------------------------------
Num_Loc = 8
CLASIFICAR = False

CONTINUO = False
CONTADOR = 1

# List of columns for region visited
cols1 = ['a' + str(i) + str(j) \
for i in range(1, Num_Loc + 1) \
for j in range(1, Num_Loc + 1) \
]

regionsCod = ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:', # ALL
                  '', # NOTHING
                  'GHIJKLMNOPQRSTUVWXYZ0123456789;:', # DOWN
                  'abcdefghijklmnopqrstuvwxyzABCDEF', # UP
                  'abcdijklqrstyzABGHIJOPQRWXYZ4567', # LEFT
                  'efghmnopuvwxCDEFKLMNSTUV012389;:', # RIGHT
                  'jklmnorstuvwzABCDEHIJKLMPQRSTUXYZ012', # IN
                  'abcdefghipqxyFGNOVW3456789;:' # OUT
                  ]

regionsCoded = [FRA.code2Vector(FRA.lettercode2Strategy(x, Num_Loc), Num_Loc) for x in regionsCod]

######################################
######################################
######################################
######################################


# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")
script, data_archivo = argv

print('Please input the measures to be obtained:')
print('0: Complete from simulation (for simulated data only)')
print('1: Classify regions')
print('2: Correct scores')
print('3: Estimate blocks')
print('4: Keep only absent')
print('5: Find max similarity')
lista = input('Input as, e.g., 124: ')

print('Opening database...')
data = pd.read_csv(data_archivo, index_col=False)
print("Done!")
# print(data)

data = M.get_measures(data, lista)
outputFile = 'output.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
