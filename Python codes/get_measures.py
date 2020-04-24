# Get measures from simulation
# Measures are: accumulated score, normalized score, number of tiles visited,
# consistency, total of tiles visited dyad, difference in consistency, DLIndex,
# distance to closest focal path, and fairness.

print("loading packages...")
from sys import argv
import pandas as pd
import Measures as M
print("Done!")

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
lista = input('Input as sequence, e.g. 124: ')

print('Opening database...')
data = pd.read_csv(data_archivo, index_col=False)
print("Done!")
# print(data)

data = M.get_measures(data, lista)

outputFile = 'output.csv'
data.to_csv(outputFile, index=False)
print("Results saved to " + outputFile)

print("Done!")
