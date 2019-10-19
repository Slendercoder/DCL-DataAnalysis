import numpy as np
import pandas as pd

data = pd.read_csv('output1.csv')
print(data[:6])

Num_Loc = 8

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
  print "Processing player: ", key
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
