import numpy as np
import pandas as pd

def calcula_total_visited(x, y):
    return x + y - z

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

data = pd.read_csv('output2.csv')
data = data.dropna()

print(data[:3])

cols = ['a' + str(i) + str(j) for i in range(1,9) for j in range(1,9)]

data['Vector'] = data.apply(lambda x: np.array(x[cols]), axis=1)
data['VectorLAG1'] = data.groupby(['Dyad', 'Player'])['Vector'].transform('shift', 1)
data = data.dropna()
data['Consistency'] = data.apply(lambda x: calcula_consistencia(x['Vector'], x['VectorLAG1']), axis=1)
del data['VectorLAG1']

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
    print(aux3)
    total1 = dict(zip(aux3.Pair, aux3.total_visited))
    total = {**total, **total1}
    dif_cons1 = dict(zip(aux3.Pair, aux3.Dif_consist))
    dif_cons = {**dif_cons, **dif_cons1}

data['Pair'] = data.apply(lambda x: tuple([x['Dyad'], x['Round']]), axis=1)
data['Total_visited_dyad'] = data['Pair'].map(total)
data['Dif_consist'] = data['Pair'].map(dif_cons)
del data['Vector']
del data['Pair']

data.to_csv('prueba.csv', index=False)
