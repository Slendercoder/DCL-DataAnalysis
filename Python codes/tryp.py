import numpy as np
import pandas as pd

SUM_SCORE = 0
N_OBS = 0
AV_SCORE = 0
CONTINUO = False
indicesIncluir = []

def nextRegion(si, s, r, i):
    global SUM_SCORE
    global N_OBS
    global AV_SCORE
    global CONTINUO
    global indicesIncluir
    if si == 'Presente':
        SUM_SCORE += s
        N_OBS += 1
        AV_SCORE = SUM_SCORE/N_OBS
        CONTINUO = True
        return r
    else:
        # print('AV_SCORE', AV_SCORE)
        # print('r', r)
        # print('SUM_SCORE', SUM_SCORE)
        if CONTINUO:
            if AV_SCORE > 30:
                SUM_SCORE = 0
                N_OBS = 0
                indicesIncluir.append([i, r])
                CONTINUO = False
                return r
            else:
                SUM_SCORE = 0
                N_OBS = 0
                indicesIncluir.append([i, 'RS'])
                CONTINUO = False
                return r
        else:
            return r

# Function to insert row in the dataframe
def Insert_row(row_number, df, row_value):
    # Starting value of upper half
    start_upper = 0

    # End value of upper half
    end_upper = row_number

    # Start value of lower half
    start_lower = row_number

    # End value of lower half
    end_lower = df.shape[0]

    # Create a list of upper_half index
    upper_half = [*range(start_upper, end_upper, 1)]

    # Create a list of lower_half index
    lower_half = [*range(start_lower, end_lower, 1)]

    # Increment the value of lower half by 1
    lower_half = [x.__add__(1) for x in lower_half]

    # Combine the two lists
    index_ = upper_half + lower_half

    # Update the index of the dataframe
    df.index = index_

    # Insert a row at the end
    df.loc[row_number] = row_value

    # Sort the index labels
    df = df.sort_index()

    # return the dataframe
    return df

########################

data = np.array([['Is', 'Score', 'Region'],
                 ['Ausente', 32, 'RS'],
                 ['Presente', 1, 'RS'],
                 ['Ausente', 10, 'LEFT'],
                 ['Ausente', 1, 'RIGHT'],
                 ['Presente', 20, 'RS'],
                 ['Presente', 15, 'RS'],
                 ['Ausente', 32, 'RIGHT'],
                 ['Presente', 32, 'RS'],
                 ['Presente', 32, 'RS'],
                 ['Ausente', 32, 'RIGHT']
                 ])

data = pd.DataFrame(data, columns=data[0,:])
data = data[1:]
data['Score'] = data['Score'].map(lambda x: int(x))
data['indice'] = data.index

print(data)

data.apply(lambda x: nextRegion(x['Is'], x['Score'], x['Region'], x['indice']), axis=1)

print(indicesIncluir)

for k in range(len(indicesIncluir)):
    c = indicesIncluir[len(indicesIncluir) - k - 1]
    if c[0] > 1:
        row_number = c[0] - 1
        row_value = [x for x in data.loc[row_number - 1]]
        print(row_value)
        row_value[0] = 'Ausente'
        row_value[2] = c[1]
        print(row_value)
        data = Insert_row(row_number, data, row_value)

print(data)

data['leadScore'] = data['Score'].transform('shift', periods=1)
data = pd.DataFrame(data.groupby('Is').get_group('Ausente')).reset_index()
data['correctedScore'] = data['leadScore'].transform('shift', periods=-1)

print(data[['Score', 'Region']])
