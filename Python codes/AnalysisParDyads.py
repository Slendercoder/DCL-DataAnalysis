import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set()

df_dyads = pd.read_csv('../Data/humans_only_absent.csv')
df_fitted = pd.read_csv('../Data/Dyads/dyads-fitted.csv')
df_classification = pd.read_csv('../Data/classificationDL.csv')

df_dyads = df_dyads.groupby('Dyad')['DLIndex'].mean().reset_index()

df_classification = df_classification[['Dyad', 'DL']]
df = pd.concat([df_dyads, df_classification], axis=1)
df.columns = ['Dyad', 'DLIndex', 'Dyad1', 'DL']
del df['Dyad1']

dict1 = {k: v for k, v in zip(df['Dyad'], df['DLIndex'])}
dict2 = {k: v for k, v in zip(df['Dyad'], df['DL'])}
df_fitted['DLindex'] = df_fitted['Dyad'].map(dict1)
df_fitted['DL'] = df_fitted['Dyad'].map(dict2)
df_fitted.sort_values(by='DLindex', ascending=False, inplace=True)

dic = {}
for dyad, grp in df_fitted.groupby('Dyad'):
    Maxi = grp['dev'].max(),
    Model = grp[grp['dev'] == Maxi]['Model'].tolist()[0]
    dic[dyad] = Model

df_fitted['Modelado'] = df_fitted['Dyad'].map(dic)

df = df_fitted[df_fitted['Model']=='FRA']

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

xs = list(df['alpha'])
ys = list(df['gamma'])
zs = list(df['DLindex'])
ax.scatter(xs, ys, zs)

ax.set_xlabel('alpha')
ax.set_ylabel('gamma')
ax.set_zlabel('DLindex')

plt.show()
