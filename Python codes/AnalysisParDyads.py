import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set()
fig, ax = plt.subplots(figsize=(10,10))

df_dyads = pd.read_csv('../Data/humans_only_absent.csv')
df_fitted = pd.read_csv('../Data/Dyads/dyads-fitted.csv')

df = df_dyads.groupby('Dyad')['DLIndex'].mean().reset_index()
aux = df_fitted.groupby('Model').get_group('FRA').reset_index()
df['alpha'] = aux['alpha']
df['delta'] = aux['delta']

# sns.pairplot(df_fitted)

# ax = fig.add_subplot(111, projection='3d')
#
# xs = df['alpha']
# ys = df['delta']
# zs = df['DLIndex']
# ax.scatter(xs, ys, zs, s=50, alpha=0.6, edgecolors='w')
#
# ax.set_xlabel('alpha')
# ax.set_ylabel('delta')
# ax.set_zlabel('DLindex')
#
plt.show()
