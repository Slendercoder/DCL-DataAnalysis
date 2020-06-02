import Measures as M
import pandas as pd


for contador in range(10):
    for modelo in ['MB', 'FR', 'WS']:
        archivo = "../Data/Confusion/Simulations/" + modelo + str(contador) + ".csv"
        print("Reading ", archivo, "...")
        data = pd.read_csv(archivo, index_col=False)
        print("Done!")

        data = M.get_measures(data, '0')

        data.to_csv(archivo, index=False)
        print("Results saved to " + archivo)

print("Done!")
