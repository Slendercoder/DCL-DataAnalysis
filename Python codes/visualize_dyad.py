print("loading packages...")
from sys import argv
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
print("Done!")

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")
script, data_archivo = argv
data = pd.read_csv(data_archivo, index_col=False)
print("Done!")

Dyads = list(data.Dyad.unique())
print("Dyads in file:")
print(Dyads)

data['DLindexMean'] = data.groupby(['Dyad'])['DLIndex'].transform('mean')
data = data.loc[data['DLindexMean'] > 0.9]
Dyads = list(data.Dyad.unique())
print("Dyads high DLindex:")
print(Dyads)

# dyad = input("Dyad to visualize:")
# dyad = int(dyad)
# assert(dyad in Dyads), "Error: dyad not in possible Dyads"
dyad = Dyads[3]

grp = data.groupby(['Dyad']).get_group(dyad)
Players = grp.Player.unique()
Num_Loc = 8
step = 1. / Num_Loc

for ronda, valores in grp.groupby(['Round']):

    if ronda < 11:
        print("Ronda " + str(ronda))

        # figs for visited locations
        fig4, axes4 = plt.subplots(1,2)
        for a in axes4:
            a.get_xaxis().set_visible(False)
            a.get_yaxis().set_visible(False)

        # Plot joint tiles
        Grp_player = valores.groupby(['Player'])
        Player = grp.Player.unique()
        aux1 = pd.DataFrame(Grp_player.get_group(Players[0]))
        # print aux1
        aux2 = pd.DataFrame(Grp_player.get_group(Players[1]))
        # print aux2

        # Plot visited locations per player
        play = 0
        for key, casillas in valores.groupby(['Player']):
            # print "Trabajando con el jugador " + str(key)
            tangulos = []
            for j in range(0, Num_Loc * Num_Loc):
                x = int(int(j) % Num_Loc)
                y = int((int(j) - x) / Num_Loc)
                # print "x: " + str(x + 1)
                # print "y: " + str(y + 1)
                colA = "a" + str(x + 1) + str(y + 1)
                # print colA
                by_x = x * step
                by_y = 1 - (y + 1) * step
            #     # print "by_x: " + str(by_x)
            #     # print "by_y: " + str(by_y)
                if (list(aux1[colA].unique())[0] == 1) and (list(aux2[colA].unique())[0] == 1):
                    tangulos.append(patches.Rectangle(*[(by_y, by_x), step, step],\
                        facecolor="red"))
                else:
                    if Players.tolist().index(key) == 0:
                        if list(aux1[colA].unique())[0] == 1:
                            tangulos.append(patches.Rectangle(*[(by_y, by_x), step, step],\
                                facecolor="black", alpha=1))
                    elif Players.tolist().index(key) == 1:
                        if list(aux2[colA].unique())[0] == 1:
                            tangulos.append(patches.Rectangle(*[(by_y, by_x), step, step],\
                                facecolor="black", alpha=1))

            # print "Dibujando recorrido jugador ", play
            for t in tangulos:
                axes4[play].add_patch(t)
            axes4[play].set_title('Player ' + str(key))
            play += 1

        # fig4.savefig(archivo)
        plt.show()
