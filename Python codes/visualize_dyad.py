print("loading packages...")
from sys import argv
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib import gridspec
from FRA import sim_consist, FRASim, create_regions_and_strategies
print("Done!")

round_from = 0
round_to = 15
dyad_number = 19
Num_Loc = 8
step = 1. / Num_Loc
regions_names = ['ALL', 'NOT', 'BOT', 'TOP', 'LEF', 'RIG', 'IN', 'OUT']
regions, strategies = create_regions_and_strategies(Num_Loc)

regions_names = regions_names[:6]
regions = regions[:6]

def dibuja_FRAS(r1, r2, j, threshold):

    assert(len(r1) == Num_Loc * Num_Loc), "Incorrect region size 1!"
    assert(len(r2) == Num_Loc * Num_Loc), "Incorrect region size 2!"

    # Initializing Plot
    fig = plt.figure()
    spec = gridspec.GridSpec(ncols=2, nrows=4,
                         height_ratios=[3, 1, 1, 1])

    ax0 = fig.add_subplot(spec[0,0])
    ax1 = fig.add_subplot(spec[0,1])
    ax2 = fig.add_subplot(spec[1,0])
    ax3 = fig.add_subplot(spec[1,1])
    ax4 = fig.add_subplot(spec[2,0])
    ax5 = fig.add_subplot(spec[2,1])
    ax6 = fig.add_subplot(spec[3,0])
    ax7 = fig.add_subplot(spec[3,1])
    fig.subplots_adjust(left=0.1, bottom=0.05, right=0.9, top=0.95, wspace=0.1, hspace=0.2)

    ax0.set_title('Player 1')
    ax1.set_title('Player 2')
    ax0.get_xaxis().set_visible(False)
    ax1.get_xaxis().set_visible(False)
    ax0.get_yaxis().set_visible(False)
    ax1.get_yaxis().set_visible(False)
    ax2.set_ylim(0,1)
    ax3.set_ylim(0,1)
    ax4.set_ylim(0,1)
    ax5.set_ylim(0,1)
    ax6.set_ylim(0,1)
    ax7.set_ylim(0,1)
    ax2.set_yticklabels([])
    ax4.set_yticklabels([])
    ax6.set_yticklabels([])
    ax2.set_ylabel('Attracted\n to', fontsize=8)
    ax4.set_ylabel('Attraction toward\n Complement of\n Overlap', fontsize=8)
    ax6.set_ylabel('FRASim', fontsize=8)
    ax3.yaxis.tick_right()
    ax5.yaxis.tick_right()
    ax7.yaxis.tick_right()
    ax2.get_xaxis().set_visible(False)
    ax3.get_xaxis().set_visible(False)
    ax4.get_xaxis().set_visible(False)
    ax5.get_xaxis().set_visible(False)

    # Ploting regions
    tangulos1 = []
    tangulos2 = []
    for j in range(0, Num_Loc * Num_Loc):
        x = int(j) % Num_Loc
        y = (int(j) - x) / Num_Loc
        by_x = x * step
        by_y = 1 - (y + 1) * step
        if r1[j] == 1:
            tangulos1.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="black", alpha=1))
        if r2[j] == 1:
            tangulos2.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="black", alpha=1))
        if r1[j] == 1 and r2[j] == 1:
            tangulos1.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="red", alpha=1))
            tangulos2.append(patches.Rectangle(*[(by_x, by_y), step, step],\
			facecolor="red", alpha=1))

    for t in tangulos1:
        ax0.add_patch(t)

    for t in tangulos2:
        ax1.add_patch(t)

    # Plot attraction to focal region
    frasPL1 = [sim_consist(regionPL1, x) for x in regions]
    frasPL2 = [sim_consist(regionPL2, x) for x in regions]
    ax2.bar(regions_names, frasPL1)
    ax3.bar(regions_names, frasPL2)

    # Plot attraction toward complement of overlap
    frasPL1 = [0] + [sim_consist(joint, complement(x)) for x in regions[1:]]
    frasPL2 = [0] + [sim_consist(joint, complement(x)) for x in regions[1:]]
    ax4.bar(regions_names, frasPL1)
    ax5.bar(regions_names, frasPL2)

    # Plot FRASim
    frasPL1 = [FRASim(regionPL1, joint, x, Num_Loc) for x in regions]
    frasPL2 = [FRASim(regionPL2, joint, x, Num_Loc) for x in regions]
    ax6.bar(regions_names, frasPL1)
    ax7.bar(regions_names, frasPL2)

    ax6.axhline(y=threshold, linewidth=1, color='k')
    ax7.axhline(y=threshold, linewidth=1, color='k')

    plt.show()
    fig.savefig('myimage.eps', format='eps', dpi=1200)

def list_from_row(r, cols):

    lista = []
    for c in cols:
        lista.append(list(r[c])[0])

    return lista

def name_complement(r):

    if r == 'ALL':
        return 'NOTHING'
    elif r == 'NOTHING':
        return 'ALL'
    elif r == 'TOP':
        return 'BOTTOM'
    elif r == 'BOTTOM':
        return 'TOP'
    elif r == 'LEFT':
        return 'RIGHT'
    elif r == 'RIGHT':
        return 'LEFT'
    elif r == 'IN':
        return 'OUT'
    elif r == 'OUT':
        return 'IN'
    else:
        print('Error: Unknown region!', r)
        return None

def complement(r):
    return [1 - x for x in r]

# Opens the file with data from DCL experiment into a Pandas DataFrame
print("Reading data...")
data_archivo = "../Data/humans_only_absent.csv"
data = pd.read_csv(data_archivo, index_col=False)
print("Done!")

Dyads = list(data.Dyad.unique())
print("Dyads in file:")
print(Dyads)

data['DLindexMean'] = data.groupby(['Dyad'])['DLIndex'].transform('mean')
# data = data.loc[data['DLindexMean'] > 0.9]
# Dyads = list(data.Dyad.unique())
# print("Dyads high DLindex:")
# print(Dyads)

# dyad = input("Dyad to visualize:")
# dyad = int(dyad)
# assert(dyad in Dyads), "Error: dyad not in possible Dyads"
dyad = Dyads[dyad_number]

grp = data.groupby(['Dyad']).get_group(dyad)
Players = grp.Player.unique()
cols = ['a' + str(i) + str(j) for i in range(1, Num_Loc + 1) for j in range(1, Num_Loc + 1)]

print('Dyad', dyad)
print('Av. DLIndex', grp['DLindexMean'].unique())

for ronda, valores in grp.groupby(['Round']):

    if (ronda >= round_from) and (ronda <= round_to):
        print("Ronda " + str(ronda))

        Grp_player = valores.groupby(['Player'])
        Player = grp.Player.unique()
        aux1 = pd.DataFrame(Grp_player.get_group(Players[0]))
        aux2 = pd.DataFrame(Grp_player.get_group(Players[1]))
        RegPL1 = aux1['Category'].unique()[0]
        RegPL2 = aux2['Category'].unique()[0]
        print('RegPL1', RegPL1, 'RegPL2', RegPL2)
        regionPL1 = list_from_row(aux1, cols)
        regionPL2 = list_from_row(aux2, cols)
        joint = [regionPL1[i] * regionPL2[i] for i in range(len(regionPL1))]
        dibuja_FRAS(regionPL1, regionPL2, joint, threshold=0.68)
