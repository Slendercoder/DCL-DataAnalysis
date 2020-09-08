from FRA import *
from scipy.optimize import minimize, Bounds

def err2_regions(region1, region2):
    sims1 = [sim_consist(region1, k) for k in focals]
    sims2 = [sim_consist(region2, k) for k in focals]
    return sum([(sims1[i] - sims2[i])**2 for i in range(len(sims1))])

def err2_sim_region(sims1, region, focals):
    sims2 = [sim_consist(region, k) for k in focals]
    return sum([(sims1[i] - sims2[i])**2 for i in range(len(sims1))])

def err2_sims(sims1, sims2):
    return sum([(sims1[i] - sims2[i])**2 for i in range(len(sims1))])

def err2_model(df, pl, modelParameters, Num_Loc, focals, estrategias, p):
    df['SimsPred'] = df.apply(lambda x: chooseStrategy(
                            x['Region'], x['Score'], x['Overlap'],
                            pl, modelParameters, Num_Loc, focals, estrategias),
                            axis = 1)
#    df['SimsPred'] = df['SimsPred'].apply(lambda x: shaky_hand(estrategias[np.argmax(x) + 1], p))
    df['SimsPred'] = df['SimsPred'].apply(lambda x: code2Vector(x, Num_Loc))
    df['SimsPred'] = df.apply(lambda x: err2_sim_region(x['Sims1'], x['SimsPred'], focals), axis=1)
    return df['SimsPred'].sum()

def MSD(params, data, focals, estrategias):

    pl = 0
    Num_Loc = 8
    p = 4
    er = err2_model(data, pl, params, Num_Loc, focals, estrategias, p)
#    print("Parametros: " + str(params) + "\nError: " + str(er))
    return er

def optimizar(parametros, minimos, maximos, PARS):
    x0 = np.array(parametros)
    bounds = Bounds(minimos, maximos)
    print("Finding fitting parameters. Please wait...")
    data = PARS[0]
    focals = PARS[1]
    estrategias = PARS[2]
    res = minimize(
        MSD,
        x0,
        args=(data, focals, estrategias),
        method='trust-constr',
        bounds=bounds,
        options={'verbose':1}
    )
    return res
