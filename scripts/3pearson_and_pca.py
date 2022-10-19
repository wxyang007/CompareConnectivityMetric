import pandas as pd
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import seaborn as sns
from scipy.stats import pearsonr
from bioinfokit.visuz import cluster
from pylab import savefig

# ======== preparation ======
df = pd.read_csv(r'/Users/wenxinyang/Desktop/GitHub/TemporalChangeConn/results/100iterations_Oct16_final.csv')
dfca = pd.read_csv(r'/Users/wenxinyang/Desktop/GitHub/TemporalChangeConn/results/ca.csv')

cols = list(("prot", "nn_d", "area_buff", "prox", "eca", "flux",
                 "awf", "pc", "protconn", "iic", "bc",
                 "degree", "clustering_coeff", "compartment","cohesion", "gyrate"))

full_metric_cols = ("Prot", "Nearest distance neighbor", "Habitat (area) within buffer",
                      "Proximity index", "Equivalent connected area", "Flux",
                      "Area weighted flux", "Probability of connectivity", "ProtConn",
                      "Integral index of connectivity", "Betweenness centrality",
                      "Node degree", "Clustering coefficient", "Compartmentalization",
                      "Patch cohesion index", "Area weighted mean patch gyration")

normcols = ['norm_' + x for x in cols]
rankcols = ['rank_' + x for x in cols]
dcols = ['d_' + x for x in cols]
pcols = ['p_' + x for x in cols]
rankpcols = ['rank_' + x for x in pcols]

def normalizeField(fieldname):
    newname = 'norm_' + fieldname
    df[newname] = (df[fieldname] - df[fieldname].min())/(df[fieldname].max() - df[fieldname].min())
    return df


def rankField(fieldname):
    newname = 'rank_' + fieldname
    df[newname] = df[fieldname].rank(ascending = False)
    return df


def calcChange(fieldname):
    ca_val = dfca.iloc[0][fieldname]
    changename = 'd_' + fieldname
    percentchangename = 'p_' + fieldname

    df[changename] = ca_val - df[fieldname]
    df[percentchangename] = 100*df[changename]/ca_val


def calculate_pvalues(df):
    df = df.dropna()._get_numeric_data()
    dfcols = pd.DataFrame(columns=df.columns)
    pvalues = dfcols.transpose().join(dfcols, how='outer')
    for r in df.columns:
        for c in df.columns:
            pvalues[r][c] = round(pearsonr(df[r], df[c])[1], 4)
    return pvalues


# no need to normalize for Pearson's correlation
def plotPearsonCorrMatrix(dfPearsonCorr, title):
    corr = dfPearsonCorr.corr()
    # a = calculate_pvalues(corr)
    corr.columns = full_metric_cols
    corr.index = full_metric_cols
    pval = corr.corr(method=lambda x, y: pearsonr(x, y)[1]) - np.eye(*corr.shape)
    ifsig = pval.applymap(lambda x: 1 if x < 0.05 else 0)
    corr_pval = corr * ifsig
    p = pval.applymap(lambda x: ''.join(['*' for t in [0.01, 0.05, 0.1] if x <= t]))
    # corr_pval_matrix = corr.round(2).astype(str) + p

    plt.figure(figsize=(15, 15))
    ax = sns.heatmap(
        corr_pval,
        vmin=-1, vmax=1, center=0,
        cmap=sns.diverging_palette(20, 220, n=200),
        square=True
    )
    ax.set_xticklabels(
        ax.get_xticklabels(),
        rotation=35,
        horizontalalignment='right'
    )
    plt.subplots_adjust(bottom=0.2)
    plt.title(label = title, fontsize = 15)
    fig = ax.get_figure()

# ====== for value ======
dfval = df[cols]
plotPearsonCorrMatrix(dfval, "Correlation among metric values")

# ====== for value rank =======
for f in cols:
    rankField(f)
dfrankcorr = df[rankcols]
plotPearsonCorrMatrix(dfrankcorr, "Correlation among metric value ranks")

# ====== for percent change ======
for f in cols:
    calcChange(f)
dfpcorr = df[pcols]
plotPearsonCorrMatrix(dfpcorr, "Correlation among percent changes")

# ====== for percent change rank ======
for f in pcols:
    rankField(f)
dfrankpcorr = df[rankpcols]
plotPearsonCorrMatrix(dfrankpcorr, "Correlation among ranks of percent changes")


# ======= pca ======
