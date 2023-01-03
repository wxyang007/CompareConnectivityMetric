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


path_text = r'/Users/wenxinyang/Desktop/AProtConn/data/CA/result/group12results_0809.csv'
# add cohesion to this data frame
# path_cohesion = r'/Users/wenxinyang/Desktop/ProtConn/data/CA/result/lsm_c_cohesion.txt'
df_group1 = pd.read_csv(path_text)
# df_lsm = pd.read_table(path_cohesion, delimiter=' ')
df = df_group1.dropna()


def normalizeField(fieldname):
    newname = 'norm_' + fieldname
    df[newname] = (df[fieldname] - df[fieldname].min())/(df[fieldname].max() - df[fieldname].min())
    return df

feat = ['dist', 'int_area', 'prox', 'centrality', 'degree', 'clustering_coeff','compartmentalization',
        'cohesion', 'gyrate', 'dIIC', 'dF', 'dAWF', 'dPC', 'dEC']
for f in feat:
    normalizeField(f)
norm_feat = ['norm_dist', 'norm_int_area', 'norm_prox', 'norm_centrality', 'norm_degree', 'norm_clustering_coeff',
             'norm_compartmentalization', 'norm_cohesion', 'norm_gyrate', 'norm_dIIC', 'norm_dF', 'norm_dAWF',
             'norm_dPC', 'norm_dEC']
label_feat = ['Nearest distance', 'Habitat within buffer', 'Proximity index', 'Betweenness centrality',
              'Degree', 'Clustering coefficient', 'Compartmentalization', 'Patch cohesion index', 'Area-weighted gyration',
              'dIIC', 'dF', 'dAWF', 'dPC', 'dEC']


#   pearson's correlation
def calculate_pvalues(df):
    df = df.dropna()._get_numeric_data()
    dfcols = pd.DataFrame(columns=df.columns)
    pvalues = dfcols.transpose().join(dfcols, how='outer')
    for r in df.columns:
        for c in df.columns:
            pvalues[r][c] = round(pearsonr(df[r], df[c])[1], 4)
    return pvalues
#
df_corr = df[norm_feat]
corr = df_corr.corr()
a = calculate_pvalues(corr)
corr.columns = label_feat
corr.index = label_feat

pval = corr.corr(method=lambda x, y: pearsonr(x, y)[1]) - np.eye(*corr.shape)
ifsig = pval.applymap(lambda x: 1 if x < 0.05 else 0)
corr_pval = corr*ifsig
p = pval.applymap(lambda x: ''.join(['*' for t in [0.01,0.05,0.1] if x<=t]))
corr_pval_matrix = corr.round(2).astype(str) + p

# remember to only show those with p-values less than 0.05
# sns.set(rc={'figure.figsize':(8,5)})
plt.figure(figsize = (10, 8))
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
plt.subplots_adjust(bottom = 0.2)

fig = ax.get_figure()
fig.savefig('corr.png')
#   pca
# importing required libraries

df_feat = df[norm_feat]
x = df.loc[:, norm_feat].values
x = StandardScaler().fit_transform(x)
pca = PCA()
principalComponents = pca.fit(x)  # use fit instead of fit_transform
# pca scores
principalComponents_arr = pca.fit_transform(x)

loadings = principalComponents.components_
principalComponents.explained_variance_ratio_
np.cumsum(principalComponents.explained_variance_ratio_)
principalComponents.explained_variance_
num_pc = principalComponents.n_features_

pc_list = ['pc' + str(i) for i in list(range(1, num_pc+1))]
loadings_df = pd.DataFrame.from_dict(dict(zip(pc_list, loadings)))
loadings_df['variable'] = loadings_df.columns.values
# principalDf = pd.DataFrame(data = principalComponents_arr, columns = ['pc1', 'pc2'])
# finalDf = pd.concat([principalDf, df_group1[['target']]], axis = 1)

principalComponents.explained_variance_
import seaborn as sns
import matplotlib.pyplot as plt
plot_transpose_loadings = loadings_df[['pc1', 'pc2', 'pc3']].transpose()
plot_transpose_loadings.columns = label_feat
plt.figure(figsize = (10, 3))
ax = sns. heatmap(plot_transpose_loadings, annot=True, cmap='Spectral')
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=25,
    horizontalalignment='right'
)
plt.subplots_adjust(bottom = 0.3)

ax = sns.heatmap(loadings_df[['pc1','pc2', 'pc3']], annot=True, cmap='Spectral')
plt.show()

# get 2D biplot
cluster.biplot(cscore=principalComponents_arr, loadings=loadings, labels=df.columns.values,
               var1=round(principalComponents.explained_variance_ratio_[0] * 100, 2),
               var2=round(principalComponents.explained_variance_ratio_[1] * 100, 2))

# get 3D biplot
cluster.biplot(cscore=principalComponents_arr, loadings=loadings, labels=label_feat,
               var1=round(principalComponents.explained_variance_ratio_[0] * 100, 2),
               var2=round(principalComponents.explained_variance_ratio_[1] * 100, 2),
               var3=round(principalComponents.explained_variance_ratio_[2] * 100, 2))
plt.show()

import os
os.getcwd()
# histograms
bins = np.linspace(0.0001,0.05,100)
plt.hist(df_group1['norm_dist'], bins=bins, alpha=0.5, label='Nearest Distance')
plt.hist(df_group1['norm_intarea'], bins=bins, alpha=0.5, label='Area within Buffer')
plt.legend(loc='upper right')
plt.show()