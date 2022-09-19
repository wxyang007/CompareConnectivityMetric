import geopandas as gpd
import os
import networkx as nx

rootfolder = r'/Users/wenxinyang/Desktop/ProtConn'
datafolder = os.path.join(rootfolder, 'data')
wdpafolder = os.path.join(datafolder, 'wdpa')
cafolder = os.path.join(datafolder, 'CA')

capa = gpd.read_file(os.path.join(cafolder, 'pa_ca_mex.shp'))
test = capa[:10]

# note: it only reads in points and lines
ntw = nx.read_shp(os.path.join(cafolder, 'pa_ca_mex.shp'))


import pandas as pd
import matplotlib.pyplot as plt
from wordcloud import WordCloud
bag = pd.read_csv(r'/Users/wenxinyang/Desktop/ProtConn/Lit/wordcloud_metrics.csv')

path_f = r'/System/Library/Fonts/Supplemental/Arial.ttf'
tuples = [tuple(x) for x in bag.values]
wordcloud = WordCloud(max_words=500,background_color='white',width=1600,height=800,font_path=path_f).generate_from_frequencies(dict(tuples))
plt.figure(figsize=(20,10), facecolor='k')
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.tight_layout(pad=0)
plt.show()

wordcloud.to_file(r'/Users/wenxinyang/Desktop/ProtConn/Lit/wordcloud1.png')