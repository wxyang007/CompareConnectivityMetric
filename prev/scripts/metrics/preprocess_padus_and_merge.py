# load the necessary packages
import os
import geopandas as gpd
import time

# paths to the folders and files
rootfolder = r'/Users/wenxinyang/Desktop/ProtConn'
datafolder = os.path.join(rootfolder, 'data')
wdpafolder = os.path.join(datafolder, 'wdpa')
cafolder = os.path.join(datafolder, 'CA')

path_cabuffer = os.path.join(cafolder, 'CA_500km_buffer.shp')
path_capa = os.path.join(cafolder, 'CA_500km_buffer_pa.shp')
path_mexpa = os.path.join(wdpafolder, 'MEX', 'mex_polygon.shp')

cabuffer = gpd.read_file(path_cabuffer)[['GEOID', 'geometry']]
capa = gpd.read_file(path_capa)
capa = capa.to_crs(cabuffer.crs)
mexpa = gpd.read_file(path_mexpa)
mexpa.crs == cabuffer.crs

capa_gap12 = capa[capa['GAP_Sts'].isin(['1', '2'])]

allpa = gpd.GeoDataFrame(columns = capa.columns, geometry = 'geometry')
allpa.crs = capa.crs
allpa = allpa.append(capa_gap12)
allpa = allpa.append(mexpa)
allpa = allpa.reset_index()
allpa.crs == cabuffer.crs

allpa_clipped = gpd.clip(allpa, cabuffer)
len(allpa_clipped)

path_final = os.path.join(cafolder, 'pa_ca_mex.shp')
allpa_clipped.to_file(path_final)