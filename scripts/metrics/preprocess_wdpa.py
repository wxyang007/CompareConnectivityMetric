# load the necessary packages
import os
import geopandas as gpd
import time

# paths to the folders and files
rootfolder = r'/Users/wenxinyang/Desktop/ProtConn'
datafolder = os.path.join(rootfolder, 'data')
wdpafolder = os.path.join(datafolder, 'wdpa')
cafolder = os.path.join(datafolder, 'CA')

li_wdpafolders = [os.path.join(wdpafolder, 'WDPA_Mar2022_Public_shp_'+ str(i)) for i in range(3)]
li_ptwdpa = [os.path.join(f, 'WDPA_Mar2022_Public_shp-points.shp') for f in li_wdpafolders]
li_plwdpa = [os.path.join(f, 'WDPA_Mar2022_Public_shp-polygons.shp') for f in li_wdpafolders]

path_cabuffer = os.path.join(cafolder, 'CA_500km_buffer.shp')
cabuffer = gpd.read_file(path_cabuffer)[['GEOID', 'geometry']]


# function to select points
def filterWDPA(f, ifPt):
    f = f[~f['STATUS'].isin(['Proposed', 'Not Reported'])]
    f = f[f['DESIG_ENG'] != 'UNESCO-MAB Biosphere Reserve']
    f = f[f['ISO3'] != 'USA']
    f = f.explode(index_parts = False)
    # f = f[f['']]
    if ifPt == 1:
        f = f[f['REP_AREA'] > 0]
        f['Areapart'] = 0
    # f = f.explode(index_parts = False)
    return f

# ====== preprocess polygons =====
print('1 ', time.time())
# read in files for polygons
li_pl = [gpd.read_file(li_plwdpa[i]) for i in range(3)]

print('2 ', time.time())
# filter specific fields and lines to the WDPA
li_pl = [filterWDPA(li_pl[i], 0) for i in range(3)]

print('3 ', time.time())
# change projection of the files to intersect with cabuffer
li_pl = [f.to_crs(cabuffer.crs) for f in li_pl]

print('4 ', time.time())
# select by location
li_inner_pl = [gpd.sjoin(li_pl[i], cabuffer, how = 'inner', op = 'intersects') for i in range(3)]

print('5 ', time.time())
# merge the selected files
all_capl = gpd.GeoDataFrame(columns = li_inner_pl[0].columns, geometry='geometry')
for f in li_inner_pl:
    all_capl = all_capl.append(f)
print('6 ', time.time()) # 11.345 mins up to now

li_fields = ['WDPAID', 'geometry']
all_capl_final = (all_capl.reset_index())[li_fields]
all_capl_final.crs = cabuffer.crs # remember to check projection at all times
path_out_pl = os.path.join(wdpafolder, 'MEX', 'mex_polygon.shp')
all_capl_final.to_file(path_out_pl)

# ====== preprocess points ====== # unnecessary for this study in particular

# read in the point files from WDPA
li_pt = [gpd.read_file(li_ptwdpa[i]) for i in range(3)]

# filter specific fields and lines to the WDPA
li_pt = [filterWDPA(li_pt[i], 1) for i in range(3)]

# change projection so that they can be intersected with the CA buffer file
li_pt = [f.to_crs(cabuffer.crs) for f in li_pt]

# select by spatial location (op = intersects)
li_inner_pt = [gpd.sjoin(li_pt[i], cabuffer, how = 'inner', op = 'intersects') for i in range(3)]

# merge all three pt files
all_capt = gpd.GeoDataFrame(columns = li_inner_pt[0].columns, geometry='geometry')
for f in li_inner_pt:
    all_capt = all_capt.append(f)
all_capt['counts'] = 0

# convert pt file into polygon file
li_WDPAID = list(all_capt['WDPAID'].unique())
for id in li_WDPAID:
    n_id = 0
    for i in range(3):
        f = li_pt[i]
        gdf = f[f['WDPAID'] == id]
        n_id = n_id + len(gdf)
    print(n_id)
    all_capt.loc[all_capt['WDPAID'] == id, 'counts'] = n_id
all_capt['Areapart'] = all_capt['REP_AREA']/all_capt['counts']
# Turns out no point PA left for Mexico. But I'm saving this script in case of other uses.
