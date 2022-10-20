library(sf)
library(raster)
library(rgdal)
library(rgeos)


#=========Step1 preprocessing =========
# read in shpfile with: pa and non pa lands within CA and in 230km buffer
# on my laptop
wd <- file.path(getwd(), 'desktop', 'AProtconn','data','CA')
setwd(wd)
shp <- read_sf('ca_pa_file_fixed.shp')
# on desktop
# shp <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/ca_pa_file_fixed.shp')
summary(shp$OBJECTID)

# filter out only the pas in CA, these are the patches to iterate with
ca_pa <- shp[which((shp$ifPA == 1) & (shp$STATEFP == '06')), ]
# somehow filter out the inverse create new NAs, so have to export in GIS softwares
# before reading in
# non_ca_pa <- shp[(shp$ifPA != 1) | (shp$STATEFP != '06'), ]
# on my laptop
non_ca_pa <- read_sf('non_ca_pa/non_ca_pa.shp')
# on desktop
# non_ca_pa <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/non_ca_pa.shp')
summary(non_ca_pa$OBJECTID)
summary(ca_pa$OBJECTID)
ca_pa$ID <- 1:nrow(ca_pa)
n_ca_pa <- nrow(ca_pa)


#=========Step2 iterate by number of patches ========
n_iter = 1

# For every iteration, do the following:
for (n in 1:n_iter) {
  print(n)
  nodes_to_drop = sample(1:n_ca_pa, round(0.1*n_ca_pa), replace=F)
  filtered_pa <- ca_pa[!ca_pa$ID %in% nodes_to_drop,]
  
  # And then calculate all the metric values
  #=====1. Vector metrics computable in R=====
  
  #=====2. Raster metrics computable in R=====
  # convert vector to raster
  all_filtered_pa <- dplyr::group_by(filtered_pa) %>% summarize()
  r <- raster()
  extent(r) <- extent(filtered_pa)
  res(r) <- 1000 # is this a good grain size?
  ras_filtered_pa <- rasterize(all_filtered_pa, r, background = 0) # if you want the background, keep them as 0, if not keep as NA
  # but how do I account for edge effects
  plot(ras_filtered_pa)
  #=====3. Vector metrics that can be processed but not comptuable in R=====
  #=====4. Raster metrics that can be processed but not computable in R=====
  
  #=========Step3 create a dataframe to store all iterations (by number)========
  #=========Step4 iterate by total patch area========
  area = 0
  delid = list()
  
  sample(ca_pa$AREA_GEO, )
  #=========Step5 create a dataframe to store all iterations (by area)========

}


#=========Step6 visualization========