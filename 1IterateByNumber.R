library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(spatialEco)
library(Makurhini)
library(landscapemetrics)
# NOTE: there might be sth. wrong with the shapefile.
# It might have been cropped by county boundary or sth.

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


# read in the near table
# on my laptop
ca_neartab <- read.csv('near_tab_CA.csv')
# on desktop

#=========Step2 iterate by number of patches ========
n_iter = 1

# For every iteration, do the following:
for (n in 1:n_iter) {
  print(n)
  nodes_to_drop = sample(1:n_ca_pa, round(0.1*n_ca_pa), replace=F)
  filtered_pa <- ca_pa[!ca_pa$ID %in% nodes_to_drop, ]
  drop_pa <- ca_pa[ca_pa$ID %in% nodes_to_drop, ]
  drop_objectids <- unique(drop_pa$OBJECTID)
  
  # create a temporary near table that stores only kept rows
  filtered_neartab <- ca_neartab[!ca_neartab$IN_FID %in% drop_objectids, ]
  filtered_neartab <- filtered_neartab[!filtered_neartab$NEAR_FID %in% drop_objectids, ]
  # filtered_neartab <- filtered_neartab[!filtered_neartab$IN_FID == filtered_neartab$NEAR_FID, ]
  
  # And then calculate all the metric values
  #=====1. Vector metrics computable in R=====
  #====metric vector 1: nearest neighbor distance====
  # WARNING: would have to consider outer boundary if do not use pre-calculated near table
  nearest <- st_nearest_feature(filtered_pa) # get which is the nearest feature
  dist <- st_distance(filtered_pa,filtered_pa[nearest,], by_element=TRUE) # compute distance
  # nn_d <- aggregate(NEAR_DIST ~ IN_FID, filtered_neartab, function(x) min(x)) <- sth. wrong with the near table
  # 1.1 mean
  mean(dist)
  # 1.2 sd
  sd(dist)
  # 1.3 cv
  sd(dist)/mean(dist)
  
  #====metric vector 2: area of habitat within buffer====
  # WARNING: would have to take into consideration the buffered areas outside boundary...
  buffered <- st_buffer(filtered_pa, 10000)
  intersect <- st_intersection(buffered, filtered_pa)
  intersect$area <- st_area(intersect)
  intersect$ID.new <- 1:nrow(intersect)
  filtered_pa$contains <- st_contains(filtered_pa, intersect)
  filtered_pa$int_area <- 0
  for (x in 1:nrow(filtered_pa)) {
    a <- filtered_pa[x, ]$contains[1]
    b <- intersect[intersect$ID.new %in% a[[1]], ]
    #sum(b$area)
    filtered_pa[x,]$int_area <- as.numeric(sum(b$area))
  }
  intersected_area <- filtered_pa$int_area
  # 2.1 mean
  mean(interesected_area)
  # 2.2 standard deviation
  sd(intersected_area)
  # 2.3 coefficient of variation
  sd(intersected_area)/mean(intersected_area)
  
  #====metric vector 3: proximity index====
  prox <- proximity.index(filtered_pa, max.dist = 10000)
  # leave the others...
  # so one of the problems is that, some metrics are designed to be used at patch level
  # some others are designed to be used at ladnscape level
  
  #====metric vector 4: equivalent connected area=====
  # saura's metrics are problematic! ugh! though I can save them with exported tables
  
  #====metric vector 5: node betweenness centrality====
  
  
  #====metric vector 6 and after: correlation length and others====
  
  
  #====metric: flux and awf====
  
  #====metric: probability of connectivity====
  
  #====metric: ProtConn====
  
  
  #=====2. Raster metrics computable in R=====
  # convert vector to raster
  all_filtered_pa <- dplyr::group_by(filtered_pa) %>% summarize()
  r <- raster()
  extent(r) <- extent(filtered_pa)
  res(r) <- 10000 # I am testing with coarser grain for speed
  ras_filtered_pa <- rasterize(all_filtered_pa, r)
  # ras_filtered_pa <- rasterize(all_filtered_pa, r, background = 0) # if you want the background, keep them as 0, if not keep as NA
  # but how do I account for edge effects
  plot(ras_filtered_pa)
  
  #====metric raster 1: patch cohesion index==== needs revision
  cohesion <- lsm_c_cohesion(ras_filtered_pa, directions = 8) # class level
  
  #====metric raster 2: area-weighted patch gyration==== needs revision
  gyrate <- lsm_p_gyrate(ras, directions = 8, cell_center = FALSE) # patch level
  
  #=====3. Vector metrics that can be processed but not comptuable in R=====
  #=====4. Raster metrics that can be processed but not computable in R=====
  
  #=========Step3 create a dataframe to store all iterations (by number)========
  
  
  sample(ca_pa$AREA_GEO, )
  #=========Step4 create a dataframe to store all iterations (by area)========
  
}


#=========Step5 visualization========