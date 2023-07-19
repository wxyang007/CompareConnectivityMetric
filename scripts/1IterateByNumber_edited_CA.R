# Simulations for California
# Author: Wenxin Yang
# Date: Sept, 2022
# Revised: July, 2023

# NOTE: if rerun a specific metric, please read in previous output csv to make sure
# the same PAs are dropped for each simulation.
# or set seed before the first run

# By default, all distances are in meters
# all areas are in km2

# read in the packages
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(spatialEco)
library(Makurhini)
library(landscapemetrics)


#=========Step1 preprocessing =========
# read in shapefile with: pa and non pa lands within CA and in 230km buffer
setwd('~/')
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA'
setwd(wd)
coneforfolder = 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/conefor_results_3'
# note for myself: conefor_results_3: relax the restriction on 10km in the near table

shp <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/ca_pa_file_fixed.shp')
summary(shp$OBJECTID)

ca_bound <- read_sf('ca-state-boundary/CA_State_TIGER2016.shp') # California

# filter out only the pas in CA, these are the patches to iterate with
ca_pa <- shp[which((shp$ifPA == 1) & (shp$STATEFP == '06')), ] # PAs of California

ca_bound_reproj <- st_transform(ca_bound, crs(ca_pa))
# somehow filter out the inverse create new NAs, so have to export in GIS softwares
# before reading in
# non_ca_pa <- shp[(shp$ifPA != 1) | (shp$STATEFP != '06'), ]
# on my laptop
# non_ca_pa <- read_sf('non_ca_pa/non_ca_pa.shp')

non_ca_pa <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/non_ca_pa.shp')
non_ca_pa$ID = -1
#non_ca_pa <- shp[which(shp$ISO3 != 'LBR'),]

non_ca_pa$ifTarget = 0
ca_pa$ID <- 1:nrow(ca_pa)
n_ca_pa <- nrow(ca_pa)


# read in the near table
ca_neartab <- read.csv('near_tab_CA.csv')
#ca_neartab <- read.csv('LiberiaPA/neartab_liberia_neighb.csv')
ca_neartab$OBJECTID <- NULL
ca_neartab$NEAR_RANK <- NULL

for (i in 1:nrow(ca_neartab)){
  if(ca_neartab$IN_FID[i] > ca_neartab$NEAR_FID[i]){
    id1 <- ca_neartab$IN_FID[i]
    id2 <- ca_neartab$NEAR_FID[i]
    ca_neartab$IN_FID[i] <- id2
    ca_neartab$NEAR_FID[i] <- id1
  }
}
#nrow(ca_neartab)
ca_neartab <- unique(ca_neartab)


# create a table to store all the iterations
iters <- data.frame(matrix(ncol = 29, nrow = 0))

colnames(iters) <- c("num_iter", "objectids_to_keep", "prot",
                     "nn_d", "area_buff", "prox", "eca", "flux",
                     "awf", "pc", "protconn", 
                     "protconn_prot", "protconn_unprot", "protconn_trans", 
                     "protconn_within", "protconn_contig",
                     "iic", "bc", 
                     "degree", "clustering_coeff", "compartment","cohesion", "gyrate",
                     "aw_gyrate",
                     "mean_patch_area", "mean_patch_peri", "mean_patch_shape", "total_edge",
                     "edge_density"
                     )

#=========Step2 iterate by number of patches ========
n_iter = 100

iters_result_path <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results/100iterations_Dec06_rev.csv'
iters_result <- read.csv(iters_result_path, sep = ",")

# For every iteration, do the following:
for (n in 1:n_iter) {   # for CA
#for (n in 1:nrow(dat)) {  # for Liberia
  print(n)
  start.time <- Sys.time()
  
  # randomly drop 10% of the nodes
  #OBJECTIDs_to_drop = sample(min(ca_pa$OBJECTID):max(ca_pa$OBJECTID), round(0.1*n_ca_pa), replace=F)
  OBJECTIDs_to_keep = as.list(as.numeric(strsplit(iters_result[n,]$objectids_to_keep, ",")[[1]]))
  #OBJECTIDs_to_keep <- unique(ca_pa$OBJECTID)
  #OBJECTIDs_to_drop <- c(dat$Var1[n], dat$Var2[n])
  
  # PAs to keep
  filtered_pa <- ca_pa[ca_pa$OBJECTID %in% OBJECTIDs_to_keep, ]
  #filtered_pa <- ca_pa[!ca_pa$OBJECTID %in% OBJECTIDs_to_drop, ]
  filtered_pa$ifTarget = 1
  #OBJECTIDs_to_keep = unique(filtered_pa$OBJECTID)
  iters[n, ]$num_iter = n
  iters[n, ]$objectids_to_keep = paste(OBJECTIDs_to_keep, collapse = ',')
  
  # PAs dropped
  #dropped_pa <- ca_pa[ca_pa$OBJECTID %in% OBJECTIDs_to_drop, ]
  dropped_pa <- ca_pa[!ca_pa$OBJECTID %in% OBJECTIDs_to_keep, ]
  OBJECTIDs_to_drop <- unique(dropped_pa$OBJECTID)
    
  # PAs to keep and non-PAs and transboundary PAs
  #keptpa_and_nonpa <- rbind(filtered_pa, non_ca_pa)
  keptpa_and_nonpa <- rbind(filtered_pa, non_ca_pa)
  
  # PAs to keep and transboundary PAs
  all_only_pa <- keptpa_and_nonpa[keptpa_and_nonpa$ifPA == 1, ]

  #all_only_pa <- keptpa_and_nonpa
  OBJECTIDs_all_pas = unique(all_only_pa$OBJECTID)
  
  
  # near tab of only selected PAs in CA
  filteredpa_neartab <- ca_neartab[(ca_neartab$IN_FID %in% OBJECTIDs_to_keep) & 
                                     (ca_neartab$NEAR_FID %in% OBJECTIDs_to_keep), ]
  
  # near tab that excludes dropped PAs
  no_droppedpa_neartab <- ca_neartab[!(ca_neartab$IN_FID %in% OBJECTIDs_to_drop),]
  no_droppedpa_neartab <- no_droppedpa_neartab[!(no_droppedpa_neartab$NEAR_FID %in% OBJECTIDs_to_drop), ]
  
  # near tab of PAs to keep and transboundary PAs
  all_only_pa_neartab <- ca_neartab[(ca_neartab$IN_FID %in% OBJECTIDs_all_pas),]
  all_only_pa_neartab <- all_only_pa_neartab[(all_only_pa_neartab$NEAR_FID %in% OBJECTIDs_all_pas), ]
  
  
  #=====1. Vector metrics computable in R=====
  #====metric vector 1: nearest neighbor distance====
  # the computation way
  # nearest <- st_nearest_feature(filtered_pa) # get which is the nearest feature
  # dist_pa <- st_distance(filtered_pa,filtered_pa[nearest, ], by_element=TRUE) # compute distance
  # the near table way
  nn_d1 <- aggregate(NEAR_DIST ~ IN_FID, all_only_pa_neartab, function(x) min(x))
  nn_d2 <- aggregate(NEAR_DIST ~ NEAR_FID, all_only_pa_neartab, function(x) min(x))
  nn_d1 <- nn_d1[nn_d1$IN_FID %in% OBJECTIDs_to_keep, ]
  nn_d2 <- nn_d2[nn_d2$NEAR_FID %in% OBJECTIDs_to_keep, ]
  colnames(nn_d1) <- c('ID', 'NEAR_DIST1')
  colnames(nn_d2) <- c('ID', 'NEAR_DIST2')
  
  nn_d <- merge(nn_d1, nn_d2, by='ID', all = TRUE)[-1]
  nn_d[is.na(nn_d)] <- 1000000000
  nn_d$NEAR_DIST <- apply(nn_d, 1, min)
  # summary(nn_d$NEAR_DIST)
  iters[n, ]$nn_d = mean(nn_d$NEAR_DIST)
  
  #====metric vector 2: area of habitat within buffer====
  filtered_pa$int_area <- 0
  buffered <- st_buffer(filtered_pa, 10000)
  st_agr(buffered) = "constant"
  st_agr(all_only_pa) = "constant"
  intersect <- st_intersection(buffered, all_only_pa)
  
  intersect$area <- st_area(intersect)
  intersect$ID.new <- 1:nrow(intersect)
  
  buffered$contains <- st_contains(buffered, intersect)
  
  buffered$int_area <- 0
  
  for (x in 1:nrow(buffered)) {
    a <- buffered[x, ]$contains[1]
    b <- intersect[intersect$ID.new %in% a[[1]],]
    buffered[x,]$int_area <- as.numeric(sum(b$area))
  }
  
  intersected_area_buff <- buffered$int_area
  # print(mean(intersected_area_buff))
  
  #iters_result[n, ]$area_buff_1 = mean(intersected_area_buff)
  iters[n, ]$area_buff = mean(intersected_area_buff)
  
  #====metric vector 3: proximity index====
  all_only_pa$prox <- proximity.index(all_only_pa, max.dist = 10000)
  # results_pa_prox <- all_only_pa[all_only_pa$ifTarget == 1, ]$prox
  results_pa_prox <- all_only_pa[all_only_pa$ifTarget == 1, ]$prox
  
  # print(mean(results_pa_prox))
  
  iters[n, ]$prox = mean(results_pa_prox)
  
  #====metric vector 4: equivalent connected area [conefor]=====
  # the following code that calls conefor command line is adapted from Makurhini
  nodedf <- all_only_pa[c("OBJECTID", "AREA_GEO", "ifPA", "STATEFP")]
  nodedf <- nodedf[nodedf$ifPA == 1, ]
  nodedf$AREA_GEO[which(nodedf$STATEFP != '06')] <- 0
  nodedf <- nodedf[c("OBJECTID", "AREA_GEO")]
  nodedf$geometry <- NULL
  
  nodesconefor <- unique(nodedf$OBJECTID)
  connectiondf <- no_droppedpa_neartab[(no_droppedpa_neartab$IN_FID %in% nodesconefor) &
                                         (no_droppedpa_neartab$NEAR_FID %in% nodesconefor), ]
  connectiondf$OBJECTID <- NULL
  connectiondf$NEAR_RANK <- NULL
  
  thisiter <- paste0(coneforfolder, "\\", n)
  dir.create(thisiter)
  
  # temp <- paste0(tempdir(), "\\tempconefor", sample(1:1000, 1, replace = T))
  #if(dir.exists(temp)){
  #  unlink(temp, recursive = TRUE)
  #}
  #dir.create(temp, recursive = TRUE)
  #file.copy(coneforpath, temp, overwrite = T)
  conefor_exe <- paste0(coneforfolder, '/coneforWin64.exe')
  
  setwd(thisiter)
  nodefile <- paste0(n, '_node.txt')
  write.table(nodedf, nodefile, row.names = FALSE, col.names = FALSE, sep = "\t")
  confile <- paste0(n, '_distance.txt')
  write.table(connectiondf, confile, row.names = FALSE, col.names = FALSE, sep = "\t")
  

  p1 <- paste(conefor_exe, "-nodeFile", nodefile, "-conFile", confile, 
              "-t dist -confProb 10000 0.5 -PC -F -AWF -confAdj 10000 -IIC -BC onlyoverall")
  
  shell(p1, intern = TRUE)

  # you can use shell in windows system
  # unlink(temp, recursive = TRUE)
  setwd(wd)
  
  #====metric vector 5: flux and awf [conefor]====
  
  #====metric vector 6: probability of connectivity [conefor]====
  
  #====metric vector 7: ProtConn [conefor and R]====
  protconn<- MK_ProtConn(filtered_pa, ca_bound_reproj, area_unit = "m2", distance = list(type = "edge"),
                  distance_thresholds = 10000, probability = 0.5, transboundary = 230000,
                  transboundary_type = "nodes", protconn_bound = FALSE)
  iters[n, ]$prot = protconn[1, ]$Percentage
  iters[n, ]$protconn = protconn[3, ]$Percentage
  iters[n, ]$protconn_prot = protconn[8, ]$Percentage
  iters[n, ]$protconn_trans = protconn[9, ]$Percentage
  iters[n, ]$protconn_unprot = protconn[10, ]$Percentage
  iters[n, ]$protconn_within = protconn[11, ]$Percentage
  iters[n, ]$protconn_contig = protconn[12, ]$Percentage
  
  # this script works but needs to be edited
  # what to edit: 1) rbind filtered_pa with ca-non-pas; 2) set protconn_bound to be TRUE;
  # 3) think about transboundary threshold; 4) get the specific value for ProtConn from the result table
  
  #====metric vector 8: node betweenness centrality [conefor]====
  centrality <- MK_RMCentrality(nodes = filtered_pa,
                                # area_unit = 'm',
                                distance = list(type = 'edge'),
                                distance_thresholds = 10000,
                                probability = 0.5,
                                write = NULL)
  iters[n, ]$bc = mean(centrality$BWC)
  
  #====metric vector 9 and after: clustering coefficient and others====
  neartab_corr <- all_only_pa_neartab[all_only_pa_neartab$NEAR_DIST <= 10000, ]
  neartab_corr$OBJECTID <- NULL
  neartab_corr$NEAR_RANK <- NULL
  neartab_corr$NEAR_DIST <- NULL
  
  # create a dataframe to store 'neighbors' for each patch
  r1 <- neartab_corr %>%
    group_by(IN_FID) %>%
    summarise(neighbors = list(NEAR_FID))
  r2 <- neartab_corr %>%
    group_by(NEAR_FID) %>%
    summarise(neighbors = list(unique(IN_FID)))
  r3 <- merge(r1, r2, by.x='IN_FID', by.y='NEAR_FID', all.x=TRUE, all.y=TRUE)
  
  r3$neighbors <- apply(r3, 1, FUN = function(x) unique(append(x$neighbors.x, x$neighbors.y)))
  r3$neighbors <- apply(r3, 1, FUN=function(x) x$neighbors[!is.na(x$neighbors)])
  r3 <- r3[c('IN_FID', 'neighbors')]
  # do the calculations for all PAs (both within CA and its transboundary neighbors)
  # will throw these in the end
  pa_corr <- all_only_pa[c('OBJECTID', 'ifPA', 'ifTarget', 'AREA_GEO')]
  #pa_corr <- all_only_pa[c('OBJECTID', 'ISO3', 'AREA_GEO')]
  tab_corr <- merge(pa_corr, r3, by.x = 'OBJECTID', by.y = 'IN_FID',
                    all.x = TRUE)
  tab_corr$geometry <- NULL
  # tab_corr <- tab_corr[tab_corr$ifPA == 1, ]
  
  id_pa <- OBJECTIDs_all_pas
  id_pa <- id_pa[!duplicated(id_pa)]
  li_id_pa <- list(id_pa)[[1]]
  
  tab_corr <- tab_corr[c('OBJECTID', 'neighbors')]
  tab_corr$common_neighbors <- 0
  tab_corr$num_neighbors <- 0
  tab_corr$pairs_neighbors <- 0
  
#  for (ni in 1:nrow(tab_corr)){
#    i = tab_corr[ni, ]$OBJECTID # get the objectid of the nith row
#    li_ni = tab_corr[ni, ]$neighbors[[1]] # get its neighbors
#    li_ni_pa = Reduce(intersect, list(li_ni, li_id_pa)) # get its neighbors that are PAs
#    if (is.na(li_ni)) {
#      print(ni)
#      print('has no neighbors')
#    } else {
#      if (li_ni != li_ni_pa) {
#        print(ni)
#        print('includes non-pa')
#      }
#    }
#    tab_corr[ni, ]$neighbors = list(li_ni_pa)
#  }
  
  for (ni in 1:nrow(tab_corr)){
    # ni = 1
    i = tab_corr[ni, ]$OBJECTID
    li_ni = tab_corr[ni, ]$neighbors[[1]]
    if (is.na(li_ni)){
      l = 0
    } else (l = length(li_ni))
    li_cni = list()
    m = 0
    q = 0
    
    # The following loop gets common neighbors
    # Specifically, it loops through every pair of neighbor patches of the target patch
    # And test if the pair of patches contain each other within their own neighbor list
    # i x (i+1:n), where n is the total number of neighbors of the target patch
    # So there are two loops, the first one for i goes from 1:n
    # The second goes from i+1:n
    # Note: if the patch itself does not have any neighbors, it should set common neighbors to 0
    
    if (length(li_ni) > 1){
      for (nj in 1:l){
        # nj = 2
        ifn = 0
        j = li_ni[nj]
        nj1 = nj+1
        if (j != i) {
          li_nj = tab_corr[tab_corr$OBJECTID == j, ]$neighbors[[1]]
          if (nj < l) {
            for (nk in nj1:l){
              k = li_ni[nk]
              if (k != j & k %in% li_nj) {
                m = m + 1 
                li_cni <- append(li_cni, k)
                li_cni <- append(li_cni, j)
              } else {m = m + 0}
            }
            li_cni <- li_cni[!duplicated(li_cni)]
            q = length(li_cni)
          }
        }
      }
    }
    tab_corr[ni, 5] = m # pairs of neighbors
    tab_corr[ni, 4] = l # num of neighbors
    tab_corr[ni, 3] = q # common neighbors
    # print(paste0(ni, ' pairs of neighbors: ', m, '. common neighbors: ', n))
  }
  
  tab_corr$test = (tab_corr$common_neighbors*tab_corr$common_neighbors - tab_corr$pairs_neighbors) >= 0
  nrow(tab_corr[tab_corr$test == FALSE,])
  nrow(tab_corr[tab_corr$common_neighbors > tab_corr$num_neighbors, ])
  
  # delete transboundary patches
  tab_corr <- tab_corr[tab_corr$OBJECTID %in% OBJECTIDs_to_keep, ]
  # replace NAs with 0s
  tab_corr[is.na(tab_corr)] <- 0
  
  #=======sub metric 1: degree of connectedness=======
  tab_corr$degree <- tab_corr$num_neighbors/length(OBJECTIDs_to_keep)
  #=======sub metric 2: clustering coefficient=======
  tab_corr$clustering_coeff <- tab_corr$common_neighbors/tab_corr$num_neighbors
  tab_corr[is.na(tab_corr$clustering_coeff), ]$clustering_coeff <- 0
  
  #======sub metric 3: compartmentalization======
  # 3-1 get neighbors' node degrees
  tab_corr$neighbors_degree <- 0
  tab_corr$average_neighbors_degree <- 0
  for (ni in 1:nrow(tab_corr)){
    i = tab_corr[ni, ]$OBJECTID
    li_neighbors = tab_corr[ni, ]$neighbors[[1]]
    l = length(li_neighbors)
    if ( l > 0 ){
      li_neighbors_degree <- c()
      for (nj in 1:l){
        j = li_neighbors[nj]
        nj_degree <- tab_corr[tab_corr$OBJECTID == j, ]$degree
        li_neighbors_degree <- c(li_neighbors_degree, nj_degree)
      } 
    } else {li_neighbors_degree <- c(0)} # if has no neighbors, set 0
    tab_corr[ni, ]$neighbors_degree <- list(li_neighbors_degree)
    tab_corr[ni, ]$average_neighbors_degree <- mean(li_neighbors_degree)
  }
  
  # 3-3 get neighbor's average degree & 3-4 get compartmentalization
  # this is problematic because there are NAs and 0s here
  tab_corr$neighbors_average_ndegree <- 0
  tab_corr$compartmentalization <- 0
  
  for (ni in 1:nrow(tab_corr)){
    i = tab_corr[ni, ]$OBJECTID
    li_neighbors = tab_corr[ni, ]$neighbors[[1]]
    
    if (is.na(li_neighbors)){
      l = 0
    } else {
      l = length(li_neighbors)
    }
    
    li_neighbors_average_degree <- c()
    
    if (l > 0) {
      for (nj in 1:l) {
        j = li_neighbors[nj]
        nj_neighbors_average_degree <- tab_corr[tab_corr$OBJECTID == j, ]$average_neighbors_degree
        li_neighbors_average_degree <- c(li_neighbors_average_degree, nj_neighbors_average_degree)
      }
      
      ndegree <- tab_corr[ni, ]$neighbors_degree
      ndegree <- as.numeric(unlist(ndegree))
      tab_corr[ni, ]$neighbors_average_ndegree <- list(li_neighbors_average_degree)
      
      # calculate correlation coefficient here
      corr_coeff <- cor(ndegree, li_neighbors_average_degree)
      if(is.na(corr_coeff)) {corr_coeff = 1}
      tab_corr[ni, ]$compartmentalization = corr_coeff
    }
  }
  

  dfdegree <- tab_corr[c('OBJECTID', 'common_neighbors', 'num_neighbors', 'degree',
                             'clustering_coeff', 'compartmentalization')]
  
  iters[n, ]$degree = mean(dfdegree$degree)
  iters[n, ]$clustering_coeff = mean(dfdegree$clustering_coeff)
  iters[n, ]$compartment = mean(dfdegree$compartmentalization)

  
  #=====2. Raster metrics computable in R=====
  # convert vector to raster
  all_only_pa_to_ras <- dplyr::group_by(all_only_pa) %>% summarize()
  r <- raster()
  extent(r) <- extent(all_only_pa)
  res(r) <- 1000 # I am testing with coarser grain for speed
  ras_filtered_pa <- rasterize(all_only_pa_to_ras, r)
  ras_pa_id <- rasterize(all_only_pa, r)
  # ras_filtered_pa <- rasterize(all_filtered_pa, r, background = 0) # if you want the background, keep them as 0, if not keep as NA
  # but how do I account for edge effects
  # plot(ras_filtered_pa)
  
  # to compute MSPA, this can be saved and indexed
  
  #====metric raster 1: patch cohesion index=====
  cohesion <- lsm_c_cohesion(ras_filtered_pa, directions = 8) # class level
  # how to account for edge effect
  iters[n, ]$cohesion = mean(cohesion$value)
  
  #====metric raster 2: area-weighted patch gyration====
  gyrate <- lsm_p_gyrate(ras_pa_id, directions = 8, cell_center = FALSE) # patch level
  
  # the following code is for area weighted gyration
  area_patch <- lsm_p_area(ras_pa_id)
  
  aw_gyrate <- dplyr::left_join(x = gyrate, y = area_patch, 
                                by = c("layer", "level", "class", "id")) %>%
    dplyr::mutate(value.w = value.x * value.y)
  
  val_aw_gyrate = sum(aw_gyrate$value.w)/sum(aw_gyrate$value.y)
  
  
  gyrate <- gyrate[gyrate$class %in% OBJECTIDs_to_keep, ]
  aw_gyrate <- aw_gyrate[aw_gyrate$class %in% OBJECTIDs_to_keep, ]
  iters[n, ]$gyrate = mean(gyrate$value)
  iters[n, ]$aw_gyrate = val_aw_gyrate
  
  
  
  #=========Step3 Summarize on the dropped patches=========
  # might need to convert to raster
  dropped_pa_to_ras <- dplyr::group_by(dropped_pa) %>% summarize()
  r1 <- raster()
  extent(r1) <- extent(dropped_pa)
  res(r1) <- 1000 # I am testing with coarser grain for speed
  ras_dropped_pa <- rasterize(dropped_pa_to_ras, r1)
  # mean patch area
  mpa <- lsm_p_area(ras_dropped_pa, directions = 8)
  iters[n, ]$mean_patch_area = mean(mpa$value)
  # mean patch perimeter
  mpp <- lsm_p_perim(ras_dropped_pa, directions = 8)
  iters[n, ]$mean_patch_peri = mean(mpp$value)
  # mean patch shape index 
  mpsi <- lsm_p_shape(ras_dropped_pa, directions = 8)
  iters[n, ]$mean_patch_shape = mean(mpsi$value)
  # total edge
  te <- lsm_c_te(ras_dropped_pa, directions = 8, count_boundary = FALSE)
  iters[n, ]$total_edge = mean(te$value)
  # edge density
  ed <- lsm_c_ed(ras_dropped_pa, directions = 8, count_boundary = FALSE)
  iters[n, ]$edge_density = mean(ed$value)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

iterspath <- paste0(getwd(), '\\results\\100iterations_Feb22.csv')
#iterspath <- paste0(getwd(), '\\results\\ca_Feb22.csv')
write.csv(iters, iterspath, row.names = FALSE)
