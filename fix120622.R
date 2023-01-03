library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(landscapemetrics)
#====metric raster: gyration & area-weighted gyration=====
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA'
setwd(wd)

shp <- read_sf('ca_pa_file_fixed.shp')
# on desktop
# shp <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/ca_pa_file_fixed.shp')
# summary(shp$OBJECTID)

ca_bound <- read_sf('ca-state-boundary/CA_State_TIGER2016.shp')

non_ca_pa <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/non_ca_pa.shp')
non_ca_pa$ifTarget = 0

# filter out only the pas in CA, these are the patches to iterate with
ca_pa <- shp[which((shp$ifPA == 1) & (shp$STATEFP == '06')), ]

coneforfolder = 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/conefor_results_2'
n_iter = 100

root_folder <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results'
setwd(root_folder)
iters_result_path <- paste0(root_folder, '/100iterations_Oct16_final.csv')
iters_result <- read.csv(iters_result_path, sep = ",")
iters_result$area_buff_1 <- 0

iters_result$aw_gyrate <- 0

for (n in 1:n_iter){
  print(n)
  # randomly drop 10% of the nodes
  OBJECTIDs_to_drop = sample(min(ca_pa$OBJECTID):max(ca_pa$OBJECTID), round(0.1*0), replace=F)
  
  # PAs to keep
  filtered_pa <- ca_pa[!ca_pa$OBJECTID %in% OBJECTIDs_to_drop, ]
  filtered_pa$ifTarget = 1
  OBJECTIDs_to_keep = unique(filtered_pa$OBJECTID)
  # OBJECTIDs_to_keep = as.list(as.numeric(strsplit(iters_result[n,]$objectids_to_keep, ",")[[1]]))
  filtered_pa <- ca_pa[ca_pa$OBJECTID %in% OBJECTIDs_to_keep, ]
  #filtered_pa <- ca_pa
  filtered_pa$ifTarget = 1
  
  # PAs to keep and non-PAs and transboundary PAs
  keptpa_and_nonpa <- rbind(filtered_pa, non_ca_pa)
  
  # PAs to keep and transboundary PAs
  all_only_pa <- keptpa_and_nonpa[keptpa_and_nonpa$ifPA == 1, ]
  
  filtered_pa$int_area <- 0
  
  #=====2. Raster metrics computable in R=====
  # convert vector to raster
  # all_only_pa_to_ras <- dplyr::group_by(all_only_pa) %>% summarize()
  r <- raster()
  extent(r) <- extent(all_only_pa)
  res(r) <- 1000 # I am testing with coarser grain for speed
  # ras_filtered_pa <- rasterize(all_only_pa_to_ras, r)
  ras_pa_id <- rasterize(all_only_pa, r)
  
  #====metric raster 2: area-weighted patch gyration====
  gyrate <- lsm_p_gyrate(ras_pa_id, directions = 8, cell_center = FALSE) # patch level
  
  # the following code is for area weighted gyration
  area_patch <- lsm_p_area(ras_pa_id)
  
  aw_gyrate <- dplyr::left_join(x = gyrate, y = area_patch, 
                                by = c("layer", "level", "class", "id")) %>%
    dplyr::mutate(value.w = value.x * value.y)
  
  gyrate <- gyrate[gyrate$class %in% OBJECTIDs_to_keep, ]
  val_gyrate = mean(gyrate$value)
  
  df_aw <- aw_gyrate[aw_gyrate$class %in% OBJECTIDs_to_keep, ]
  val_aw_gyrate = sum(df_aw$value.w)/sum(df_aw$value.y)
  
  iters_result[n, ]$gyrate = val_gyrate
  iters_result[n, ]$aw_gyrate = val_aw_gyrate
  
  print(n)
  print(val_gyrate)
  print(val_aw_gyrate)
  
}

write.table(iters_result, paste0(root_folder, '/100iterations_Dec06_rev.csv'), sep = ',', row.names = FALSE)
