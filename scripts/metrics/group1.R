library(sf)
library(sp)
library(raster)
library(spatialEco)
library(Makurhini)
library(landscapemetrics)
library(dplyr)
library(xlsx)

wd <- file.path(getwd(), 'desktop', 'AProtconn','data','CA')
setwd(wd)

# CA_PA <- read_sf('PA_230km_CA_July2021.shp')
CA_PA <- read_sf('ca_pa_file_fixed.shp')
CA_PA_only <- CA_PA[CA_PA$ifPA == 1, ]

test <- CA_PA_only
#test <- head(CA_PA, 10)[,2]

# metric 1: nearest neighbor distance
# remember to fix geometry before running this step
nearest <- st_nearest_feature(test)
dist <- st_distance(test,test[nearest,], by_element=TRUE)
test$dist <- dist
test$ID <- 1:nrow(test)

# metric 2: area of habitat within buffer
buffered <- st_buffer(test, 10000)
intersect <- st_intersection(buffered, test)
intersect$area <- st_area(intersect)
intersect$ID.new <- 1:nrow(intersect)
test$contains <- st_contains(test, intersect)
test$int_area <- 0
for (x in 1:nrow(test)) {
  a <- test[x, ]$contains[1]
  b <- intersect[intersect$ID.new %in% a[[1]], ]
  #sum(b$area)
  test[x,]$int_area <- as.numeric(sum(b$area))
}

# metric 3: proximity index /w 'spatialEco'
test$prox <- proximity.index(test, max.dist = 10000) # need to see R script in the package to examine



# metric 4: IIC
IIC <- MK_dPCIIC(nodes = test, attribute = NULL,
                 distance = list(type = 'edge'),
                 metric = 'IIC', distance_thresholds = 10000)
# can be summarized with different municipal code
test$IIC <- IIC$dIIC

# metric 5: node betweenness centrality
centrality <- MK_RMCentrality(nodes = test,
                              area_unit = 'm',
                              distance = list(type = 'edge'),
                              distance_thresholds = 10000,
                              probability = 0.5,
                              write = NULL)
test$centrality <- centrality$BWC
centrality$degree
class(centrality)
centrality$geometry <- NULL
dfcentrality <- centrality[c('OBJECTID', 'degree', 'BWC', 'cluster')]
write.csv(dfcentrality, 'result/centrality_0802.csv')

# metric 6: patch cohesion index /w 'landscapemetrics'
# convert into raster --> this is done in QGIS
ras <- raster('ras_ca_pa.tif')
cohesion <- lsm_c_cohesion(ras, directions = 8) 
# set grid value by the patch they belong to when converting from vector into raster

# metric 7: correlation length or area-weighted patch gyration
# workflow adapted from their website: landscape distribution statistics
gyrate <- lsm_p_gyrate(ras, directions = 8, cell_center = FALSE)
area_patch <- lsm_p_area(ras)

# calculate weighted mean
area_weighted_gyrate <- dplyr::left_join(x = gyrate, y = area_patch, 
                                     by = c("layer", "level", "class", "id")) %>%
  dplyr::mutate(value.w = value.x * value.y) %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(value.am = sum(value.w) / sum(value.y)) 

area_weighted_gyrate
write.csv(area_weighted_gyrate, 'result/area_weighted_gyrate_0809.csv')

dfgroup1 <- test[c('OBJECTID','dist','int_area','prox','centrality', 'IIC')]
dfgroup1$IIC <- IIC$dIIC
dfgroup1$geometry <- NULL
# write.csv(dfgroup1, 'dfgroup1_0727.csv')
write.xlsx(dfgroup1, 'result/dfgroup1_0802.xlsx')
write.csv(dfgroup1, 'result/dfgroup1_0802.csv')
