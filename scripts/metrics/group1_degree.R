library(dplyr)
library(sf)

wd <- file.path(getwd(), 'desktop', 'AProtconn','data','CA')
setwd(wd)

ca <- read.csv('near_tab_CA.csv')
ca <- ca[ca$NEAR_DIST <= 10000, ]
ca$OBJECTID <- NULL
ca$NEAR_RANK <- NULL
ca$NEAR_DIST <- NULL

# ca_neartab <- ca[!duplicated(ca),]
r1 <- ca %>%
  group_by(IN_FID) %>%
  summarise(neighbors = list(NEAR_FID))


CA_PA <- read_sf('ca_pa_file_fixed.shp')

capa_neartab <- merge(CA_PA, r1, by.x = 'OBJECTID', by.y = 'IN_FID')
capa_neartab <- capa_neartab[capa_neartab$ifPA == 1, ]

id_pa <- capa_neartab$OBJECTID
id_pa <- id_pa[!duplicated(id_pa)]
li_id_pa <- list(id_pa)[[1]]

capa_neartab <- capa_neartab[c('OBJECTID', 'neighbors')]
capa_neartab$geometry <- NULL
capa_neartab$common_neighbors <- 0
capa_neartab$num_neighbors <- 0
capa_neartab$pairs_neighbors <- 0

for (ni in 1:nrow(capa_neartab)){
  i = capa_neartab[ni, ]$OBJECTID
  li_ni = capa_neartab[ni, ]$neighbors[[1]]
  li_ni_pa = Reduce(intersect, list(li_ni, li_id_pa))
  capa_neartab[ni, ]$neighbors = list(li_ni_pa)
}

for (ni in 1:nrow(capa_neartab)){
  # ni = 1
  i = capa_neartab[ni, ]$OBJECTID
  li_ni = capa_neartab[ni, ]$neighbors[[1]]
  l = length(li_ni)
  li_cni = list()
  m = 0
  n = 0
  if (length(li_ni) > 1){
    for (nj in 1:l){
      # print(paste0('nj: ', nj))
      # nj = 2
      ifn = 0
      j = li_ni[nj]
      nj1 = nj+1
      # print(paste0('j: ', j))
      if (j != i) {
        li_nj = capa_neartab[capa_neartab$OBJECTID == j, ]$neighbors[[1]]
        if (nj < l) {
          for (nk in nj1:l){
            # nk = 4
            k = li_ni[nk]
            # print(paste0('k: ', k))
            if (k != j & k %in% li_nj) {
              m = m + 1 
              # print(paste0(k, ' and ', j, ' are common neighbors'))
              li_cni <- append(li_cni, k)
              li_cni <- append(li_cni, j)
              } else {m = m + 0}
          }
          li_cni <- li_cni[!duplicated(li_cni)]
          n = length(li_cni)
          # print(paste0('n: ', n))
          }
      }
      }
    }
    capa_neartab[ni, 5] = m # pairs of neighbors
    capa_neartab[ni, 4] = l # num of neighbors
    capa_neartab[ni, 3] = n # common neighbors
    print(paste0(ni, ' pairs of neighbors: ', m, '. common neighbors: ', n))
}

capa_neartab$test = (capa_neartab$common_neighbors*capa_neartab$common_neighbors - capa_neartab$pairs_neighbors) >= 0
capa_neartab[capa_neartab$test == FALSE,]
capa_neartab[capa_neartab$common_neighbors > capa_neartab$num_neighbors, ]
# metric 1: degree of connectedness
capa_neartab$degree <- capa_neartab$num_neighbors/length(id_pa)
# metric 2: clustering coefficient
capa_neartab$clustering_coeff <- capa_neartab$common_neighbors/capa_neartab$num_neighbors

# metric 3: compartmentalization
# 3-1 get neighbors' node degrees
capa_neartab$neighbors_degree <- 0
capa_neartab$average_neighbors_degree <- 0
for (ni in 1:nrow(capa_neartab)){
  i = capa_neartab[ni, ]$OBJECTID
  li_neighbors = capa_neartab[ni, ]$neighbors[[1]]
  l = length(li_neighbors)
  if ( l > 0 ){
    li_neighbors_degree <- c()
    for (nj in 1:l){
      j = li_neighbors[nj]
      nj_degree <- capa_neartab[capa_neartab$OBJECTID == j, ]$degree
      li_neighbors_degree <- c(li_neighbors_degree, nj_degree)
    } 
  } else {li_neighbors_degree <- c(0)} # if has no neighbors, set 0
  capa_neartab[ni, ]$neighbors_degree <- list(li_neighbors_degree)
  capa_neartab[ni, ]$average_neighbors_degree <- mean(li_neighbors_degree)
}

# 3-3 get neighbor's average degree & 3-4 get compartmentalization
capa_neartab$neighbors_average_ndegree <- 0
capa_neartab$compartmentalization <- 0

for (ni in 1:nrow(capa_neartab)){
  i = capa_neartab[ni, ]$OBJECTID
  li_neighbors = capa_neartab[ni, ]$neighbors[[1]]
  l = length(li_neighbors)
  li_neighbors_average_degree <- c()
  if ( l > 0 ) {
    for (nj in 1:l){
      j = li_neighbors[nj]
      nj_neighbors_average_degree <- capa_neartab[capa_neartab$OBJECTID == j, ]$average_neighbors_degree
      li_neighbors_average_degree <- c(li_neighbors_average_degree, nj_neighbors_average_degree)
    }
    ndegree <- capa_neartab[ni, ]$neighbors_degree
    ndegree <- as.numeric(unlist(ndegree))
    capa_neartab[ni, ]$neighbors_average_ndegree <- list(li_neighbors_average_degree)
    # and calculate correlation coefficient here
    corr_coeff <- cor(ndegree, li_neighbors_average_degree)
    if (is.na(corr_coeff)) {corr_coeff = 1}
    capa_neartab[ni, ]$compartmentalization = corr_coeff
  }
}
dfdegree <- capa_neartab[c('OBJECTID', 'common_neighbors', 'num_neighbors', 'degree',
                           'clustering_coeff', 'compartmentalization')]
write.csv(dfdegree, 'dfdegree_0727.csv')
