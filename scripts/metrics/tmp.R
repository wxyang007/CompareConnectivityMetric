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
      # nj = 2
      ifn = 0
      j = li_ni[nj]
      nj1 = nj+1
      print(paste0('nj: ', nj, ' j: ', j))
      if (j != i) {
        li_nj = capa_neartab[capa_neartab$OBJECTID == j, ]$neighbors[[1]]
        if (nj < l) {
          for (nk in nj1:l){
            # nk = 4
            k = li_ni[nk]
            print(paste0('k: ', k))
            if (k != j & k %in% li_nj) {
              m = m + 1 
              ifn = 1
              print(paste0(k, ' and ', j, ' are common neighbors'))
            } else {m = m + 0}
          }
          n = n + ifn
          if (n >0) {n = n + 1}
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