getProtConn <- function(pc_pr, pc_contus) {
  v_pr = (pc_pr*pc_pr)*(a_pr*a_pr)/(100*100)
  v_contus = (pc_contus*pc_contus)*(a_contus+a_pr)*(a_contus+a_pr)/(100*100)-v_pr
  pc_contus = 100*sqrt(v_contus)/a_contus
  print(pc_contus)
}

a_pr = 15689.538446
a_contus = 8081887.59259

# d = 1km
getProtConn(1.2, 0.755)

# d = 10km
getProtConn(1.754, 1.240)

# d = 100km
getProtConn(3.58, 3.02)

Prot = 640798.025202/a_contus
print(Prot)
