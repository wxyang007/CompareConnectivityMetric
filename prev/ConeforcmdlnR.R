# create a node file
nodesconefor <- unique(nodedf$OBJECTID)
# create a distance/connection file
connectiondf <- neartab[(neartab$IN_FID %in% nodesconefor) &
                                         (neartab$NEAR_FID %in% nodesconefor), ]

# create a folder to store results of this iteration
thisiter <- paste0(coneforfolder, "\\", n)
dir.create(thisiter)

# copy and paste the executive file for Conefor command line to the root folder
conefor_exe <- paste0(coneforfolder, '/coneforWin64.exe')

setwd(thisiter)
# save the node and distance files to this iteration's folder
nodefile <- paste0(n, '_node.txt')
write.table(nodedf, nodefile, row.names = FALSE, col.names = FALSE, sep = "\t")
confile <- paste0(n, '_distance.txt')
write.table(connectiondf, confile, row.names = FALSE, col.names = FALSE, sep = "\t")

# run command line to compute indices
p1 <- paste(conefor_exe, "-nodeFile", nodefile, "-conFile", confile,
              "-t dist -confProb 10000 0.5 -PC -F -AWF -confAdj 10000 -IIC -BC onlyoverall")

shell(p1, intern = TRUE)

# then there will be result files in the iteration folder
