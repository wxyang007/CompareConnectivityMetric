library(readxl)

rootfolder <- "/Users/wenxinyang/Desktop/ProtConn"
datafolder <- file.path(rootfolder, 'data')
filepath <- file.path(datafolder, 'Results_protconn.xlsx')

# we are trying to calculate:
# % of protected area network that is connected: protected and connected land / total protected land
# = sum(protconn * land) / sum(prot * land)
# % of land that is protected and connected: protected and connected land / total land
# = sum(protconn * land) / sum(land)

getVal <- function(name, n) {
  df <- read_excel(filepath, sheet = name)
  df <- df[n[[1]]:n[[2]],]
  
  df$ProtConn <- as.numeric(df$ProtConn)
  df$areakm2 <- as.numeric(df$areakm2)
  df$Prot <- as.numeric(df$Prot)

  v1 <- sum(df$ProtConn/100 * df$areakm2) / sum(df$Prot/100 * df$areakm2)
  v2 <- sum(df$ProtConn/100 * df$areakm2) / sum(df$areakm2)
  v1 <- format(round(v1, 3), nsmall = 3)
  v2 <- format(round(v2, 3), nsmall = 3)
  
  print(paste("For", name,":", sep = ' '))
  print(paste("% of protected area network that is connected:", v1, sep = ' '))
  print(paste("% of land that is protected and connected:", v2, sep = ' '))
}

# CONUS
getVal('CONUS', c(1, 1)) # same
getVal('CONUS', c(2, 2)) # same
getVal('CONUS', c(3, 3)) # same

# DOI: for all US
getVal('DOI 1KM', c(1, 12)) # same
getVal('DOI 10KM', c(1, 12)) # same
getVal('DOI 100KM', c(1, 12)) # same

# DOI: for CONUS
getVal('DOI 1KM', c(1, 10)) # not calculated in the spreadsheet
getVal('DOI 10KM', c(1, 10)) # same
getVal('DOI 100KM', c(1, 10)) # same

# STATE: for all US
getVal('STATE 1KM', c(1, 56))
getVal('STATE 10KM', c(1, 56))
getVal('STATE 100KM', c(1, 56)) # same

# STATE: for CONUS
getVal('STATE 1KM', c(2, 51))
getVal('STATE 10KM', c(2, 51))
getVal('STATE 100KM', c(2, 51)) # same
