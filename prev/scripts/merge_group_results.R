library(sf)

wd <- file.path(getwd(), 'desktop', 'AProtconn','data','CA')
setwd(wd)

# group2 results
dfconefor <- read.csv('result/6_test_WITHOUT_TRANS_node_importances.csv')
li_group2 <- c('Node', 'dIIC', 'dF', 'dAWF', 'dPC')
dfgroup2 <- dfconefor[li_group2]

ca <- read_sf('ca_pa_file_fixed.shp')
ca_pa <- ca[ca$ifPA == 1, ]
li_keep <- c('OBJECTID', 'ifPA', 'ORIG_FID')
ca_pa_keep <- ca_pa[li_keep]

dfgroup2result <- merge(dfgroup2, ca_pa_keep, by.x = 'Node', by.y = 'ORIG_FID')

dfgroup2result$dEC <- 100*(1 - sqrt(1-dfgroup2result$dPC/100))
dfgroup2result$Node <- NULL

min(dfgroup2result$dPC)

# group1 results
dfgroup1 <- read.csv('result/dfgroup1_0802.csv')
dfgroup1$X <- NULL

dfdegree <- read.csv('result/dfdegree_0727.csv')
dfdegree$X <- NULL
dfdegree$common_neighbors <- NULL
dfdegree$num_neighbors <- NULL

dfgyrate <- read.csv('result/area_weighted_gyrate_0809.csv')
dfgyrate$gyrate <- dfgyrate$value.am
dfgyrate$OBJECTID <- dfgyrate$class
dfgyrate <- dfgyrate[c('OBJECTID', 'gyrate')]

dfcohesion <- read.csv('result/cohesion_0727.csv')
dfcohesion$cohesion <- dfcohesion$value
dfcohesion$OBJECTID <- dfcohesion$class
dfcohesion <- dfcohesion[c('OBJECTID', 'cohesion')]

dfgroup1result <- merge(dfgroup1, dfdegree, by = 'OBJECTID')
dfgroup1result <- merge(dfgroup1result, dfcohesion, by = 'OBJECTID')
dfgroup1result <- merge(dfgroup1result, dfgyrate, by = 'OBJECTID')

# merge
dfall <- merge(dfgroup1result, dfgroup2result, by = 'OBJECTID')
dfall$geometry <- NULL
dfall$ifPA <- NULL
write.csv(dfall, "result/group12results_0809.csv")
# metric 1: IFM


# analysis 1:
library(corrplot)
dfall <- read.csv("result/group12results_")
dfcor <- dfall
dfcor$OBJECTID <- NULL
corrplot(cor(dfcor), method = 'color')


# analysis 2: pca
