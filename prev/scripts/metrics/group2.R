library(sf)
library(Makurhini)

wd <- file.path(getwd(), 'desktop', 'AProtconn','data','CA')
setwd(wd)

CA_PA <- read_sf('ca_pa_file_fixed.shp')
CA_PA_only <- CA_PA[CA_PA$ifPA == 1, ]

test <- CA_PA_only
# metric 1: Incidence Function Measure (IFM)
# needs revision of code

# metric 2: Flux & AWF <- calculated in conefor
# needs revision of code
# check code at lconnect


awf <- con_metric(landscape, metric = 'AWF')
con_metric(test, 'AWF')
k <- log(0.5)/(max_dist/2)
out <- matrix(NA, row = length(area_c), ncol = length(area_c))
for (i in seq(area_c)){
  for (j in seq(area_c)){
    prob <- exp(-k*(as.matrix(distance)[i, j])) * area_c[i] * area_c[j]
    out[i, j] <- prob
  }
}
diag(out) <- 0
result <- c(result, AWF = sum(out))



component_calc <- function(object, distance, min_dist = NULL) {
  if (is.null(min_dist)){
    clusters <- rep(1, length(object))
  } else{
    groups <- stats::hclust(distance, "single")
    clusters <- stats::cutree(groups, h = min_dist)
  }
  area_c <- sf::st_area(object)
  return(list(area_c = area_c, clusters = clusters))
}

# filepath = system.file('C:/Users/wenxinyang/Desktop/ProtConn/data/CA/CA_500km_buffer_PA.shp', package = 'lconnect')
# lc_test <- upload_land(filepath, bound_path = NULL, habitat = 1, max_dist = 500000)
# con_metric(test, 'AWF')
# utils::file_test("-f", filepath)

habitat = 1
bound_path = NULL
max_dist = 5000000
min_dist = NULL
landscape <- sf::st_read('./data/CA/CA_500km_buffer_PA.shp', quiet = T)
landscape <- head(landscape, 10)[2]
# df[, -1] <- apply(df[, -1], 2, scale)
# landscape <- landscape[landscape[[1]] == habitat, ]
landscape <- sf::st_union(landscape)
if (is.null(bound_path)) {
  boundary <- sf::st_convex_hull(landscape)
} else{
  boundary <- sf::st_read(bound_path, quiet = T)
}
area_l <- sf::st_area(boundary)
landscape <- sf::st_cast(landscape, "POLYGON")
distance <- st_distance(landscape)
distance <- stats::as.dist(distance)
aux <- component_calc(landscape, distance, max_dist)
landscape <- suppressWarnings(sf::st_sf(clusters = aux$clusters,
                                        geometry = landscape))
object <- list(landscape = landscape, max_dist = max_dist,
               clusters = aux$clusters, distance = distance,
               boundary = boundary, area_l = area_l)
class(object) <- "lconnect"

# lc_test <- upload_land(filepath, bound_path = NULL, habitat = 1, max_dist = 500000)
con_metric(object, 'AWF')
patch_imp(object, 'AWF', vector_out = FALSE) # wtf the argument 'metric' must be 'IIC'

# metric 3: Probability of Connectivity (PC)
PC <- MK_dPCIIC(nodes = test, attribute = NULL,
                distance = list(type = "edge"),
                metric = "PC", probability = 0.05,
                distance_thresholds = 10000)
class(PC)
dfpc <- as.data.frame(PC)
dfpc$geometry <- NULL
dfpc <- dfpc[c('OBJECTID', 'STATEFP', 'ORIG_FID', 'nodeID', 'dPC', 'dPCintra', 'dPCflux', 'dPCconnector', 'IIC')]
write.csv2(dfpc, 'pc.csv')


# metric 4: Equivalent Connected Area (ECA)
ECA <- MK_dECA(nodes = list(test, list), attribute = NULL, area_unit = "km2",
                 distance = list(type = 'edge', resistance = NULL), metric = 'PC',
                 probability = 0.05, distance_thresholds = 10000, LA = 10000000,
               plot = TRUE, intern = TRUE)

ECA <- MK_dECA(
  nodes = list(test, test),
  attribute = NULL,
  area_unit = "km2",
  distance = list(type = "edge", resistance = NULL),
  metric = "PC",
  probability = 0.05,
  distance_thresholds = 10000,
  LA = 10000000,
  plot = TRUE,
  intern = TRUE
)
data("list_forest_patches", package = "Makurhini")
class(list_forest_patches)
class(test)
# metric 6: Change in Equivalent Connectivity (dEC)
# should be something similar to ECA?

# metric 7: ProtConn Indicator
ProtConn <- MK_ProtConn(nodes = test, region = test,
                    area_unit = "m",
                    distance = list(type= "edge"),
                    distance_thresholds = 10000,
                    probability = 0.5, transboundary = 2300000,
                    LA = NULL, plot = TRUE, parallel = NULL,
                    protconn_bound=TRUE,
                    write = NULL, intern = TRUE)

MK_ProtConn(nodes = test, region = test, area_unit = "m",
            distance = list(type = "edge"),
            distance_thresholds = 10000,
            probability = 0.5) # there is one doubt about protconn: does it need to be divided by a region
# in other words, is it a class level metric or a patch level