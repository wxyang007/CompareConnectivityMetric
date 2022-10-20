library(raster)
ras <- raster('ras_ca_pa.tif')
landscape <- ras
directions <- 8

points <- raster_to_points(landscape)[, 2:4]
landscape <- raster::as.matrix(landscape)

classes <- get_unique_values(landscape, verbose = FALSE)

patches_class <- 17
landscape_labeled <- get_patches(landscape,
                                     class = patches_class,
                                     directions = directions)[[1]]

landscape_labeled <- t(landscape_labeled)
points <- matrix(points[which(!is.na(landscape_labeled)), ],
                 ncol = 3)
points[, 3] <- landscape_labeled[!is.na(landscape_labeled)]
points <- stats::setNames(object = data.frame(points),
                          nm = c("x", "y", "id"))
centroid <- stats::aggregate(points[, c(1, 2)],
                             by = list(id = points[, 3]),
                             FUN = mean)
full_data <- merge(x = points, y = centroid, by = "id",
                   suffixes = c("","_centroid"))
full_data$dist <- sqrt((full_data$x - full_data$x_centroid) ^ 2 +
                         (full_data$y - full_data$y_centroid) ^ 2)

