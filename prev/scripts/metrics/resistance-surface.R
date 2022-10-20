library(raster)
path_ras2 <- '/Users/wenxinyang/Desktop/AProtConn/data/CA/raster/res2.tif'

ras2 <- raster(path_ras2)
ras2@data@min
ras2@data@max

ras2.1 <- ras2
ras2.1[ras2.1 == 0] <- 0.0001
ras2.1@data@min

path_dem <- '/Users/wenxinyang/Desktop/AProtConn/data/CA/raster/slope-100'
dem <- raster(path_dem)

ras3.1 <- ras2.1^(1 - dem/dem@data@max)
writeRaster(ras3.1, '/Users/wenxinyang/Desktop/AProtConn/data/CA/raster/resis3-1.tif')
