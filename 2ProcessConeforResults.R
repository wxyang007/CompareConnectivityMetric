root_folder <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results'
setwd(root_folder)


conefor_folder = 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/conefor_results_3'

n_iter = 100

#iters_result_path <- paste0(root_folder, '/Liberia_100iterations_Feb22_raw.csv')
#iters_result_path <- paste0(root_folder, '/Liberia_results.csv')
iters_result_path <- paste0(root_folder, '/100iterations_Feb22.csv')
iters_result <- read.csv(iters_result_path, sep = ",")

for (i in 1:n_iter) {
  iter_folder <- paste0(conefor_folder, '/', as.character(i))
  #iter_folder <- paste0(conefor_folder, '/', 'Liberia')
  conefor_result_path <- paste0(iter_folder, '/overall_indices.txt')
  conefor_result <- read.table(conefor_result_path, sep = "")
  
  iters_result[i, ]$iic <- conefor_result[2, ]$V2
  iters_result[i, ]$flux <- conefor_result[3, ]$V2
  iters_result[i, ]$awf <- conefor_result[4, ]$V2
  iters_result[i, ]$pc <- conefor_result[6, ]$V2
  
}

a = 423965.360911 # km2; CA
#a = 95923.9
# according to the equation: ProtConn = 100 * ECA / AL
# so, ECA = ProtConn * AL / 100
iters_result$eca <- a*iters_result$protconn/100
write.table(iters_result, paste0(root_folder, '/100iterations_Feb22_final.csv'), sep = ',')
#write.table(iters_result, paste0(root_folder, '/Liberia_results_final.csv'), sep = ',')
