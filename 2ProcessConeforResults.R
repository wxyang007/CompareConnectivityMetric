root_folder <- '/Users/wenxinyang/Desktop/GitHub/TemporalChangeConn/results'
setwd(root_folder)

conefor_folder <- paste0(root_folder, '/conefor_results')

n_iter = 100

iters_result_path <- paste0(root_folder, '/100iterations_Oct03.csv')
iters_result <- read.csv(iters_result_path, sep = ",")

for (i in 1:n_iter) {
  iter_folder <- paste0(conefor_folder, '/', as.character(i))
  conefor_result_path <- paste0(iter_folder, '/overall_indices.txt')
  conefor_result <- read.table(conefor_result_path, sep = "")
  
  iters_result[i, ]$iic <- conefor_result[2, ]$V2
  iters_result[i, ]$flux <- conefor_result[3, ]$V2
  iters_result[i, ]$awf <- conefor_result[4, ]$V2
  iters_result[i, ]$PC <- conefor_result[6, ]$V2
  
}


iters_result$eca <- iters_result$mean_patch_area*(1036)*iters_result$protconn/100
write.table('...')
