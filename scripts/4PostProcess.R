# Create figures -- corrplot
# Author: Wenxin Yang
# Date: Sept, 2022
# Revised: July, 2023


library(readxl)
library(reshape2)
library(ggridges)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(tidyverse)
library(factoextra)
library(FactoMineR)

setwd('~/')
# adjust for study region
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/Colombia'
setwd(wd)

#path_ca <- paste0(wd, '\\results\\100iterations_Jul18_final.csv') # CA
#path_all_ca <- paste0(wd, '\\results\\ca_Jul18.csv') # CA

path_ca <- paste0(wd, '\\results\\Colombia_100iterations_Jul19_final.csv') # Colombia
path_all_ca <- paste0(wd, '\\results\\Colombia_results_Jul18_final.csv') # Colombia

#path_ca <- paste0(wd, '\\results\\Liberia_100iterations_Jul18_final.csv') # Liberia
#path_all_ca <- paste0(wd, '\\results\\Liberia_results_Jul18_final.csv') # Liberia

all_ca_df <- read.table(path_all_ca, sep = ',')
colnames(all_ca_df) <- all_ca_df[1,]
all_ca_df <- all_ca_df[-1,]
orig_cols <- colnames(ca_df)

ca_df <- read.table(path_ca, sep = ',')
colnames(ca_df) <- ca_df[1,]
ca_df <- ca_df[-1, ]

num_cols <- c("num_iter", "prot",
               "nn_d", "area_buff", "prox", "eca", "flux",
               "awf", "pc", "protconn", 
               "iic", "bc", 
               "degree", "clustering_coeff", "compartment","cohesion", "gyrate", "aw_gyrate",
               "mean_patch_area", "mean_patch_peri", "mean_patch_shape", "total_edge",
               "edge_density")
ca_df$num_iter <- as.numeric(ca_df$num_iter)
ca_df$prot <- as.numeric(ca_df$prot)
ca_df$nn_d <- as.numeric(ca_df$nn_d)
ca_df$area_buff <- as.numeric(ca_df$area_buff)
ca_df$prox <- as.numeric(ca_df$prox)
ca_df$eca <- as.numeric(ca_df$eca)
ca_df$flux <- as.numeric(ca_df$flux)
ca_df$awf <- as.numeric(ca_df$awf)
ca_df$pc <- as.numeric(ca_df$pc)
ca_df$protconn <- as.numeric(ca_df$protconn)
ca_df$iic <- as.numeric(ca_df$iic)
ca_df$bc <- as.numeric(ca_df$bc)
ca_df$degree <- as.numeric(ca_df$degree)
ca_df$clustering_coeff <- as.numeric(ca_df$clustering_coeff)
ca_df$compartment <- as.numeric(ca_df$compartment)
ca_df$cohesion <- as.numeric(ca_df$cohesion)
ca_df$gyrate <- as.numeric(ca_df$gyrate)
ca_df$aw_gyrate <- as.numeric(ca_df$aw_gyrate)


# ====== create change value columns ======
prot_ca <- as.numeric(all_ca_df[1,]$prot)
nnd_ca <- as.numeric(all_ca_df[1,]$nn_d)
area_buff_ca <- as.numeric(all_ca_df[1,]$area_buff)
prox_ca <- as.numeric(all_ca_df[1,]$prox)
eca_ca <- as.numeric(all_ca_df[1,]$eca)
flux_ca <- as.numeric(all_ca_df[1,]$flux)
awf_ca <- as.numeric(all_ca_df[1,]$awf)
pc_ca <- as.numeric(all_ca_df[1,]$pc)
protconn_ca <- as.numeric(all_ca_df[1,]$protconn)
iic_ca <- as.numeric(all_ca_df[1,]$iic)
bc_ca <- as.numeric(all_ca_df[1,]$bc)
degree_ca <- as.numeric(all_ca_df[1,]$degree)
clus_ca <- as.numeric(all_ca_df[1,]$clustering_coeff)
comp_ca <- as.numeric(all_ca_df[1,]$compartment)
coh_ca <- as.numeric(all_ca_df[1,]$cohesion)
gyrate_ca <- as.numeric(all_ca_df[1,]$gyrate)
aw_gyrate_ca <- as.numeric(all_ca_df[1,]$aw_gyrate)

# ====== compute change value ======
ca_df$d_prot <- prot_ca - ca_df$prot
ca_df$d_nn_d <- nnd_ca - ca_df$nn_d
ca_df$d_area_buff <- area_buff_ca - ca_df$area_buff
ca_df$d_prox <- prox_ca - ca_df$prox
ca_df$d_eca <- eca_ca - ca_df$eca
ca_df$d_flux <- flux_ca - ca_df$flux
ca_df$d_awf <- awf_ca - ca_df$awf
ca_df$d_pc <- pc_ca - ca_df$pc
ca_df$d_protconn <- protconn_ca - ca_df$protconn
ca_df$d_iic <- iic_ca - ca_df$iic
ca_df$d_bc <- bc_ca - ca_df$bc
ca_df$d_degree <- degree_ca - ca_df$degree
ca_df$d_clustering_coeff <- clus_ca - ca_df$clustering_coeff
ca_df$d_compartment <- comp_ca - ca_df$compartment
ca_df$d_cohesion <- coh_ca - ca_df$cohesion
ca_df$d_gyrate <- gyrate_ca - ca_df$gyrate
ca_df$d_aw_gyrate <- aw_gyrate_ca - ca_df$aw_gyrate

# ======= compute all percent change values ======
# the percentage of decrease in connectivity
ca_df$p_prot <- 100*(prot_ca - ca_df$prot)/ca_df$prot
ca_df$p_nn_d <- 100*(nnd_ca - ca_df$nn_d)/ca_df$nn_d
ca_df$p_area_buff <- 100*(area_buff_ca - ca_df$area_buff)/ca_df$area_buff
ca_df$p_prox <- 100*(prox_ca - ca_df$prox)/ca_df$prox
ca_df$p_eca <- 100*(eca_ca - ca_df$eca)/ca_df$eca
ca_df$p_flux <- 100*(flux_ca - ca_df$flux)/ca_df$flux
ca_df$p_awf <- 100*(awf_ca - ca_df$awf)/ca_df$awf
ca_df$p_pc <- 100*(pc_ca - ca_df$pc)/ca_df$pc
ca_df$p_protconn <- 100*(protconn_ca - ca_df$protconn)/ca_df$protconn
ca_df$p_iic <- 100*(iic_ca - ca_df$iic)/ca_df$iic
ca_df$p_bc <- 100*(bc_ca - ca_df$bc)/ca_df$bc
ca_df$p_degree <- 100*(degree_ca - ca_df$degree)/ca_df$degree
ca_df$p_clustering_coeff <- 100*(clus_ca - ca_df$clustering_coeff)/ca_df$clustering_coeff
ca_df$p_compartment <- 100*(comp_ca - ca_df$compartment)/ca_df$compartment
ca_df$p_cohesion <- 100*(coh_ca - ca_df$cohesion)/ca_df$cohesion
ca_df$p_gyrate <- 100*(gyrate_ca - ca_df$gyrate)/ca_df$gyrate
ca_df$p_aw_gyrate <- 100*(aw_gyrate_ca - ca_df$aw_gyrate)/ca_df$aw_gyrate

pchange_cols <- c('p_prot', 'p_nn_d', 'p_area_buff', 'p_prox', 'p_eca', 'p_flux', 'p_awf',
                'p_pc', 'p_protconn', 'p_iic', 'p_bc', 'p_degree', 'p_clustering_coeff', 
                'p_compartment','p_cohesion', 'p_gyrate', 'p_aw_gyrate')

# ====== prepare for viz ======
curr_cols <- c("num_iter", "objectids_to_keep", "prot",
               "nn_d", "area_buff", "prox", "eca", "flux",
               "awf", "pc", "protconn", 
               "protconn_prot", "protconn_unprot", "protconn_trans", 
               "protconn_within", "protconn_contig",
               "iic", "bc", 
               "degree", "clustering_coeff", "compartment","cohesion", "gyrate",
               "aw_gyrate",
               "mean_patch_area", "mean_patch_peri", "mean_patch_shape", "total_edge",
               "edge_density"
)

metric_cols <- c("prot", "nn_d", "area_buff", "prox", "eca", "flux",
                 "awf", "pc", "protconn", "iic", "bc", 
                 "degree", "clustering_coeff", "compartment","cohesion", "gyrate",
                 "aw_gyrate")
# norm_cols <- lapply(metric_cols, function(x) paste0('norm_', x)) # --> list
norm_cols <- paste0("norm_", unlist(metric_cols)) # --> character

full_metric_cols <- c("Prot", "Nearest neighbor distance", "Habitat (area) within buffer",
                      "Proximity index", "Equivalent connected area", "Flux",
                      "Area weighted flux", "Probability of connectivity", "ProtConn",
                      "Integral index of connectivity", "Betweenness centrality",
                      "Node degree", "Clustering coefficient", "Compartmentalization",
                      "Patch cohesion index", "Patch gyration",
                      "Area weighted mean patch gyration")
abbr_metric_cols <- c("Prot", "Dist", "BA", "Prox", "ECA", "Flux", "AWF", "PC", "ProtConn",
                      "IIC", "BC", "Degree", "ClusCoeff", "Compart", "Cohesion", "Gyrate",
                      "AWGyrate")


scale01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (metric in metric_cols) {
  rk_metric <- paste0("rk_", metric)
  norm_metric <- paste0("norm_", metric)
  ca_df[, rk_metric] <- rank(ca_df[, metric])
  ca_df[, norm_metric] <- scale01(ca_df[, metric])
  
  p_metric <- paste0("p_", metric)
  rk_p_metric <- paste0("rk_p_", metric)
  norm_p_metric <- paste0("norm_p_", metric)
  ca_df[, rk_p_metric] <- rank(ca_df[, p_metric])
  ca_df[, norm_p_metric] <- scale01(ca_df[, p_metric])
}


#metric_cols <- c("prot", "nn_d", "area_buff", "prox", "eca", "flux",
#                 "awf", "pc", "protconn", "iic", "bc", 
#                 "degree", "clustering_coeff", "compartment","cohesion", "gyrate")


df_all_norm_metrics <- data.frame(row.names = c("value", "metric"))
df_all_metrics <- data.frame(row.names =c("value", "metric"))

df_all_norm_percentchange <- data.frame(row.names = c("value", "metric"))
df_all_percentchange <- data.frame(row.names = c("value", "metric"))

for (i in 1:length(metric_cols)) {
  metric <- metric_cols[i]
  #full_metric_name <- full_metric_cols[i]
  full_metric_name <- abbr_metric_cols[i]
  
  df_metric <- data.frame(ca_df[, metric])
  colnames(df_metric) <- c("value")
  df_metric$metric <- full_metric_name
  df_all_metrics <- rbind(df_all_metrics, df_metric)
  
  norm_metric <- paste0("norm_", metric)
  df_norm_metric <- data.frame(ca_df[, norm_metric])
  colnames(df_norm_metric) <- c("value")
  df_norm_metric$metric <- full_metric_name
  df_all_norm_metrics <- rbind(df_all_norm_metrics, df_norm_metric)
  
  
  p_metric = paste0("p_", metric)
  norm_p_metric = paste0("norm_p_", metric)
  df_p_norm_metric <- data.frame(ca_df[, norm_p_metric])
  df_p_metric <- data.frame(ca_df[, p_metric])
  colnames(df_p_norm_metric) <- c("value")
  colnames(df_p_metric) <- c("value")
  df_p_norm_metric$metric <- full_metric_name
  df_p_metric$metric <- full_metric_name
  df_all_norm_percentchange <- rbind(df_all_norm_percentchange, df_p_norm_metric)
  df_all_percentchange <- rbind(df_all_percentchange, df_p_metric)
  
}



# ====== step 1: agreement on highs and lows ======
# ====== 1.1 pearson's correlation ======

new_cols <- c("Proximity index", "Nearest neighbor distance", "Compartmentalization", "Clustering coefficient",
              "Flux", "Node degree", "Patch gyration",
              "Betweenness centrality",
              "Patch cohesion index", "Habitat (area) within buffer","Area weighted mean patch gyration", 
              "Area weighted flux", 
              "Integral index of connectivity", "Probability of connectivity",
              "Equivalent connected area", "ProtConn",  "Prot")
# FPC, hclust, AOE, alphabet

abbr_cols <- c("Prox", "Dist", "Compart", "ClusCoeff", "Flux", "Degree", "Gyrate","BC",
                "Cohesion","BA","AWGyrate","AWF",  "IIC", "PC", "ECA", "ProtConn",  "Prot")

abbr_metric_by_mean <- c("Compart", "Gyrate", "Dist", "Degree", "Cohesion", "ClusCoeff",
                         "AWGyrate", "BA", "Prot", "IIC", "PC", "ECA", "ProtConn", "Flux",
                         "AWF", "Prox", "BC")

# for raw metric values
df_v = ca_df[, metric_cols] # this is for metric values
colnames(df_v) <- full_metric_cols
df_v = df_v[, new_cols]
colnames(df_v) <- abbr_cols
sapply(df_v, class)
# cor(df_v, method = "pearson")
corrplot(cor(df_v), 
         type='lower', 
         order = 'original', 
         #tl.pos='n',            # to remove label names
         tl.col = 'black', tl.srt = 45)



# for percent change
df_p = ca_df[, pchange_cols]
colnames(df_p) <- full_metric_cols
df_p = df_p[, new_cols]
corrplot(cor(df_p), order = 'original', tl.col = 'black', tl.srt = 45)

rk_cols <- c()
rk_p_cols <- c()
for (x in metric_cols){
  rk_cols <- append(rk_cols, paste0("rk_", x))
  rk_p_cols <- append(rk_p_cols, paste0("rk_p_", x))
}

df_r = ca_df[, rk_cols]
colnames(df_r) <- full_metric_cols
df_r <- df_r[, new_cols]
corrplot(cor(df_r), order = 'original', tl.col = 'black', tl.srt = 45)

df_p_r = ca_df[, rk_p_cols]
colnames(df_p_r) <- full_metric_cols
df_p_r <- df_p_r[, new_cols]
corrplot(cor(df_p_r), order = 'original', tl.col = 'black', tl.srt = 45)


# for value
#df_normv = ca_df[, norm_cols]
#corrplot(cor(df_normv), order = 'hclust', tl.col = 'black', tl.srt = 45)


# ====== 1.2 pca ======
# I am performing pca in python


# ====== 1.3 hierarchical clustering on principal components =====
df_v1 <- df_v
df_v1$`Proximity index` <- NULL
dftrans <- t(df_v1)
pca <- PCA(dftrans, ncp = 3, graph = FALSE)
hcpc <- HCPC(pca, graph = FALSE)
fviz_dend(hcpc,
          cex = 0.7,
         palette = 'jco',
          rect = TRUE, rect_fill = TRUE,
          rect_border = 'jco',
          labels_track_height = 0.8
)

d <- dist(t(df_v), method = 'euclidean')
hc1 <- hclust(d, method = 'complete')
plot(hc1, cex = 0.6, hang = -1)
#view(USArrests)
#print(pca)
fviz_dend(hcpc)

fviz_cluster(hcpc, repel = TRUE,
             snow.clust.cent = TRUE,
             palette = 'jco',
             ggtheme = theme_minimal(),
             main = 'Factor map')


summary(df_p)

# ====== step 2: examine similarities in dsitribution ======
# ====== 2.1.1 overlayed histograms ======
li1 <- c('Prot','ProtConn', 'AWF')
dfridge_clus <- df_all_percentchange[df_all_percentchange$metric %in% li1,]
dfridge_clus$metric <- factor(dfridge_clus$metric)
ggplot(dfridge_clus, aes(value, color=metric)) + geom_density(alpha=0.2)+scale_x_continuous(limits = c(-20,100))

df_all_metrics$metric <- factor(df_all_metrics$metric, levels = rev(abbr_metric_by_mean))
df_all_norm_metrics$metric <- factor(df_all_norm_metrics$metric, levels = rev(abbr_metric_by_mean))

df_all_percentchange$metric <- factor(df_all_percentchange$metric, levels = rev(abbr_metric_by_mean))
df_all_norm_percentchange$metric <- factor(df_all_norm_percentchange$metric, levels = rev(abbr_metric_by_mean))


ggplot(df_all_norm_metrics, aes(value, color = metric)) + geom_density(alpha = 0.2)
ggplot(df_all_norm_percentchange, aes(value, color = metric)) + geom_density(alpha = 0.2)

ggplot(df_all_metrics, aes(value, fill = metric)) + geom_density(alpha = 0.2)
ggplot(df_all_percentchange, aes(value, fill = metric)) + geom_density(alpha = 0.2)


# =======2.1.2 ridge plot ======
# Note: this is the first version, not the version used to produce plots in the paper
# df_test <- df_all_norm_metrics[df_all_norm_metrics$metric == 'Proximity index',]
# df_new_all_metrics <- df_all_metrics
# colnames(df_new_all_metrics)
ggplot(df_all_norm_metrics, aes(x = value, y = metric, fill = metric)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


# dfplot1 = df_all_percentchange[metric_by_mean]
dftest <- df_all_percentchange
dftest[sapply(dftest, is.infinite)] <- 0 # 42 BC values are Inf for Liberia

li1 <- c('Prot', 'IIC', 'PC', 'ECA', 'ProtConn', 'AWF')
dftest1 <- dftest[dftest$metric %in% li1,]

li1_1 <- c('Cohesion', 'AWGyrate', 'BA')
dftest1_1 <- dftest[dftest$metric %in% li1_1,]

li2 <- c('Gyrate', 'Dist', 'BA')
         #,'Compart', 'BC')
dftest2 <- dftest[dftest$metric %in% li2,]

li3 <- c('Flux', 'Degree', 'ClusCoeff', 'Cohesion')
dftest3 <- dftest[dftest$metric %in% li3,]

li4 <- c('Compart', 'BC')
dftest4 <- dftest[dftest$metric %in% li4, ]

ggplot(dftest, aes(x = value, y = metric, fill = metric)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.y=element_text(size=20)) +
  scale_x_continuous(limits = c(-30,100)) + # the range to start with for CA full list of metrics is (-70, 200)
  theme(legend.position = "none")

# ====== 2.2 Kolmogorov-Smirnov test ======
# --> create a matrix out of it
df_ksval_pval <- data.frame(matrix(0, nrow = length(metric_cols), ncol = length(metric_cols)))
df_ksnormval_pval <- data.frame(matrix(0, nrow=length(metric_cols), ncol=length(metric_cols)))
df_ksp_pval <- data.frame(matrix(0, nrow = length(metric_cols), ncol = length(metric_cols)))

df_ksval <- data.frame(matrix(0, nrow = length(metric_cols), ncol = length(metric_cols)))
df_ksnormval <- data.frame(matrix(0, nrow = length(metric_cols), ncol = length(metric_cols)))
df_ksp <- data.frame(matrix(0, nrow = length(metric_cols), ncol = length(metric_cols)))

colnames(df_ksval_pval) = metric_cols
colnames(df_ksnormval_pval) = metric_cols
colnames(df_ksp_pval) = metric_cols

colnames(df_ksval) = metric_cols
colnames(df_ksnormval) = metric_cols
colnames(df_ksp) = metric_cols

rownames(df_ksval_pval) = metric_cols
rownames(df_ksnormval_pval) = metric_cols
rownames(df_ksp_pval) = metric_cols

rownames(df_ksval) = metric_cols
rownames(df_ksnormval) = metric_cols
rownames(df_ksp) = metric_cols

for (i in 1:length(metric_cols)) {
  metric <- metric_cols[i]
  full_metric_name <- full_metric_cols[i]
  
  p_metric <- paste0("p_", metric)
  
  norm_metric <- paste0("norm_", metric)
  norm_p_metric <- paste0("norm_p_", metric)
  
  #hist1 <- hist(ca_df[, norm_metric])
  #hist1$counts <- cumsum(hist1$counts)
  
  #histp1 <- hist(ca_df[, norm_p_metric])
  #histp1$counts <- cumsum(histp1$counts)
  
  for (j in 1:length(metric_cols)){
    if(TRUE) {
      metric2 <- metric_cols[j]
      full_metric_name_j <- full_metric_cols[j]
      
      p_metric2 <- paste0("p_", metric2)
      
      norm_metric2 <- paste0("norm_", metric2)
      norm_p_metric2 <- paste0("norm_p_", metric2)
      
      #hist2 <- hist(ca_df[, norm_metric2])
      #hist2$counts <- cumsum(hist2$counts)
      
      #histp2 <- hist(ca_df[, norm_p_metric2])
      #histp2$counts <- cumsum(histp2$counts)
      
      ks_val <- ks.test(ca_df[, metric], ca_df[, metric2])
      ks_normval <- ks.test(ca_df[, norm_metric], ca_df[, norm_metric2])
      ks_p <- ks.test(ca_df[, p_metric], ca_df[, p_metric2])
      
      df_ksval[i, j] <- ks_val$statistic
      df_ksval_pval[i, j] <- ks_val$p.value
      
      df_ksnormval[i, j] <- ks_normval$statistic
      df_ksnormval_pval[i, j] <- ks_normval$p.value
      
      df_ksp[i, j] <- ks_p$statistic
      df_ksp_pval[i, j] <- ks_p$p.value
      
      print(metric)
      print(metric2)
    }
  }
  
}

root_folder <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results'
write.table(df_ksval_pval, paste0(root_folder, '/value_ks_test_pvalues_0224_raw.csv'), sep = ',',
            row.names = FALSE)
write.table(df_ksnormval_pval, paste0(root_folder, '/normvalue_ks_test_pvalues_0224_raw.csv'), sep = ',',
            row.names = FALSE)
write.table(df_ksp_pval, paste0(root_folder, '/p_ks_test_pvalues_0224_raw.csv'), sep = ',',
            row.names = FALSE)


write.table(df_ksval, paste0(root_folder, '/value_ks_test_statistic_0224_raw.csv'), sep = ',',
            row.names = FALSE)
write.table(df_ksnormval, paste0(root_folder, '/normvalue_ks_test_statistic_0224_raw.csv'), sep = ',',
            row.names = FALSE)
write.table(df_ksp, paste0(root_folder, '/p_ks_test_statistic_0224_raw.csv'), sep = ',',
            row.names = FALSE)

reordered <- read.csv(paste0(root_folder, '/reordered_ks_pvalues_1024.csv'), sep = ',')
mat_reordered <- data.matrix(reordered)
colnames(mat_reordered) <- new_cols
rownames(mat_reordered) <- new_cols


colnames(reordered) <- new_cols
rownames(reordered) <- new_cols


longData <- melt(mat_reordered)
ggplot(longData, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient(low = "white", high = "red") +
  # labs(x = "x", y = "y", title = "title") +
  theme_bw() + theme(axis.text.x = element_text(size = 9, angle = 25, vjust = 0.6, hjust = 0.9),
                     axis.text.y = element_text(size = 9),
                     plot.title = element_text(size = 11))
