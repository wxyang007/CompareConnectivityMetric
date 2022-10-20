library(readxl)
library(ggridges)
library(ggplot2)
library(corrplot)

setwd('~/')
# wd <- file.path(getwd(), 'Desktop', 'AProtconn','data','CA')

# setwd(wd)

# on desktop
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA'
setwd(wd)

path_ca <- paste0(wd, '\\results\\100iterations_Oct16_final.csv')
ca_df <- read.table('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results/100iterations_Oct16_final.csv', sep = ',')

path_all_ca <- paste0(wd, '\\results\\ca.csv')
all_ca_df <- read.table(path_all_ca, sep = ',')
colnames(all_ca_df) <- all_ca_df[1,]
all_ca_df <- all_ca_df[-1,]
orig_cols <- colnames(ca_df)

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

# ====== compute change value ======
ca_df$d_prot <- prot_ca - ca_df$prot
ca_df$d_nnd <- nnd_ca - ca_df$nn_d
ca_df$d_areabuff <- area_buff_ca - ca_df$area_buff
ca_df$d_prox <- prox_ca - ca_df$prox
ca_df$d_eca <- eca_ca - ca_df$eca
ca_df$d_flux <- flux_ca - ca_df$flux
ca_df$d_awf <- awf_ca - ca_df$awf
ca_df$d_pc <- pc_ca - ca_df$pc
ca_df$d_protconn <- protconn_ca - ca_df$protconn
ca_df$d_iic <- iic_ca - ca_df$iic
ca_df$d_bc <- bc_ca - ca_df$bc
ca_df$d_degree <- degree_ca - ca_df$degree
ca_df$d_clus <- clus_ca - ca_df$clustering_coeff
ca_df$d_comp <- comp_ca - ca_df$compartment
ca_df$d_coh <- coh_ca - ca_df$cohesion
ca_df$d_gyrate <- gyrate_ca - ca_df$gyrate

# ======= compute all percent change values ======
# the percentage of decrease in connectivity
ca_df$p_prot <- 100*(prot_ca - ca_df$prot)/prot_ca
ca_df$p_nnd <- 100*(nnd_ca - ca_df$nn_d)/nnd_ca
ca_df$p_areabuff <- 100*(area_buff_ca - ca_df$area_buff)/area_buff_ca
ca_df$p_prox <- 100*(prox_ca - ca_df$prox)/prox_ca
ca_df$p_eca <- 100*(eca_ca - ca_df$eca)/eca_ca
ca_df$p_flux <- 100*(flux_ca - ca_df$flux)/flux_ca
ca_df$p_awf <- 100*(awf_ca - ca_df$awf)/awf_ca
ca_df$p_pc <- 100*(pc_ca - ca_df$pc)/pc_ca
ca_df$p_protconn <- 100*(protconn_ca - ca_df$protconn)/protconn_ca
ca_df$p_iic <- 100*(iic_ca - ca_df$iic)/iic_ca
ca_df$p_bc <- 100*(bc_ca - ca_df$bc)/bc_ca
ca_df$p_degree <- 100*(degree_ca - ca_df$degree)/degree_ca
ca_df$p_clus <- 100*(clus_ca - ca_df$clustering_coeff)/clus_ca
ca_df$p_comp <- 100*(comp_ca - ca_df$compartment)/comp_ca
ca_df$p_coh <- 100*(coh_ca - ca_df$cohesion)/coh_ca
ca_df$p_gyrate <- 100*(gyrate_ca - ca_df$gyrate)/gyrate_ca

pchange_cols <- c('p_prot', 'p_nnd', 'p_areabuff', 'p_prox', 'p_eca', 'p_flux', 'p_awf',
                'p_pc', 'p_protconn', 'p_iic', 'p_bc', 'p_degree', 'p_clus', 'p_comp',
                'p_comp', 'p_coh', 'p_gyrate')

# ====== prepare for viz ======
curr_cols <- c("num_iter", "objectids_to_keep", "prot",
               "nn_d", "area_buff", "prox", "eca", "flux",
               "awf", "pc", "protconn", 
               "protconn_prot", "protconn_unprot", "protconn_trans", 
               "protconn_within", "protconn_contig",
               "iic", "bc", 
               "degree", "clustering_coeff", "compartment","cohesion", "gyrate",
               "mean_patch_area", "mean_patch_peri", "mean_patch_shape", "total_edge",
               "edge_density"
)

metric_cols <- c("prot", "nn_d", "area_buff", "prox", "eca", "flux",
                 "awf", "pc", "protconn", "iic", "bc", 
                 "degree", "clustering_coeff", "compartment","cohesion", "gyrate")
# norm_cols <- lapply(metric_cols, function(x) paste0('norm_', x)) # --> list
norm_cols <- paste0("norm_", unlist(metric_cols)) # --> character

full_metric_cols <- c("Prot", "Nearest distance neighbor", "Habitat (area) within buffer",
                      "Proximity index", "Equivalent connected area", "Flux",
                      "Area weighted flux", "Probability of connectivity", "ProtConn",
                      "Integral index of connectivity", "Betweenness centrality",
                      "Node degree", "Clustering coefficient", "Compartmentalization",
                      "Patch cohesion index", "Area weighted mean patch gyration")


scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
for (metric in metric_cols) {
  rk_metric <- paste0("rk_", metric)
  norm_metric <- paste0("norm_", metric)
  ca_df[, rk_metric] <- rank(ca_df[, metric])
  ca_df[, norm_metric] <- scale01(ca_df[, metric])
}

sapply(ca_df, class)

#metric_cols <- c("prot", "nn_d", "area_buff", "prox", "eca", "flux",
#                 "awf", "pc", "protconn", "iic", "bc", 
#                 "degree", "clustering_coeff", "compartment","cohesion", "gyrate")


df_all_norm_metrics <- data.frame(row.names = c("value", "metric"))

for (i in 1:length(metric_cols)) {
  metric <- metric_cols[i]
  full_metric_name <- full_metric_cols[i]
  norm_metric <- paste0("norm_", metric)
  df_metric <- data.frame(ca_df[, norm_metric])
  colnames(df_metric) <- c("value")
  df_metric$metric <- full_metric_name
  df_all_norm_metrics <- rbind(df_all_norm_metrics, df_metric)
}


# ====== step 1: agreement on highs and lows ======
# ====== 1.1 pearson's correlation ======
# for percent change
df_p = ca_df[, pchange_cols]
sapply(df_p, class)
cor(df_p, method = "pearson")
corrplot(cor(df_p), order = 'hclust', tl.col = 'black', tl.srt = 45)

# for value
df_normv = ca_df[, norm_cols]
corrplot(cor(df_normv), order = 'hclust', tl.col = 'black', tl.srt = 45)


# ====== 1.2 pca ======


# ====== step 2: examine similarities in dsitribution ======
# ====== 2.1.1 overlayed histograms ======
ggplot(df_all_norm_metrics, aes(value, fill = metric)) + geom_density(alpha = 0.2)


# =======2.1.2 ridge plot ======
ggplot(df_all_norm_metrics, aes(x = value, y = metric, fill = metric)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


# ====== 2.2 Kolmogorov-Smirnov test ======
# --> create a matrix out of it
hist_prot <- hist(ca_df$norm_prot)
hist_prot$counts <- cumsum(hist_prot$counts)
plot(hist_prot)

hist_protconn <- hist(ca_df$norm_protconn)
hist_protconn$counts <- cumsum(hist_protconn$counts)

hist_iic <- hist(ca_df$norm_iic)
hist_iic$counts <- cumsum(hist_iic$counts)

hist_areabuff <- hist(ca_df$norm_area_buff)
hist_areabuff$counts <- cumsum(hist_areabuff$counts)

hist_prox <- hist(ca_df$norm_prox)
hist_prox$counts <- cumsum(hist_prox$counts)

hist_cohesion <- hist(ca_df$norm_cohesion)
hist_cohesion$counts <- cumsum(hist_cohesion$counts)

hist_gyrate <- hist(ca_df$norm_gyrate)
hist_gyrate$counts <- cumsum(hist_gyrate$counts)

hist_degree <- hist(ca_df$norm_degree)
hist_degree$counts <- cumsum(hist_degree$counts)

hist_clu_coeff <- hist(ca_df$norm_clustering_coeff)
hist_clu_coeff$counts <- cumsum(hist_clu_coeff$counts)

hist_compart <- hist(ca_df$norm_compartment)
hist_compart$counts <- cumsum(hist_compart$counts)

hist_eca <- hist(ca_df$norm_eca)
hist_eca$counts <- cumsum(hist_eca$counts)

ks.test(hist_iic$counts, hist_prox$counts)
ks.test(hist_prot$counts, hist_protconn$counts)


# ====== step 3: examine associations with network characteristics =====
# ====== 3.1 pearson's correlation? ======
# ====== 3.2 3D plots ======
