# Create figures -- ridgeline plots, descriptive stats, spaghetti plots, and PA size histograms
# Author: Wenxin Yang
# Date: April, 2023
# Revised: July, 2023


library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(viridis)
library(moments)
library(plotly)


# ====== Read in data =====
path <- '/Users/wenxinyang/Desktop/GitHub/TemporalChangeConn/Results'
setwd(path)
cols_order <- c('num_iter', 
                'prox', 'nn_d', 'compartment', 'clustering_coeff', 'flux', 
                'degree',
                'gyrate', 'bc', 'cohesion', 'area_buff', 'aw_gyrate', 'awf',
                'iic', 'pc', 'eca', 'protconn', 'prot')

cols_annotmetric <- c('Prox', 'Dist', 'Compart', 'ClusCoeff','Flux',
                      'Degree', 'Gyrate', 'BC', 'Cohesion', 'BA',
                      'AWGyrate', 'AWF', 'IIC', 'PC', 'ECA', 'ProtConn',
                      'Prot')

# California
dfca <- read.csv('California_100iterations_Jul18_final.csv')[cols_order]
dfca0 <- read.csv('ca_Jul18.csv')[cols_order]

# Liberia
dflib <- read.csv('Liberia_100iterations_Jul18_final.csv')[cols_order]
dflib0 <- read.csv('Liberia_results_Jul18_final.csv')[cols_order]

# Colombia
dfcol <- read.csv('Colombia_100iterations_Jul19_final.csv')[cols_order]
dfcol0 <- read.csv('Colombia_results_Jul18_final.csv')[cols_order]


# ====== prepare data for dist comparison - cor ======
dfca_cor <- dfca[-c(1)]
colnames(dfca_cor) <- cols_annotmetric
cor_ca <- data.frame(cor(dfca_cor))
cor_ca_prot <- data.frame(cor_ca$Prot)
cor_ca_dist <- data.frame(cor_ca$Dist)
colnames(cor_ca_prot) <- 'Cor'
colnames(cor_ca_dist) <- 'Cor'
cor_ca_prot$MetricPair <- paste(cols_annotmetric, '_Prot', sep='')
cor_ca_dist$MetricPair <- paste('Dist_', cols_annotmetric, sep='')
cor_ca_prot$Region <- 'California'
cor_ca_dist$Region <- 'California'


dfcol_cor <- dfcol[-c(1)]
colnames(dfcol_cor) <- cols_annotmetric
cor_col <- data.frame(cor(dfcol_cor))
cor_col_prot <- data.frame(cor_col$Prot)
cor_col_dist <- data.frame(cor_col$Dist)
colnames(cor_col_prot) <- 'Cor'
colnames(cor_col_dist) <- 'Cor'
cor_col_prot$MetricPair <- paste(cols_annotmetric, '_Prot', sep='')
cor_col_dist$MetricPair <- paste('Dist_', cols_annotmetric, sep='')
cor_col_prot$Region <- 'Colombia'
cor_col_dist$Region <- 'Colombia'

dflib_cor <- dflib[-c(1)]
colnames(dflib_cor) <- cols_annotmetric
cor_lib <- data.frame(cor(dflib_cor))
cor_lib_prot <- data.frame(cor_lib$Prot)
cor_lib_dist <- data.frame(cor_lib$Dist)
colnames(cor_lib_prot) <- 'Cor'
colnames(cor_lib_dist) <- 'Cor'
cor_lib_prot$MetricPair <- paste(cols_annotmetric, '_Prot', sep='')
cor_lib_dist$MetricPair <- paste('Dist_', cols_annotmetric, sep='')
cor_lib_prot$Region <- 'Liberia'
cor_lib_dist$Region <- 'Liberia'

dfall_cor_prot <- rbind(cor_ca_prot, cor_col_prot, cor_lib_prot)
dfall_cor_prot[is.na(dfall_cor_prot)] <- 0

dfall_cor_dist <- rbind(cor_ca_dist, cor_col_dist, cor_lib_dist)
dfall_cor_dist[is.na(dfall_cor_dist)] <- 0


# ====== compute percent changes ======

for (c in cols_order){
  if(c != 'num_iter'){
    percchangename = paste0('p_', c)
    
    valca = as.numeric(dfca0[1, c])
    vallib = as.numeric(dflib0[1, c])
    valcol = as.numeric(dfcol0[1, c])
    
    dfca[, percchangename] = 100*(valca - dfca[, c])/dfca[, c]
    dflib[, percchangename] = 100*(vallib - dflib[, c])/dflib[, c]
    dfcol[, percchangename] = 100*(valcol - dfcol[, c])/dfcol[, c]
  }
}

cols_metric <- cols_order[-1]
print(cols_metric)


pcols_metric <- paste('p_', cols_metric, sep='')

dfpca <- dfca[pcols_metric]
colnames(dfpca) <- cols_annotmetric

dfplib <- dflib[pcols_metric]
colnames(dfplib) <- cols_annotmetric

dfpcol <- dfcol[pcols_metric]
colnames(dfpcol) <- cols_annotmetric

dfpearsonca <- cor(dfca, method='pearson')
dfpearsoncol <- cor(dfcol, method='pearson')
dfpearsonlib <- cor(dflib, method='pearson')

# ===== produce dataframes for the ridgeline plots =====
dfhistpca <- data.frame(row.names=c('value', 'metric'))
dfhistplib <- data.frame(row.names=c('value', 'metric'))
dfhistpcol <- data.frame(row.names=c('value', 'metric'))


for (i in 1:length(pcols_metric)) {
  metric <- cols_annotmetric[i]
  
  dfcametric <- data.frame(dfpca[, metric])
  colnames(dfcametric) <- c("value")
  dfcametric$metric <- metric
  dfcametric$region <- 'California'
  dfhistpca <- rbind(dfhistpca, dfcametric)
  
  dflibmetric <- data.frame(dfplib[, metric])
  colnames(dflibmetric) <- c("value")
  dflibmetric$metric <- metric
  dfcametric$region <- 'Liberia'
  dfhistplib <- rbind(dfhistplib, dflibmetric)
  
  dfcolmetric <- data.frame(dfpcol[, metric])
  colnames(dfcolmetric) <- c("value")
  dfcolmetric$metric <- metric
  dfcametric$region <- 'Colombia'
  dfhistpcol <- rbind(dfhistpcol, dfcolmetric)
}

ggplot()+
  geom_point(data=dfca, aes(x=prot, y=nn_d))


# ====== ridgeline plots ======
li_pc1 <- c('p_area_buff', 'p_awg_yrate', 'p_awf', 'p_iic',
            'p_pc', 'p_eca', 'p_protconn', 'p_prot')
df1 <- dfhistpca[dfhistpca$metric %in% li_pc1,]

dfhistpca$metric <- factor(dfhistpca$metric, levels = rev(cols_annotmetric))
dfhistplib$metric <- factor(dfhistplib$metric, levels = rev(cols_annotmetric))
dfhistpcol$metric <- factor(dfhistpcol$metric, levels = rev(cols_annotmetric))

ggplot()+
  geom_histogram(data=dfhistpca, aes(x=value), bins=50)+
  #geom_density(data=dfhistpca, aes(y=..density..*10))+
  xlim(-30,100)+
  facet_wrap(~metric)
  

dfhistplib[dfhistplib==Inf] <- 0
dfhistplib1 <- dfhistplib[dfhistplib$metric != 'Compart',]
plt <- ggplot(dfhistpcol, aes(x=value, y=metric, fill=metric))+
  geom_density_ridges(alpha=0.6)+
  theme_ridges()+
  scale_x_continuous(limits = c(-50,100)) + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position = "none")
plt
ggsave('ridgeplot_col_07191.png',
       height=12, width=4,dpi=72,
       #plot=last_plot())
       plot=plt)




# ======= produce summary statistics table ======
sumcols <- c('Metric', 'Min', 'Mean', 'Max', 'Variance','Kurtosis')

getsumtable <- function(df, outfile){
  
  dfsumarea <- data.frame(matrix(ncol=6, nrow=17))
  colnames(dfsumarea) <- sumcols
  
  for (i in 1:length(colnames(df))){
    metricname <- colnames(df)[i]
    array_metric <- df[,i]
    
    dfsumarea[i, ]$Metric <- metricname
    dfsumarea[i, ]$Min <- min(array_metric)
    dfsumarea[i, ]$Mean <- mean(array_metric)
    dfsumarea[i, ]$Max <- max(array_metric)
    dfsumarea[i, ]$Variance <- var(array_metric)
    dfsumarea[i, ]$Kurtosis <- kurtosis(array_metric)
  }
  
  write.csv(dfsumarea, outfile, row.names = FALSE)
}

getsumtable(dfpca, 'sum_ca_Jul18.csv')
getsumtable(dfpcol, 'sum_col_Jul19.csv')
getsumtable(dfplib, 'sum_lib_Jul18.csv')


# ====== test US ProtConn ======
df1km <- read.csv('/Users/wenxinyang/Desktop/AProtConn/results/USProtConnResults1km.csv')
df1km$ProtConn <- as.numeric(df1km$ProtConn)
df1km$Prot <- as.numeric(df1km$Prot)
cor(df1km$ProtConn, df1km$Prot) # 0.83
cor.test(df1km$ProtConn, df1km$Prot, method='pearson') # significant


df10km <- read.csv('/Users/wenxinyang/Desktop/AProtConn/results/USProtConnResults10km.csv')
df10km$ProtConn <- as.numeric(df10km$ProtConn)
df10km$Prot <- as.numeric(df10km$Prot)
cor(df10km$ProtConn, df10km$Prot) # 0.91
cor.test(df10km$ProtConn, df10km$Prot, method='pearson') # significant

df100km <- read.csv('/Users/wenxinyang/Desktop/AProtConn/results/USProtConnResults100km.csv')
df100km$ProtConn <- as.numeric(df100km$ProtConn)
df100km$Prot <- as.numeric(df100km$Prot)
cor(df100km$ProtConn, df100km$Prot) # 0.99
cor.test(df100km$ProtConn, df100km$Prot, method='pearson') # significant


# Mann-Whitney U tests x not doing this time
# cumulative distribution plots x not doing this time
# average absolute difference between the distributions # Yes
# ====== distribution similarities =====
li_levels <- seq(5, 100, by=5)

# cols_annotmetric


perc_below <- function(df, regioncode){
  dfallrelmetric <- data.frame(matrix(ncol=2, nrow=length(li_levels)))
  colnames(dfallrelmetric) <- c('Threshold', 'Region')
  dfallrelmetric$Region <- regioncode
  dfallrelmetric$Threshold <- li_levels
  for (n1 in 1:length(cols_annotmetric)) {
    m1 <- cols_annotmetric[n1]
    for (n2 in 1:length(cols_annotmetric)) {
      if (n1 < n2) {
        m2 <- cols_annotmetric[n2]
        rel_name <- paste0(m1, '_', m2)
        dfallrelmetric[, rel_name] <- -1
        for (nlev in 1:length(li_levels)){
          lev <- as.numeric(li_levels[nlev])
          array_lev <- abs(df[, m1]-df[, m2])
          perc_below_lev <- length(array_lev[array_lev < lev])
          dfallrelmetric[nlev, rel_name] <- perc_below_lev
        }
      }
    }
  }
  return(dfallrelmetric)
}


perc_below_ver2 <- function(df, regioncode){
  df1 <- data.frame(matrix(ncol=4))
  colnames(df1) <- c('Threshold', 'Region', 'MetricPair', 'Value')
  i = 0
  for (n1 in 1:length(cols_annotmetric)) {
    m1 <- cols_annotmetric[n1]
    for (n2 in 1:length(cols_annotmetric)) {
      if (n1 < n2) {
        for (nlev in 1:length(li_levels)){
          m2 <- cols_annotmetric[n2]
          rel_name <- paste0(m1, '_', m2)
          i = i + 1
          lev <- as.numeric(li_levels[nlev])
          df1[i, 'Threshold'] <- lev
          df1[i, 'Region'] <- regioncode
          df1[i, 'MetricPair'] <- rel_name
          array_lev <- abs(df[, m1]-df[, m2])
          perc_below_lev <- length(array_lev[array_lev < lev])
          df1[i, 'Value'] <- perc_below_lev
        }
      }
    }
  }
  return(df1)
}



dfrelmetricpca <- perc_below_ver2(dfpca, 'California')
dfrelmetricpcol <- perc_below_ver2(dfpcol, 'Colombia')
dfrelmetricplib <- perc_below_ver2(dfplib, 'Liberia')
dfrelmetricplib$Value <- 100*dfrelmetricplib$Value/120
dfrelmetric <- rbind(dfrelmetricpca, dfrelmetricpcol, dfrelmetricplib)

cols_sim <- c('AWF_Prot','IIC_Prot','PC_Prot','ECA_Prot','ProtConn_Prot')
dfsim <- dfrelmetric[dfrelmetric$MetricPair %in% cols_sim, ]

ggplot(data=dfsim, aes(x=Threshold, y=Value, color=MetricPair))+
  geom_smooth(se=F, alpha=0.2
              #, show.legend = FALSE
              )+
  facet_wrap(~Region)


col_scheme1 <- c('#c83753', '#37C8AC', '#6ba15e', '#0e3cf1', '#7a48b7','#f8078c','#F1EF0E')


# figure out a threshold --> percent of simulations having a absolute difference
# 5%, 10%, 25% ...
cols_sim1 <- c('AWF_Prot','IIC_Prot','PC_Prot','ECA_Prot','ProtConn_Prot'
               #,'Prox_Prot', 'Dist_Prot', 'Compart_Prot', 'ClusCoeff_Prot',
               #'Degree_Prot', 'Gyrate_Prot', 'BC_Prot', 'Cohesion_Prot'
               ,'BA_Prot', 'AWGyrate_Prot'
               #, 'Flux_Prot'
               )
dfsim1 <- dfrelmetric[dfrelmetric$MetricPair %in% cols_sim1, ]


dfall_cor_prot <- subset(dfall_cor_prot, MetricPair != 'Prot_Prot')
dfsimprot <- merge(dfsim1, dfall_cor_prot, by=c('Region', 'MetricPair'))

p1<-ggplot(data=dfsimprot, aes(x=Threshold, y=Value, color=MetricPair))+
  #geom_smooth(se=F, alpha=0.2
              #, show.legend = FALSE
  #)+
  geom_line()+
  #geom_point(size=1, aes(fill=MetricPair), color='white')+
  #scale_color_gradient2(
  #  low="darkred",
  #  mid="white",
  #  high="dodgerblue",
  #  midpoint=0
  #)+
  # scale_fill_viridis(discrete=TRUE) +
  # scale_color_viridis(discrete=TRUE) +
  scale_color_manual(values=col_scheme1)+
  facet_wrap(~Region)+
  #geom_vline(yintercept=25)+
  geom_hline(yintercept=50, linetype='dashed')+
  #scale_y_continuous(limits=c(0,50))+
  theme_classic()
#horizontal: abs diff between prot and selected metrics
#vertical: percent of simulations
#try add more space between each region's plot
#re-label the regions to (a) California

ggplotly(p1)


col_scheme2 <- c('#3198ce',
                 '#CE6731',
                 '#668A99',
                 '#D52AD2',
                 '#4D20DF','#B2DF20', '#0EF1D7','#E7185E')
cols_sim2 <- c(
  #'Dist_AWF','Dist_IIC','Dist_PC','Dist_ECA','Dist_ProtConn',
               'Prox_Dist', 
               #'Dist_Prot', 
               'Dist_Compart', 
               'Dist_ClusCoeff',
               'Dist_Degree', 'Dist_Gyrate', 'Dist_BC', 'Dist_Cohesion'
               #,'Dist_BA', 'Dist_AWGyrate', 
               ,'Dist_Flux'
               )
dfsim2 <- dfrelmetric[dfrelmetric$MetricPair %in% cols_sim2, ]

dfall_cor_dist <- subset(dfall_cor_dist, MetricPair != 'Dist_Dist')
dfall_cor_dist[dfall_cor_dist$MetricPair=='Dist_Prox', ]$MetricPair <- 'Prox_Dist'
dfsimdist <- merge(dfsim2, dfall_cor_dist, by=c('Region', 'MetricPair'))

p2<-ggplot(data=dfsimdist, aes(x=Threshold, y=Value, color=MetricPair))+
  #geom_smooth(se=F, alpha=0.2
              #, show.legend = FALSE
   #           )+
  geom_line()+
  #scale_color_gradient2(
    #low="darkred",
    #mid="white",
    #high="dodgerblue",
    #midpoint=0
  #)+
  #scale_color_viridis(discrete=TRUE) +
  scale_color_manual(values = col_scheme2)+
  facet_wrap(~Region)+
  #geom_vline(yintercept=25)+
  geom_hline(yintercept=50, linetype='dashed')+
  #scale_y_continuous(limits=c(0,50))+
  theme_classic()

ggplotly(p2)

