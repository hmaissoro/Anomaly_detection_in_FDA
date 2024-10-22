# Import des packages
if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
if (! require('fst', quietly = TRUE)){ install.packages('fst') } ; library('fst', quietly = TRUE)
if (! require('magrittr', quietly = TRUE)){ install.packages('magrittr') } ; library('magrittr', quietly = TRUE)
if (! require('dplyr', quietly = TRUE)){ install.packages('dplyr') } ; library('dplyr', quietly = TRUE)
if (! require('fda', quietly = TRUE)){ install.packages('fda') } ; library('fda', quietly = TRUE)

# Import des fonctions
lapply(list.files('/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/R/',
                  recursive = T, full.names = T, pattern = ".R$|.r$"), function(X) {
                    source(X, encoding = 'UTF-8')
                  }) %>% invisible

# Import des données
dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")

fdc <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, FDC)],
                         formula = id_prod ~ date, value.var = "FDC")
rm(dt_fdc_ws)

#----- Calcul des profondeurs sans clustering ---- #

#Tukey and Simplicial depths
f_tukey_depth <- fdc[, -c("id_prod")] %>% functional_tukey_depth()

system.time(f_sdepth <- fdc[, -c("id_prod")] %>% functional_simplicial_depth())

fdc.depths <- data.table(id_prod = fdc[, id_prod],
                     tdepth = f_tukey_depth,
                     sdepth = f_sdepth)

# Mini analyse
fdc.depths[order(tdepth), ]
plot(fdc.depths[order(tdepth), tdepth], type = "o")

saveRDS(object = fdc.depths,
        file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/global_fdc_depths.rds")

#----- Calcul des profondeurs par cluster --- #

# 1) Lissage
fb <- create.fourier.basis(rangeval = c(1, 365*24), nbasis =  1780)
fdc.fd <- Data2fd(argvals = 1:(365*24), y = t(fdc[, -1]),
                  lambda = 10,
                  fdnames = list(time = "time", reps = fdc[,id_prod], values = "normalized load curve"),
                  basisobj = fb, method="chol", dfscale=1)

# 2) Clustering
fdc.cluster <- curves_FPCAclustering(smooth_curves = fdc.fd,
                                     ncp = 5, 
                                     clustering_method = "hclust",
                                     ncluster = 12)

fdc.cluster[, .N, by = cluster_index]

# 3) On merge et on calcul la profondeur par cluster

fdc_cluster_index <- data.table::merge.data.table(x = fdc.cluster, y = fdc, by = "id_prod")


fdc.depths_by_cluster <- data.table::rbindlist(lapply(unique(fdc_cluster_index[, cluster_index]), function(clust) {
  
  
  # On extrait les données du cluster
  fdc_by_clust <- fdc_cluster_index[cluster_index %in% clust, ]
  if(nrow(fdc_by_clust) > 15){
    # # Calcul des fonctions profondeurs
    f_tukey_depth <- fdc_by_clust[, -c("id_prod", "cluster_index")] %>% functional_tukey_depth()
    f_sdepth <- fdc_by_clust[, -c("id_prod", "cluster_index")] %>% functional_simplicial_depth()
    
    # On construit un data.table
    data.table(id_prod = fdc_by_clust[, id_prod],
               cluster_index = fdc_by_clust[, cluster_index],
               tdepth = f_tukey_depth,
               sdepth = f_sdepth)
  }
}))


saveRDS(object = fdc.depths_by_cluster,
        file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/fdc_depths_by_cluster.rds")











