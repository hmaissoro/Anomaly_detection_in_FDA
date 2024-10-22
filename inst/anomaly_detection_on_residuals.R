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

wind_sp <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, wind_sp_100)],
                             formula = id_prod ~ date, value.var = "wind_sp_100")

rm(dt_fdc_ws)

# --- Lissage des courbes avec des paramètres par defaut --- #

# Base de fourier
fb <- create.fourier.basis(rangeval = c(1, 365*24), nbasis =  1780)

# 1) Lissage
fdc.fd <- Data2fd(argvals = 1:(365*24), y = t(fdc[, -1]),
                  lambda = 10,
                  basisobj = fb,
                  fdnames = list(time = "time", reps = fdc[,id_prod], values = "normalized load curve"))

wind_sp.fd <- Data2fd(argvals = 1:(365*24), y = t(wind_sp[, -1]),
                      lambda = 10,
                      basisobj = fb,
                      fdnames = list(time = "time", reps = wind_sp[,id_prod], values = "wind speed at 100m"))

# Extraction des résidus
residus_global <- get_linear_model_resuals(y_curves = fdc.fd, x_curves = wind_sp.fd, y.ncp = 5, x.ncp = 5)

#----- Calcul des profondeurs sans clustering ---- #

#Tukey and Simplicial depths
f_tukey_depth <- residus_global[, -c("id_prod")] %>% functional_tukey_depth()
f_sdepth <- residus_global[, -c("id_prod")] %>% functional_simplicial_depth()

residus.depths <- data.table(id_prod = residus_global[, id_prod],
                         tdepth = f_tukey_depth,
                         sdepth = f_sdepth)

# Mini analyse
residus.depths[order(tdepth), ]
plot(residus.depths[order(tdepth), tdepth], type = "o")

saveRDS(object = residus.depths,
        file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/depths_on_global_residuals.rds")

#----- Calcul des profondeurs par cluster --- #

# 1) Lissage
  
  # Déjà fait

# 2) Clustering
fdc.cluster <- curves_FPCAclustering(smooth_curves = fdc.fd,
                                     ncp = 5, 
                                     clustering_method = "hclust",
                                     ncluster = 12)

fdc.cluster[, .N, by = cluster_index]

# 3) On merge et on calcul la profondeur par cluster

fdc_cluster_index <- data.table::merge.data.table(x = fdc.cluster, y = fdc, by = "id_prod")

fdc_residuals.depths_by_cluster <- data.table::rbindlist(lapply(unique(fdc_cluster_index[, cluster_index]), function(clust) {
  
  # 1) On extrait les données du cluster
  fdc_by_clust <- fdc_cluster_index[cluster_index %in% clust, ]
  if(nrow(fdc_by_clust) > 15){
    # 2) Extraction des données de la vitesse du vent
    id_parc <- unique(fdc_by_clust[, id_prod])
    wind_sp_by_cluster <- wind_sp[id_prod %in% id_parc, ]
    
    # 3) Lissage
    fdc_by_clust.fd <- Data2fd(argvals = 1:(365*24), y = t(fdc_by_clust[, -c("id_prod", "cluster_index")]),
                               lambda = 10,
                               basisobj = fb,
                               fdnames = list(time = "time", reps = fdc_by_clust[,id_prod], values = "normalized load curve"))
    wind_sp_by_clust.fd <- Data2fd(argvals = 1:(365*24), y = t(wind_sp_by_cluster[, -c("id_prod")]),
                               lambda = 10,
                               basisobj = fb,
                               fdnames = list(time = "time", reps = wind_sp_by_cluster[,id_prod], values = "wind speed at 100m"))
    
    # 4) Extraction des résidus
    residus_by_cluster <- get_linear_model_resuals(y_curves = fdc_by_clust.fd, 
                                                   x_curves = wind_sp_by_clust.fd, 
                                                   y.ncp = 5, 
                                                   x.ncp = 5)
  
    # 5.1) Calcul des fonctions profondeurs
    f_tukey_depth <- fdc_by_clust[, -c("id_prod", "cluster_index")] %>% functional_tukey_depth()
    f_sdepth <- fdc_by_clust[, -c("id_prod", "cluster_index")] %>% as.matrix() %>% functional_simplicial_depth()
    
    # 5.2) On construit un data.table
    data.table(id_prod = fdc_by_clust[, id_prod],
               cluster_index = fdc_by_clust[, cluster_index],
               tdepth = f_tukey_depth,
               sdepth = f_sdepth)
  }
})) 


saveRDS(object = fdc_residuals.depths_by_cluster,
        file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/fdc_residuals_depths_by_cluster.rds")
