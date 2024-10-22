# ---- Import des données et fonctions ---- #

dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")

# ---- #
# Import des fonctions
lapply(list.files('/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/R/',
                  recursive = T, full.names = T, pattern = ".R$|.r$"), function(X) {
                    source(X, encoding = 'UTF-8')
                  }) %>% invisible
# ---- #
# NB : Les PI des parc c("137668", "551629") sont males renseignées

fdc <- data.table::dcast(data = dt_fdc_ws[ ! id_prod %in% c("137668", "551629"), .(id_prod, date, FDC)],
                         formula = id_prod ~ date, value.var = "FDC")

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
                                     ncluster = 7)

# 4 Clusters en among c'est assez

fdc.cluster[, .N, by = cluster_index]

# 3) On merge et on calcul la profondeur par cluster
# dt_fdc_ws %>% names()
fdc_cluster_index <- data.table::merge.data.table(x = fdc.cluster, y = fdc, by = "id_prod")


fdc_cluster_index[cluster_index == 10, id_prod]

# NB : La PI du parc 137668 est mal renseignée


fdc_cluster_index[cluster_index == 10, .SD, .SDcols = ! c("id_prod", "cluster_index")]
df <- data.table(date = fdc_cluster_index[cluster_index == 10, .SD, .SDcols = ! c("id_prod", "cluster_index")] %>% 
                   names() %>% 
                   as.POSIXct(tz = "UTC"),
                 FDC = as.numeric(c(fdc_cluster_index[cluster_index == 10, .SD, .SDcols = ! c("id_prod", "cluster_index")])))

rAmCharts::amTimeSeries(data = df,
                        col_date = "date", 
                        col_series = "FDC", 
                        ylab = "Facteur de charge", 
                        xlab = "Date")
