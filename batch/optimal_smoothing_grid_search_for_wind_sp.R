# -------------- Package ---------------------- #
library('fda.usc')
library('fda')
library('data.table')
library('magrittr')

# ---
print("#-------- Import des packages : ok --------#")
# ---

# -------------- Import des données ----------- #

dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")
wind_sp <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, wind_sp_100)],
                             formula = id_prod ~ date, value.var = "wind_sp_100") %>% as.matrix()
rm(dt_fdc_ws)
# ---
print("#-------- Import des données : ok --------#")
# ---

# -------------- Recherche du lissage optimal -------- #
# On va utiliser la fonction "optim.basis"
# qui prend un 'fdata object' en argument

# 1)--- fdata object
wind_sp.fdata <- fdata(wind_sp[, -1], argvals = 1:ncol(wind_sp[, -1]))

# 2)--- Grid for grid search 
nb <- floor(seq(10, 300, 10))
l <- seq(1, 10, by = 1)

# 3)--- Grid search 

wind_sp.fdata.opt_bas <- optim.basis(wind_sp.fdata,
                                     numbasis = nb,
                                     type.CV = CV,
                                     type.basis = "fourier")
# 4)--- Saving
saveRDS(object = df_fdata.opt_bas, 
        file = "~/Stage/functionnal_data_analysis/anomaly_detection/data/wind_sp_opt_bas.rds")


# ---
print("#-------- Recherche terminée --------#")
# ---

# ---------------------------------------------------------------- #


