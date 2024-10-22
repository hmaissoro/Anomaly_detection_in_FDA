# -------------- Package ---------------------- #
library('fda.usc')
library('fda')

# ---
print("#-------- Import des packages : ok --------#")
# ---

# -------------- Import des données ----------- #
# Import des données, moyenne de chaque 2h
# raison moyenne chaque 2h : temps de calcul long

df_mean_2h <- readRDS("~/Stage/functionnal_data_analysis/data_exploration/df_mean_2h.rds")
df_mat <- as.matrix(df_mean_2h[, -1])
rownames(df_mat) <- df_mean_2h[,1]

# ---
print("#-------- Import des données : ok --------#")
# ---

# -------------- Recherche du lissage optimal -------- #
# On va utiliser la fonction "optim.basis"
# qui prend un 'fdata object' en argument

# 1)--- fdata object
df_fdata <- fdata(df_mat, argvals = 1:ncol(df_mat))

# 2)--- Grid for grid search 
nb <- floor(seq(1000, 2000, 10))
l <- seq(1, 10, by = 1)

# 3)--- Grid search 

df_fdata.opt_bas <- optim.basis(df_fdata,
                              lambda = l,
                              numbasis = readRDS("~/Stage/functionnal_data_analysis/anomaly_detection/data/df_fdata_opt_bas.rds")$numbasis.opt,
                              type.basis = "fourier")
# 4)--- Saving
saveRDS(object = df_fdata.opt_bas, 
        file = "~/Stage/functionnal_data_analysis/anomaly_detection/data/df_fdata_opt_bas_bis.rds")


# ---
print("#-------- Recherche terminée --------#")
# ---

# ---------------------------------------------------------------- #


