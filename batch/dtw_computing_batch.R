# -------------- Package ---------------------- #
library('dtw', quietly = TRUE)
library('doParallel', quietly = TRUE)
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

# -------------- Calcul de la matrice dtw -------- #

# Calcul de la distance DTW
dist_dtw <- matrix(data = 0, nrow = nrow(df_mat), ncol = nrow(df_mat))

# 1) - la matrice de distance est symétrique
# 2) - il suffit de calculer la partie triangulaire inférieure
# 3) - la diagonale est nulle

# 4) - On va utiliser la parrelisation

# ----------- Parallélisation 
# 1) Récupère le nombre de coeurs composant la machine
cl <- detectCores()

# 2) Crée les ensembles de copies de R
makeCluster(cl - 6)

# 3) Enregistre le backend parallèle pour la parallélisation du processus
registerDoParallel(cl - 6)


# ----------- Calcul de la distance DTW
# 1)--- Calcul de la matrice de distance

Sys.time()
for(i in 2:nrow(df_mat)){
  for(j in 1:(i-1)){
    
    # On calcul la distance DTW
    dist_dtw[i, j] <- dtw(na.omit(df_mat[i,]), na.omit(df_mat[j,]), distance.only=TRUE)$normalizedDistance
    
    # On affecte la valeur transposée
    dist_dtw[j, i] <- dist_dtw[i, j]
  }
}
Sys.time()

# 4)--- Saving
saveRDS(object = dist_dtw, 
        file = "~/Stage/functionnal_data_analysis/anomaly_detection/data/dist_dtw_mean_2h.rds")

# ---
print("# -------------- Calcul de la matrice dtw : fin -------- #")
# ---
# --------------------------------------------------- #
