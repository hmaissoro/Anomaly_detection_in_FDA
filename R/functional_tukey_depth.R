# Calcul la profondeur de Tukey d'un échantillon de fonction
# Prend en argument une matrice N x Sampling time 
# Renvoie un vecteur de valeurs de la fonction profondeur

functional_tukey_depth <- function(fda_matrix){
  # On calcule la profondeur instant par instant
  fda_depth <- apply(X = fda_matrix, MARGIN = 2, FUN = tukey_depth)
  
  # On normalise
  fda_depth <- rowSums(fda_depth)/ncol(fda_depth)
  
  # On renvoie le résultat
  return(fda_depth)
}


