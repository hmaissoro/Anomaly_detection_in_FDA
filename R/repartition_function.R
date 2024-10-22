# Cette Fonction calcule la fonction de répartition empirique d'un échantillon
# prend en argument un vecteur

repartition_function <- function(sample_vecteur){
  # --- Taille de l'échantillon
  len <- length(sample_vecteur)
  
  # -- Calcul de la fonction de répartition
  fr.x <- rank(x = sample_vecteur, ties.method = "max")/len
  
  return(fr.x)
}
