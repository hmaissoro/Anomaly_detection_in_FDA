# Fonction profondeur de Tukey
# Prend en argument un vecteur c(X(t_1), X(t_2), ...., X(t_N))

tukey_depth <- function(sample_vector){
  
  #--- Fonction de répartition
  fr.x <- repartition_function(sample_vector)
  
  #--- Fonction depth
  depth <- 1/2 - abs(1/2 - fr.x)
  
  return(depth)
}
