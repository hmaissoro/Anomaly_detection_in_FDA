# Fonction profondeur simpliciale
# Prend en argument un vecteur c(X(t_1), X(t_2), ...., X(t_N))

simplicial_depth <- function(sample_vector){
  
  #--- Fonction de répartition
  if(length(unique(sample_vector)) == length(sample_vector)){
    # Pour aller plus vite
    fr.x <- order(order(sample_vector)) / length(sample_vector)
  }else{
    fr.x <- repartition_function(sample_vector)
  }
    
  #--- Fonction depth
  depth <- 2*fr.x*(1-fr.x)
  
  return(depth)
}
