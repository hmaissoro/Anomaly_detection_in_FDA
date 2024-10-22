
simulate_contaminated_curves <- function(N, n_sampling_point, n_eolienne, n_contanimated_parc, n_contanimated_eolienne, mean  = 100, sd = 1){
  
  # Simulation des données
  random_prod <- vector(mode = "list", length = N)
  Time <- seq(0,1, by = 1/n_sampling_point)
  for (i in 1:N) {
    # On prend une matrice vide avec les indexes du temp
    production_by_parc <- matrix(0, ncol = n_eolienne+1, nrow = n_sampling_point)
    production_by_parc[,1] <- Time[1:n_sampling_point]
    
    # On simule les données
    for(j in 2:n_eolienne){
      production_by_parc[,j] <- abs(rnorm(n = n_sampling_point,
                                          mean = mean,
                                          sd = sd))
    }
    random_prod[[i]] <- production_by_parc
  }
  
  # Ajout anomalie
  
  # -- 1) On prend 50 parc par harsard
  id_parc_random <- sample(1:N, n_contanimated_parc)
  
  # -- 2) On chaque parc 
  for(id in id_parc_random){
    df_by_parc <- random_prod[[id]]
    
    # -- 3) On prend 5 éolienne par hasard
    id_eolienne <- sample(1:n_eolienne, n_contanimated_eolienne)
    for (id_eol in id_eolienne) {
      # -- 3.1) On prend la partie qui sera comtaminée
      B1 <- runif(n = 1, min = 0, max = 1)
      B2 <- runif(n = 1, min = 0, max = 1)
      if(B1 < B2){
        U_1 <- B1
        U_2 <- B2
      }else{
        U_1 <- B2
        U_2 <- B1
      }
      
      # -- 3.2) On met à zéro la production de ces éoliennes
      underproduction_area_length <- length(df_by_parc[which(df_by_parc[,1] >= U_1 & df_by_parc[,1] <= U_2), id_eol])
      df_by_parc[which(df_by_parc[,1] >= U_1 & df_by_parc[,1] <= U_2), id_eol] <- abs(rnorm(
        n = underproduction_area_length,
        mean = mean*99/100,
        sd = sd))
    }
    
    # -- 4) On stocke les données contaminée
    random_prod[[id]] <- df_by_parc
  }
  
  # -- 5) On prend la production totale d'un parc comme somme des productions par éolienne
  
  for(i in 1:N){
    # -- 5.1) On prend les données par parc
    df_by_parc <- random_prod[[i]]
    
    # -- 5.2) On agrége :sum et on divis par le max pour se ramer sur [0,1]
    dt <- rowSums(df_by_parc[, 2:n_eolienne])/max(rowSums(df_by_parc[, 2:n_eolienne]))
    if(i ==1){
      final_data <- dt
    }else{
      final_data <- rbind(final_data,dt)
    }
  }
  # -- 6) On crée une variable pour garder la trace des courbes contaminée par une sous-production
  final_data <- cbind(id_prod = 1:N, 
                      label = ifelse(1:N %in% id_parc_random, "contaminated", "safe"),
                      final_data)
  
  # -- 7) On renvoie les données simulées
  return(final_data)
}