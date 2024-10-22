# Cette function permet d'agréger les courbes en prenant la moyenne chaque 2h

# prend en argument un data.table 'dt' avec id_prod comme clef et 
# Renvoie la moyenne chaque 2h de la courbe de facteur de charge

curve_aggregation <- function(data = dt, year = 2017, nhour = 2){
  # Import package
  if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
  if (! require('lubridate', quietly = TRUE)){ install.packages('lubridate') } ; library('lubridate', quietly = TRUE)
  
  # ---- 1) Elimation des courbes dont la FDC > 2
  # On renome FC en FDC
  if( "FC" %in% names(data)){
    setnames(data, "FC", "FDC")
  }else if( ! "FDC" %in% names(data)) {
    warning("Le data.table ne contient pas la variable FDC")
  }
  
  # On pren les courbes dont tous les FDC sont < 2
  id_parc_fdc_sup_2 <- dt[FDC > 2, unique(id_prod)]
  
  data <- data[( ! id_prod %in% id_parc_fdc_sup_2) & year(Horodate_HL48) %in% year, ]
  
  # ---- 2) On agrége chaque hour = hour*2 observations
  
  # On élimine les séries dont la taille est < 17520, soit un an de données
  truncated_curves <- data[, .(year_length = length(FDC) < 17520), by = id_prod]
  
  # On calcul la moyenne chaque nhour heures
  data_aggregated <- data[ ! id_prod %in% truncated_curves[year_length == TRUE, id_prod],
                    .(FDC = FDC %>% as.ts %>% aggregate(, ndeltat = nhour*2, FUN = mean)),
                    by = id_prod]
  
  # ---- 3) Ajout de la variable date
  
  date <- seq(from = as.POSIXct("2017-1-1 00:00", tz="GMT"),
              to = as.POSIXct("2017-12-31 22:00", tz="GMT"),
              length.out = 365*24*2/(nhour*2))
  d <- rep(date, length(unique(data_aggregated[, id_prod])))

  data_aggregated[, date := d]
  
  # ---- 4) On renvoie un data.table contenant : id_prod, FDC, et date.
  return(data_aggregated)
}
