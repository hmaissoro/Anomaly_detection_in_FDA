if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
if (! require('fst', quietly = TRUE)){ install.packages('fst') } ; library('fst', quietly = TRUE)
if (! require('magrittr', quietly = TRUE)){ install.packages('magrittr') } ; library('magrittr', quietly = TRUE)
if (! require('dplyr', quietly = TRUE)){ install.packages('dplyr') } ; library('dplyr', quietly = TRUE)
if (! require('foreach', quietly = TRUE)){ install.packages('foreach') } ; library('foreach', quietly = TRUE)

# --- Treated CDC --- #
dt_cdc <- readRDS("/home/projets/2020-A258-ENEDIS_Prev/Eolien/data_final_2016_2017.RDS")$data

# dt contient les données de localisation
dt <- read.fst("/home/projets/2020-A258-ENEDIS_Prev/Eolien/new_DT_PROD_eolien_2014_2017_format.fst", as.data.table = T)

# -- On extrait la longitude et la latitude -- #
# ---- Extraction des cdc chaque 1h en 2h --- #
# -- 
date <- seq(from = as.POSIXct("2017-1-1 00:00", tz="GMT"),
            to = as.POSIXct("2017-12-31 23:00", tz="GMT"),
            length.out = 365*24)

# Pour chaque courbe de charge, on prend l'observation chaque 1h
# NB : dans un premier temps on garde que les courbes ayant une observation par heure...
#    : Lorsque le nombre d'observations est inférieur à 365*24...On y reviendra et on changera la méthode d'estimation du modèle linéaire


dt_uni <- dt[, .(LON = unique(LON), LAT = unique(LAT)), by = id_prod]

dt_cdc_by_hour <- dt_cdc[Horodate_HL48 %in% date, .SD, by = id_prod
                         ][, len := .N, by = id_prod
                           ][len == 8760, .(date = Horodate_HL48, FDC = FC), by = id_prod]

dt_fdc <- data.table::merge.data.table(
  x = dt_uni,
  y = dt_cdc_by_hour,
  by = "id_prod")



saveRDS(object = dt_fdc, file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_by_hour.rds")

# ------------- On Supprime les objects non necessaires --------- #

rm(dt, dt_cdc)

# --------------------------------------------------------------- #

# ------------- Import des données du 'vent' ------------- #

dt_wind <- readRDS(file = "/home/projets/2020-A258-ENEDIS_Prev/DUMP_ARPEGE100/mep_arp100_filter_pvrf_HL48_2017_run2020.RDS")

# On a un enregistrement chaque 1h
dt_wind[, .N, by = .(lat, lon)]
class(dt_wind$Horodate_HL48)


# --- Appairage (parc éolien, station météo) --- #

# On crée un id pour la vitesse du vent
dt_wind[, id_ws := paste("ws", lon, lat, sep="-"),]

dt_ws_loc <- dt_wind[, .(LON = unique(lon), LAT = unique(lat)), by = id_ws]

dt_loc <- dt_fdc[, .(LON = unique(LON), LAT = unique(LAT)), by = id_prod]

parc_ws <- data.table::rbindlist(lapply(dt_loc[, id_prod], function(id) {
  
  parc_lon <- dt_loc[id_prod %in% id, LON]
  parc_lat <- dt_loc[id_prod %in% id, LAT]
  dist_eucl <- sqrt((parc_lon - dt_ws_loc$LON)^2 + (parc_lat - dt_ws_loc$LAT)^2)
  
  data.table(id_prod = id,
             id_ws = dt_ws_loc[which.min(dist_eucl), id_ws],
             lon_parc = parc_lon,
             lat_parc = parc_lat,
             lon_ws = dt_ws_loc[which.min(dist_eucl), LON],
             lat_ws = dt_ws_loc[which.min(dist_eucl), LAT])
  
}))


saveRDS(object = parc_ws, file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/matching_parc_ws.rds")


# --------------------- Quelques plot de vérification  ----------------------- #
library(raster)

# Les fermes sur la carte
# - 1) contour de la carte de FR
adm_fr <- getData('GADM', country='FRA', level=1)

# - 2) Extraction des fermes

# - 2) Carte

par(mar=c(1,1,1,1),bg="transparent")
plot(adm_fr, main = 'Répartition des fermes éoliennes')
#
points(parc_ws[id_ws %in% "ws-4.1-48.6", lon_ws], parc_ws[id_ws %in% "ws-4.1-48.6", lat_ws], 
       col = "blue", pch=5, cex=0.6)
points(parc_ws[id_ws %in% "ws-4.1-48.6", lon_parc], parc_ws[id_ws %in% "ws-4.1-48.6", lat_parc], 
       col = "red", pch=5, cex=0.6)
#
points(parc_ws[id_ws %in% "ws-4.2-48.9", lon_ws], parc_ws[id_ws %in% "ws-4.2-48.9", lat_ws], 
       col = "blue", pch=5, cex=0.6)
points(parc_ws[id_ws %in% "ws-4.2-48.9", lon_parc], parc_ws[id_ws %in% "ws-4.2-48.9", lat_parc], 
       col = "red", pch=5, cex=0.6)
#
points(parc_ws[id_ws %in% "ws-4-48.7", lon_ws], parc_ws[id_ws %in% "ws-4-48.7", lat_ws], 
       col = "blue", pch=5, cex=0.6)
points(parc_ws[id_ws %in% "ws-4-48.7", lon_parc], parc_ws[id_ws %in% "ws-4-48.7", lat_parc], 
       col = "red", pch=5, cex=0.6)

#---- Combinaison données météo et CDC --- #

# Ajouter de id_ws par parc
df_fdc <- merge(as.data.frame(dt_fdc), as.data.frame(parc_ws[, .(id_prod, id_ws)]), by = "id_prod")
df_wind <- merge(as.data.frame(dt_wind), as.data.frame(parc_ws[, .(id_prod, id_ws)]), by = "id_ws")

# Ajout des données météo
df_fdc_ws <- merge(as.data.talbe(dt_fdc), as.data.talbe(df_wind), by = "id_prod")




# ---------------------------------------------------------------------------- #


####
dt_fdc <- data.table::merge.data.table(
  x = dt_fdc, 
  y = parc_ws[, .(id_prod, id_ws)],
  by = "id_prod")

dt_wind <- data.table::merge.data.table(
  x = dt_wind,
  y = parc_ws[, .(id_prod, id_ws)],
  by = "id_ws")

# Ajout des données météo
summary(dt_wind[, Horodate_HL48])
dt_wind[, Horodate_HL48 := Horodate_HL48 - lubridate::hours(1)]

dt_fdc_ws <- data.table::merge.data.table(
  x = dt_fdc, 
  y = dt_wind, 
  by.x = c("id_prod", "date"),
  by.y = c("id_prod", "Horodate_HL48"))

saveRDS(object = dt_fdc_ws, file = "/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")


