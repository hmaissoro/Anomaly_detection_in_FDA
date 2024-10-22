# ----------- Import des données ------- #
dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")

# Import des profondeurs calculées
fdc_depths <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/global_fdc_depths.rds")
fdc_depths_by_cluster <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/fdc_depths_by_cluster.rds")
rfdc_depths <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/depths_on_global_residuals.rds")
rfdc_depths_by_cluster <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/fdc_residuals_depths_by_cluster.rds")

# ------------------------------------------------------------ #

# ----------- Détection sans clustering ---------------------- #

# Evolution des courbes de charge

rAmCharts::amPlot(x = rfdc_depths[order(tdepth), tdepth], 
                  y = rfdc_depths[order(sdepth), sdepth], 
                  col = "#ad1b0e",
                  xlab = "Tukey depth",
                  ylab = "Simplicial depth")


lm(rfdc_depths[order(tdepth), tdepth]~ rfdc_depths[order(sdepth), sdepth]) %>% summary

# fdc_depths %>% as.data.frame() -> df

# --- Pots pour mettre en exergue les coudes --- # 

# --- Profondeur de Tukey

plot_tdepth <- rAmCharts::amPlot(x = fdc_depths[order(tdepth), id_prod],
                                 y = fdc_depths[order(tdepth), tdepth],
                                 labelRotation = -45,
                                 type = "both",
                                 xlab = "id du parc éolien",
                                 ylab = "profondeur de Tukey") %>% 
  amOptions(
    theme = "light",
    export = FALSE,
    zoom = TRUE,
    scrollbar = TRUE
  ) %>% plot()

# Modification des noms de variables dans 'balloonText'
plot_tdepth$x$chartData$graphs[[1]]$balloonText <- gsub("^value", "tdepth", plot_tdepth$x$chartData$graphs[[1]]$balloonText)
plot_tdepth

plot_tdepth$x$chartData$graphs[[1]]$valueField

# --- Profondeur Simpliciale

plot_sdepth <- rAmCharts::amPlot(x = fdc_depths[order(sdepth), id_prod],
                                 y = fdc_depths[order(sdepth), sdepth],
                                 labelRotation = -45,
                                 type = "both",
                                 col = "#ad1b0e",
                                 xlab = "id du parc éolien",
                                 ylab = "profondeur simplicale") %>% 
  amOptions(
    theme = "light",
    export = TRUE,
    zoom = TRUE,
    scrollbar = TRUE
  ) %>% plot()

plot_sdepth$x$chartData$graphs[[1]]$balloonText <- gsub("^value", "sdepth", plot_sdepth$x$chartData$graphs[[1]]$balloonText)
plot_sdepth

# --- Profondeur de Tukey et Profondeur Simpliciale

plot_tsdepth <- rAmCharts::amPlot(x = fdc_depths[order(tdepth), id_prod],
                                  y = fdc_depths[order(tdepth), tdepth],
                                  labelRotation = -45,
                                  type = "both",
                                  title = "Tukey depth",
                                  xlab = "id du parc éolien",
                                  ylab = "profondeur de Tukey",
                                  legend = TRUE) %>% 
  amLines(fdc_depths[order(sdepth), sdepth],
          type = "both",
          title = "Simpliciale depth",
          col = "#ad1b0e") %>% 
  amOptions(
    theme = "light",
    export = TRUE,
    zoom = TRUE,
    scrollbar = TRUE
  ) %>% plot()

# Modification des noms de variables dans 'balloonText'
plot_tsdepth$x$chartData$graphs[[1]]$balloonText <- gsub("^value", "tdepth", plot_tsdepth$x$chartData$graphs[[1]]$balloonText)
plot_tsdepth$x$chartData$graphs[[2]]$balloonText <- gsub("^Simpliciale depth", "sdepth", plot_tsdepth$x$chartData$graphs[[2]]$balloonText)
plot_tsdepth
# Comment arrondir les valeur dans 'balloonText' ?
# Comment montrer la courbe de charge sur clic ?

# ------------------------------------------------------------ #

# ----------------------- Multiple widget -------------------- #
library(manipulateWidget)

manipulateWidget::combineWidgets(list = list(plot_tdepth, plot_sdepth), 
                                 ncol = 1, 
                                 width = "400px",
                                 height = "520px")
# ------------------------------------------------------------ #
 
# ----------------------- Nuage de points --------------------- #
id_parc <- unique(dt_fdc_ws[, id_prod])
plot_fdc_wind_scatter <- rAmCharts::amPlot(x = dt_fdc_ws[id_prod %in% id_parc[1], wind_sp_100],
                                           y = dt_fdc_ws[id_prod %in% id_parc[1], FDC],
                                           type = "points",
                                           col = "#ad1b0e",
                                           xlab = "Wind speed at 100m",
                                           ylab = "Load ratio curve") %>% 
  amOptions(
    theme = "light",
    export = TRUE,
    zoom = TRUE,
    scrollbar = TRUE
  ) %>% plot()

# Modification des noms de variables dans 'balloonText'
plot_fdc_wind_scatter$x$chartData$graphs[[1]]$balloonText <- "Vistesse du vent : <b>[[x]] m/s</b><br>Facteur de charge : <b>[[y]]</b><br>"

plot_fdc_wind_scatter
# ------------------------------------------------------------ #

# --------------------- Time serie plot----------------------- #
id_parc <- unique(dt_fdc_ws[, id_prod])

# Construction du data.frame
df <- data.frame(date = dt_fdc_ws[id_prod %in% id_parc[1], date],
                 FDC = dt_fdc_ws[id_prod %in% id_parc[1], FDC])

rAmCharts::amTimeSeries(data = df,
                        col_date = "date",
                        col_series = "FDC",
                        main = paste("Parc", id_parc[1], sep = " "),
                        xlab = "date",
                        ylab = "Facteur de charge")

# ------------------------------------------------------------ #


# --- ----#
anomaly_id_parc_res <- rfdc_depths[tdepth <=0.21, id_prod]
anomaly_id_parc <- fdc_depths[order(tdepth), id_prod][1:length(anomaly_id_parc_res)]

identique_detection <- anomaly_id_parc %in% anomaly_id_parc_res

summary(identique_detection)

# -- -
id <- which(identique_detection==FALSE)
par(mfrow = c(1,1))

plot.ts(dt_fdc_ws[id_prod %in% anomaly_id_parc[id[2]], FDC])

plot(x = dt_fdc_ws[id_prod %in% anomaly_id_parc[id[2]], wind_sp_100],
     y = dt_fdc_ws[id_prod %in% anomaly_id_parc[id[2]], FDC])

for(i in id[1:2]){
  plot(x = dt_fdc_ws[id_prod %in% anomaly_id_parc[i], wind_sp_100],
       y = dt_fdc_ws[id_prod %in% anomaly_id_parc[i], FDC])
}

id <- which(identique_detection==FALSE)
par(ask = TRUE)
for(i in id){
  plot(x = dt_fdc_ws[id_prod %in% anomaly_id_parc[i], wind_sp_100],
       y = dt_fdc_ws[id_prod %in% anomaly_id_parc[i], FDC])
}

id <- which(identique_detection==TRUE)
par(ask = TRUE)
for(i in id){
  plot.ts(dt_fdc_ws[id_prod %in% anomaly_id_parc[i], FDC])
}


