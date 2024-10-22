# --- Test plotDepth --- #

fdc_depths <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/global_fdc_depths.rds")

# Tukey depth
plotDepth(data = fdc_depths, col_id = "id_prod", col_depth = "tdepth")

# Simpliciale depth
plotDepth(data = fdc_depths, col_id = "id_prod", col_depth = "sdepth")

# Both on same graph
plotDepth(data = fdc_depths, col_id = "id_prod", col_depth = c("tdepth", "sdepth"))
