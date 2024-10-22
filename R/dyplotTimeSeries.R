# DataTable = data.table(id_prod = ...,
#                        date = ...,
#                        FDC = ...)

if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
if (! require('dygraphs', quietly = TRUE)){ install.packages('dygraphs') } ; library('dygraphs', quietly = TRUE)
if (! require('magrittr', quietly = TRUE)){ install.packages('magrittr') } ; library('magrittr', quietly = TRUE)


dyplotTimeSeries <- function(data = data.table,
                             id_parc = "",
                             col_date = "",
                             col_series = "",
                             color = "#4682B4",
                             main = "",
                             group = ""){
  
  if ( ! col_date %in% colnames(data) & col_series %in% colnames(data) ){
    message("Error ! Check the 'col_date' and 'col_series' arguments...")
  } else {
    # Construct data
    dt <- data[, .(id_prod, get(col_date), get(col_series))][id_prod == id_parc]
    colnames(dt) <- c("id_prod", col_date, col_series)
      
    data_one_ts <- dcast(dt, date ~ id_prod, value.var = col_series)
    
    # Construct plot
    plot_ts <- dygraphs::dygraph(data_one_ts, 
                                 main = ifelse(main != "", main, paste("Wind farm ", id_parc, sep = " ")),
                                 group = group) %>% 
      dySeries(label = col_series, color = color) %>%
      dyAxis("y", label = "Load profile") %>% 
      dyRangeSelector(height = 20) %>% 
      dyCSS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/css/dyCSS.css")
    
    # return contructed plot
    return(plot_ts)
  }
}
