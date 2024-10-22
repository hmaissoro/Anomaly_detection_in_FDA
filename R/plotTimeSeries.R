# DataTable = data.table(id_prod = ...,
#                        date = ...,
#                        FDC = ...)

if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
if (! require('rAmCharts', quietly = TRUE)){ install.packages('rAmCharts') } ; library('rAmCharts', quietly = TRUE)
if (! require('magrittr', quietly = TRUE)){ install.packages('magrittr') } ; library('magrittr', quietly = TRUE)

plotTimeSeries <- function(data = DataTable,
                           id_parc = "",
                           col_date = "",
                           col_series = "",
                           color = "#4682B4",
                           main = "",
                           scrollbar = FALSE){
  
  if ( ! col_date %in% colnames(data) & col_series %in% colnames(data) ){
    message("Error ! Check the 'col_date' and 'col_series' arguments...")
  } else {
    # Construct data
    df <- data.frame(date = data[id_prod %in% id_parc, get(col_date)],
                     FDC = data[id_prod %in% id_parc, get(col_series)])
    
    # Construct plot
    plot_ts <- rAmCharts::amTimeSeries(data = df,
                                       col_date = col_date,
                                       col_series = col_series,
                                       color = color,
                                       main = ifelse(main != "", main, paste("Wind farm ", id_parc, sep = " ")),
                                       xlab = "Date",
                                       ylab = "Load ratio curve",
                                       maxSeries = nrow(df) + 10,
                                       scrollbar = scrollbar) %>% plot()
    
    # return contructed plot
    return(plot_ts)
  }
}
