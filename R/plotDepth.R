# Prend en argument un data = data.table (id_prod = ...,
#                                         tdepth = ...,
#                                         sdepth = ...)
# col_depth = "tdepth", "sdepth", "c('tdepth', 'depth')"

if (! require('rAmCharts', quietly = TRUE)){install.packages('rAmCharts')} ; library('rAmCharts', warn.conflicts = FALSE)
if (! require('data.table', quietly = TRUE)){install.packages('data.table')} ; library('data.table', warn.conflicts = FALSE)

plotDepth <- function(data = depthResults, col_id = "id_prod", 
                      col_depth = "tdepth", title = "", col = "", 
                      export = FALSE, scrollbar = FALSE, ylim = c(0, 0.5)){
  
  if(length(col_depth) == 1){
    if( col_depth %in% c("tdepth", "sdepth") ){
      plot_depth <- rAmCharts::amPlot(x = data[order(get(col_depth)), get(col_id)],
                                      y = data[order(get(col_depth)), get(col_depth)],
                                      labelRotation = -45,
                                      type = "both",
                                      col = col,
                                      title = title,
                                      xlab = "Wind farm identifier",
                                      ylab = ifelse(col_depth == "tdepth", "Tukey depth", "Simplicial depth"),
                                      ylim = ylim) %>% 
        amOptions(
          theme = "light",
          export = export,
          zoom = TRUE,
          scrollbar = scrollbar
        ) %>% plot()
      
      # Modification des noms de variables dans 'balloonText'
      plot_depth$x$chartData$graphs[[1]]$balloonText <- gsub("^value",
                                                             col_depth,
                                                             plot_depth$x$chartData$graphs[[1]]$balloonText)
      
      return(plot_depth)
      # On vérifie qu'on a bien c('tdepth', 'sdepth') ou c('tdepth', 'sdepth')
    }else{
      message(" Error ! invalide 'col_depth' argument...")
    }
    
  }else if(length(col_depth) == 2){
    
    if( col_depth[1] %in% c("tdepth", "sdepth") & col_depth[2] %in% c("tdepth", "sdepth")){
      
      plot_tsdepth <- rAmCharts::amPlot(x = data[order(get(col_depth[1])), get(col_id)],
                                        y = data[order(get(col_depth[1])), get(col_depth[1])],
                                        labelRotation = -45,
                                        type = "both",
                                        title = ifelse(col_depth[1] == "tdepth", "Tukey depth", "Simplicial depth"),
                                        xlab = "Wind farm identifier",
                                        ylab = ifelse(col_depth[1] == "tdepth", "Tukey depth", "Simplicial depth"),
                                        legend = TRUE,
                                        ylim = ylim) %>% 
        amLines(data[order(get(col_depth[2])), get(col_depth[2])],
                type = "both",
                title = ifelse(col_depth[2] == "tdepth", "Tukey depth", "Simplicial depth"),
                col = "#ad1b0e") %>% 
        amOptions(
          theme = "light",
          export = export,
          zoom = TRUE,
          scrollbar = TRUE,
          legend = TRUE,
          legendPosition = "top") %>% plot()
      
      # Modification des noms de variables dans 'balloonText'
      plot_tsdepth$x$chartData$graphs[[1]]$balloonText <- gsub("^value",
                                                               col_depth[1],
                                                               plot_tsdepth$x$chartData$graphs[[1]]$balloonText)
      
      plot_tsdepth$x$chartData$graphs[[2]]$balloonText <- gsub(ifelse(col_depth[2] == "tdepth", 
                                                                      "^Tukey depth", 
                                                                      "^Simplicial depth"),
                                                               col_depth[2],
                                                               plot_tsdepth$x$chartData$graphs[[2]]$balloonText)
      return(plot_tsdepth)
    } else{
      message("Error ! Check the 'col_depth' argument...")
    }
  }else{
    message("Error ! Check the 'col_depth' argument...")
  }
}

