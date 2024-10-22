# Depth contour implementation which plotly

## data
dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")

## packages
library('grDevices')
library("plotly")

## parameters
p <- 0.99
n <- ncol(fdc[, -1])

## Plot
id_parc <- unique(dt_fdc_ws[, id_prod])

df <- dt_fdc_ws[id_prod %in% id_parc[1], .(id_prod, date, FDC)]

## standardize ??
# df[, FDC := (FDC - mean(FDC))/sd(FDC)]

df <- df[, c("time", 
             "tdepth", 
             "sdepth") := .(difftime(date, as.POSIXct("2017-01-01 00:00:00 GMT"), units = "hours"),
                            tukey_depth(FDC),
                            simplicial_depth(FDC))
]

convex.hull <- df[
  
  # On prend les n*p premier éléments
  rank(FDC, ties.method = "min") < ifelse(floor(n*p), floor(n*p), floor(n*p) +1 )
][
  
  # On détermine l'enveloppe convexe
  chull(FDC)
]

convex.hull0.25 <- df[
  rank(FDC, ties.method = "min") < ifelse(floor(n*0.25), floor(n*0.25), floor(n*0.25) +1 )
][
  chull(FDC)
]

convex.hull0.5 <- df[
  rank(FDC, ties.method = "min") < ifelse(floor(n*0.5), floor(n*0.5), floor(n*0.5) +1 )
][
  chull(FDC)
]
convex.hull0.75 <- df[
  rank(FDC, ties.method = "min") < ifelse(floor(n*0.75), floor(n*0.75), floor(n*0.75) +1 )
][
  chull(FDC)
]
convex.hull0.9 <- df[
  rank(FDC, ties.method = "min") < ifelse(floor(n*0.9), floor(n*0.9), floor(n*0.9) +1 )
][
  chull(FDC)
]
convex.hull0.975 <- df[
  rank(FDC, ties.method = "min") < ifelse(floor(n*0.975), floor(n*0.975), floor(n*0.975) +1 )
][
  chull(FDC)
]



plot_ly(data = df, x= ~time, y = ~FDC, type = "scatter",
        marker = list(size = 4, color = ~tdepth),
        name = "Observed FDC"
) %>%
  layout(
    xaxis = ax,
    yaxis = ax,
    margin = list(pad = 20)
  ) %>% 
  add_polygons(x = convex.hull0.25[, time], 
               y = convex.hull0.25[, FDC], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#111613 ",
                             line = list(color = "black", width = 2)),
               line = list(color = "#111613"),
               name = '0.25 th Q. surface') %>% 
  add_polygons(x = convex.hull0.5[, time], 
               y = convex.hull0.5[, FDC], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#FFC300",
                             line = list(color = "black", width = 2)),
               line = list(color = "#FFC300"),
               name = '0.5 th Q. surface') %>% 
  add_polygons(x = convex.hull0.75[, time], 
               y = convex.hull0.75[, FDC], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#7D3C98",
                             line = list(color = "black", width = 2)),
               line = list(color = "#7D3C98"),
               name = '0.75 th Q. surface') %>% 
  add_polygons(x = convex.hull0.9[, time], 
               y = convex.hull0.9[, FDC], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#EE154A",
                             line = list(color = "black", width = 2)),
               line = list(color = "#EE154A"),
               name = '0.90 th Q. surface') %>% 
  add_polygons(x = convex.hull0.975[, time], 
               y = convex.hull0.975[, FDC],
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#15EE6E",
                             line = list(color = "black", width = 2)),
               line = list(color = "#15EE6E"),
               name = '0.975 th Q. surface') %>% 
  add_markers(x = df[which.max(tdepth), time], y = df[which.max(tdepth), FDC],
              marker = list(size = 10, color = "blue"),
              name = "Most depthest point")


#------------------- Dépth contour on résiduals ----------------------- #

# --- Modèle linéaire --- #

# variable dépendante : FDC
# variable indépendante : vind_sp_100

fdc <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, FDC)],
                         formula = id_prod ~ date, value.var = "FDC")

wind_sp <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, wind_sp_100)],
                             formula = id_prod ~ date, value.var = "wind_sp_100")


# --- Lissage des courbes avec des paramètres par defaut --- #

## Base de fourier
fb <- create.fourier.basis(rangeval = c(1, 365*24), nbasis =  1780)

## 1) Lissage
fdc.fd <- Data2fd(argvals = 1:(365*24), y = t(fdc[, -1]),
                  lambda = 10,
                  basisobj = fb, method="chol", dfscale=1)

wind_sp.fd <- Data2fd(argvals = 1:(365*24), y = t(wind_sp[, -1]),
                      lambda = 10,
                      basisobj = fb, method="chol", dfscale=1)

## Mettre l'id_parc des courbes nom des courbes
fdc.fd$fdnames$reps <- fdc[, id_prod]
wind_sp.fd$fdnames$reps <- wind_sp[, id_prod]

residus <- get_linear_model_resuals(y_curves = fdc.fd, x_curves = wind_sp.fd, y.ncp = 5, x.ncp = 5)


## Plot
id_parc <- unique(residus[, id_prod])

df <- data.table(id_prod = id_parc[1],
                 res = as.numeric(residus[id_prod %in% id_parc[1], .SD, .SDcols = !"id_prod"]))

## On centre et on réduit
## df[, res:= (res - mean(res))/sd(res)]

df <- df[, c("time", "tdepth", "sdepth") := .(0:8759, tukey_depth(res), simplicial_depth(res))]

convex.hull <- df[
  
  # On prend les n*p premier éléments
  rank(res, ties.method = "min") < ifelse(floor(n*p), floor(n*p), floor(n*p) +1 )
][
  
  # On détermine l'enveloppe convexe
  chull(res)
]

convex.hull0.25 <- df[
  rank(res, ties.method = "min") < ifelse(floor(n*0.25), floor(n*0.25), floor(n*0.25) +1 )
][
  chull(res)
]

convex.hull0.5 <- df[
  rank(res, ties.method = "min") < ifelse(floor(n*0.5), floor(n*0.5), floor(n*0.5) +1 )
][
  chull(res)
]
convex.hull0.75 <- df[
  rank(res, ties.method = "min") < ifelse(floor(n*0.75), floor(n*0.75), floor(n*0.75) +1 )
][
  chull(res)
]
convex.hull0.9 <- df[
  rank(res, ties.method = "min") < ifelse(floor(n*0.9), floor(n*0.9), floor(n*0.9) +1 )
][
  chull(res)
]
convex.hull0.975 <- df[
  rank(res, ties.method = "min") < ifelse(floor(n*0.975), floor(n*0.975), floor(n*0.975) +1 )
][
  chull(res)
]

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  showgrid = FALSE
)


plot_ly(data = df, x= ~time, y = ~res, type = "scatter",
        marker = list(size = 5, opacity = ~tdepth + 0.15),
        name = "Extracted résiduals"
)%>%
  layout(
    xaxis = ax,
    yaxis = ax,
    margin = list(pad = 20)
  ) %>% 
  add_polygons(x = convex.hull0.25[, time], 
               y = convex.hull0.25[, res], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#111613 ",
                             line = list(color = "black", width = 2)),
               line = list(color = "#111613", width = 2.5),
               name = '0.25 th Q. surface') %>% 
  add_polygons(x = convex.hull0.5[, time], 
               y = convex.hull0.5[, res], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#FFC300",
                             line = list(color = "black", width = 2)),
               line = list(color = "#FFC300", width = 3, dash = "dash"),
               name = '0.5 th Q. surface') %>% 
  add_polygons(x = convex.hull0.75[, time], 
               y = convex.hull0.75[, res], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#7D3C98",
                             line = list(color = "black", width = 2)),
               line = list(color = "#7D3C98", width = 3, dash = "dot"),
               name = '0.75 th Q. surface') %>% 
  add_polygons(x = convex.hull0.9[, time], 
               y = convex.hull0.9[, res], 
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#EE154A",
                             line = list(color = "black", width = 2)),
               line = list(color = "#EE154A", width = 3, dash = "dash"),
               name = '0.90 th Q. surface') %>% 
  add_polygons(x = convex.hull0.975[, time], 
               y = convex.hull0.975[, res],
               fillcolor='rgba(255, 0, 0, 0.0)',
               marker = list(size = 1, color = "#15EE6E",
                             line = list(color = "black", width = 2)),
               line = list(color = "#15EE6E", width = 3, dash = "dot"),
               name = '0.975 th Q. surface') %>% 
  add_markers(x = df[which.max(tdepth), time], y = df[which.max(tdepth), res],
              marker = list(size = 10, color = "red"),
              name = "Most depthest point")


# ------- Courbe de Charge ------------------------- #



# ------------- polygon by probability ------------- #



