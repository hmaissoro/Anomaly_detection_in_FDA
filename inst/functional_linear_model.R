if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)
if (! require('fst', quietly = TRUE)){ install.packages('fst') } ; library('fst', quietly = TRUE)
if (! require('magrittr', quietly = TRUE)){ install.packages('magrittr') } ; library('magrittr', quietly = TRUE)
if (! require('dplyr', quietly = TRUE)){ install.packages('dplyr') } ; library('dplyr', quietly = TRUE)
if (! require('fda', quietly = TRUE)){ install.packages('fda') } ; library('fda', quietly = TRUE)

dt_fdc_ws <- readRDS("/home/hmaissoro/Stage/functionnal_data_analysis/anomaly_detection/data/dt_fdc_ws.rds")


id_parc <- unique(dt_fdc_ws[, id_prod])

# NB : Le lien n'est pas linéaire
plot(x = dt_fdc_ws[id_prod %in% id_parc[1], wind_sp_100], 
     y = dt_fdc_ws[id_prod %in% id_parc[1], FDC])
plot(x = dt_fdc_ws[id_prod %in% id_parc[2], wind_sp_100], 
     y = dt_fdc_ws[id_prod %in% id_parc[2], FDC])

# --- Modèle linéaire --- #

# variable dépendante : FDC
# variable indépendante : vind_sp_100

fdc <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, FDC)],
                         formula = id_prod ~ date, value.var = "FDC")

wind_sp <- data.table::dcast(data = dt_fdc_ws[, .(id_prod, date, wind_sp_100)],
                         formula = id_prod ~ date, value.var = "wind_sp_100")


# --- Lissage des courbes avec des paramètres par defaut --- #

# Base de fourier
fb <- create.fourier.basis(rangeval = c(1, 365*24), nbasis =  1780)

# 1) Lissage
fdc.fd <- Data2fd(argvals = 1:(365*24), y = t(fdc[, -1]),
                        lambda = 10,
                        basisobj = fb, method="chol", dfscale=1)

wind_sp.fd <- Data2fd(argvals = 1:(365*24), y = t(wind_sp[, -1]),
                  lambda = 10,
                  basisobj = fb, method="chol", dfscale=1)

# Mettre l'id_parc des courbes nom des courbes
fdc.fd$fdnames$reps <- fdc[, id_prod]
wind_sp.fd$fdnames$reps <- wind_sp[, id_prod]

# 2) Analyse en composante principal

## On centre les courbes
fdc.fd.c = center.fd(fdc.fd)
wind_sp.fd.c = center.fd(wind_sp.fd)

## FPCA
fdc.pca <- pca.fd(fdc.fd.c, nharm = 5)
wind_sp.pca <- pca.fd(wind_sp.fd.c, nharm = 5)

## Plot des variances
plot(fdc.pca$varprop, type = "o")
plot(wind_sp.pca$varprop, type = "o")

# NB : Necessité de faire un choix optimal pour les paramètres de lissage de wind_sp

# 3) Calcul des coefficients
sigmas = t(fdc.pca$scores) %*% wind_sp.pca$scores
cs = diag(1/wind_sp.pca$values[1:5]) %*% sigmas

# 4) Objet bivariate fonctional data
psi.coefs = wind_sp.pca$harmonics$coefs%*%cs%*%t(fdc.pca$harmonics$coefs)

psi.bifd = bifd(coef = psi.coefs, 
                sbasisobj = wind_sp.pca$harmonics$basis,
                tbasisobj = fdc.pca$harmonics$basis)

## Evaluation de \psi à des temps discrets
psi.bifd.mat <- eval.bifd(sevalarg = 1:(365*24), tevalarg = 1:(365*24), bifd = psi.bifd)

# 5) Expression de fdc et de wind_sp dans leur bases de composantes principales

# fdc
dim(fdc.pca$scores)
fdc.coef <- fdc.pca$harmonics$coefs %*% t(fdc.pca$scores)
fdc.hfd <- fd(coef = fdc.coef, basisobj = fdc.pca$harmonics$basis)

fdc.mat <- eval.fd(evalarg = 1:(365*24), fdobj = fdc.hfd)

# wind_sp
wind_sp.coef <- wind_sp.pca$harmonics$coefs %*% t(wind_sp.pca$scores)
wind_sp.hfd <- fd(coef = wind_sp.coef, basisobj = wind_sp.pca$harmonics$basis)
wind_sp.mat <- eval.fd(evalarg = 1:(365*24), fdobj = wind_sp.hfd)

# 5) Extraction des résidus... Y(t) = \int (\psi(s,t)X(s)ds) + eps(t)
dim(fdc.mat)
dim(wind_sp.mat)
dim(psi.bifd.mat)

# psi(s,t)
eps.mat <- t(fdc.mat) - t(wind_sp.mat) %*% psi.bifd.mat

# ---- Test de la fonction --- #

# residus <- get_linear_model_resuals(y_curves = fdc.fd, x_curves = wind_sp.fd, y.ncp = 5, x.ncp = 5)

# --- Quelques plots sur la regression --- #

# Courbes vraies
plot(x = dt_fdc_ws[id_prod %in% id_parc[1], wind_sp_100], 
     y = dt_fdc_ws[id_prod %in% id_parc[1], FDC])
# Courbes estimées dans les bases de fonctions
plot(x = wind_sp.mat[,1], y = fdc.mat[,1])

#
plot.ts(dt_fdc_ws[id_prod %in% id_parc[1], wind_sp_100])

# Faible inertie sur les premières composantes principales
plot.ts(wind_sp.mat[1,])

# Forte inertie dans les derniers composantes principales
plot.ts(dt_fdc_ws[id_prod %in% id_parc[1], FDC])

# Question : Est-ce que le fait d'avoir plus inertie sur les premiers axes 
#            modifie fortement la structure de la courbe reconstruite ?

# ---- Remarque -----#
# Le problème est de la representation est dû 
# au fait qu'on a centré les données

plot.ts(wind_sp.mat[,1], fdc.mat[,1])

# -----  --- #
fdc.meanfd <- mean.fd(fdc.fd)
wind_sp.meanfd <- mean.fd(wind_sp.fd)

# NB : Ajouter la noyenne courbe par courbe

## On prend une courbe fdc
fdc.hfd.1 <- fdc.meanfd + fdc.hfd[1]
wind_sp.hfd.1 <- wind_sp.meanfd + wind_sp.hfd[1]

## On évalue les courbes à des instants discrets
wind_sp.1 <- eval.fd(evalarg = 1:(365*24), fdobj = wind_sp.hfd.1)

fdc.1 <- eval.fd(evalarg = 1:(365*24), fdobj = fdc.hfd.1)

## On plot

#### Courbes vraies
plot(x = dt_fdc_ws[id_prod %in% id_parc[1], wind_sp_100], 
     y = dt_fdc_ws[id_prod %in% id_parc[1], FDC])

#### Courbes réconstruites sans la noyenne
plot(x = wind_sp.mat[,1], y = fdc.mat[,1])

#### Courbes réconstruites avec la noyenne
plot(wind_sp.1, fdc.1)

## Conclusion : On pert un peu en information !




# ------ Test of no 'linear' effect ------ # 
# Biblio : voir kokozka and Reimherr - Intro to fda
#         algorithm 6.5.1

## Step 1 : Select number of fpc
            # Already done

## Step 2 : Center fdc and wind_speed
            # Already done

## Step 3 : Compute the statistics

## Number of curves
N <- length(fdc.pca$scores[,1])

## Cacul de delta
delta <- t(fdc.pca$scores) %*% wind_sp.pca$scores / N


stat_pf_test <- N * t(1/fdc.pca$values[1:5]) %*% ( delta^2 ) %*% (1/wind_sp.pca$values[1:5])

qchisq(p = .975, df = 5*5)

1 - pchisq(stat_pf_test, df = 5*5)


test <- data.table("Test statistic" = stat_pf_test,
                   "df" = 5*5,
                   "qchisq 0.975" = qchisq(p = .975, df = 5*5) %>% round(digits = 3),
                   "qchisq 0.999" = qchisq(p = .900, df = 5*5) %>% round(digits = 3),
                   "p-value" = 1 - pchisq(stat_pf_test, df = 5*5) %>% round(digits = 3)
                   )
# Print test
cat("-----------------------------------------------------------\n\n")
cat("Test statistic : ", stat_pf_test, "\n")
cat("Degrees of Freedom : ", 5*5, "\n")
cat("Chi-squared 0.975 quantile \n", qchisq(p = .975, df = 5*5) %>% round(digits = 3), "\n")
cat("Chi-squared 0.999 quantile \n", qchisq(p = .900, df = 5*5) %>% round(digits = 3), "\n")
cat("p-value : ", format(1 - pchisq(stat_pf_test, df = 5*5), nsmall=3), "\n\n")
cat("-----------------------------------------------------------")


