if (! require('fda', quietly = TRUE)){ install.packages('fda') } ; library('fda', quietly = TRUE)
if (! require('data.table', quietly = TRUE)){ install.packages('data.table') } ; library('data.table', quietly = TRUE)


get_linear_model_resuals <- function(y_curves = fdobject, x_curves = fdobject, y.ncp = 5, x.ncp = 5){
  
  # 1.1) On centre les courbes
  y_curves.c = center.fd(y_curves)
  x_curves.c = center.fd(x_curves)
  
  # 1.2) On fait l'analyse en composantes principales fonctionnelles
  y_curves.pca <- pca.fd(y_curves.c, nharm = y.ncp)
  x_curves.pca <- pca.fd(x_curves.c, nharm = x.ncp)
  
  # 2) Calcul des coefficients de fonction psi
  sigmas = t(y_curves.pca$scores) %*% x_curves.pca$scores
  cs = diag(1/x_curves.pca$values[1:x.ncp]) %*% sigmas
  
  # 3.1) Construction la fonction psi dans la base des fonctions propres
  psi.coefs = x_curves.pca$harmonics$coefs%*%cs%*%t(y_curves.pca$harmonics$coefs)
  
  psi.bifd = bifd(coef = psi.coefs, 
                  sbasisobj = x_curves.pca$harmonics$basis,
                  tbasisobj = y_curves.pca$harmonics$basis)
  
  # 3.2) On évalue la fonction aux instants définies dans l'objet bifd
  psi.bifd.mat <- eval.bifd(sevalarg = seq(from = x_curves$basis$rangeval[1], 
                                           to = x_curves$basis$rangeval[2], 
                                           len = x_curves$basis$params + 1),
                            tevalarg = seq(from = y_curves$basis$rangeval[1], 
                                           to = y_curves$basis$rangeval[2], 
                                           len = y_curves$basis$params + 1), 
                            bifd = psi.bifd)
  
  # 4) Expression de y_curves et de x_curves dans la base des fonctions propres
  
  # 4.1.1) construction de l'objet fd
  y_curves.coef <- y_curves.pca$harmonics$coefs %*% t(y_curves.pca$scores)
  y_curves.hfd <- fd(coef = y_curves.coef, basisobj = y_curves.pca$harmonics$basis)
  
  # 4.1.2) On évalue la fonction aux instants définies dans l'objet fd
  y_curves.mat <- eval.fd(evalarg = seq(from = x_curves$basis$rangeval[1], 
                                        to = x_curves$basis$rangeval[2], 
                                        len = x_curves$basis$params + 1),
                          fdobj = y_curves.hfd)
  
  # 4.2.1) construction de l'objet fd
  x_curves.coef <- x_curves.pca$harmonics$coefs %*% t(x_curves.pca$scores)
  x_curves.hfd <- fd(coef = x_curves.coef, basisobj = x_curves.pca$harmonics$basis)
  
  # 4.2.2) On évalue la fonction aux instants définies dans l'objet fd
  x_curves.mat <- eval.fd(evalarg = seq(from = y_curves$basis$rangeval[1], 
                                        to = y_curves$basis$rangeval[2], 
                                        len = y_curves$basis$params + 1),
                          fdobj = x_curves.hfd)
  
  # 5) Extraction des résidus... eps(t) = Y(t) - \int psi(s,t) X(s) ds
  
  eps.mat <- t(y_curves.mat) - t(x_curves.mat) %*% psi.bifd.mat
  eps.mat <- cbind(id_prod = y_curves$fdnames$reps, eps.mat) %>% as.data.table()
  
  # 6) Test de validé du modèle linéaire
  
  #    H_0 : \psi(.,.) = 0 vs H_1 : non H_0
  
  # Biblio : voir kokozka and Reimherr - Intro to fda
  #         algorithm 6.5.1
  
  
  # 6.1) Number of curves
  N <- length(y_curves.pca$scores[,1])
  
  # 6.2) Cacul de statistique de test
  delta <- t(y_curves.pca$scores) %*% x_curves.pca$scores / N
  
  stat_test <- N * t(1/y_curves.pca$values[1:y.ncp]) %*% ( delta^2 ) %*% (1/x_curves.pca$values[1:x.ncp])
  
  # Print test
  cat("----------------------- Test of no effect-----------------------\n")
  cat("Null Hypothesis : bivariate functional coefficient égal to zéro\n\n")
  cat("Test statistic : ", stat_test, "\n")
  cat("Degrees of Freedom : ", x.ncp*y.ncp, "\n")
  cat("Chi-squared 0.975 quantile : ", qchisq(p = .975, df = x.ncp*y.ncp) %>% round(digits = 3), "\n")
  cat("Chi-squared 0.999 quantile : ", qchisq(p = .900, df = x.ncp*y.ncp) %>% round(digits = 3), "\n")
  cat("p-value : ", 1 - pchisq(stat_test, df = x.ncp*y.ncp) %>% round(digits = 3), "\n")
  cat("----------------------------------------------------------------")
  
  # 7) On return les résidus
  return(eps.mat) 
}
