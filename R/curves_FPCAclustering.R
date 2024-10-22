# Curve clustering on FCP score
# Arguments : smooth_curves : An object of the fd class containing
#             ncp : Number of functional principal components
#             ncluster : Number of cluster
#             clustering_method : Clustering méthod tabke "kmeans" pour K-means or 
#                                 "hclust" pour Agglomerative Hiera. Clustering

# Packages
if (! require('fda', quietly = TRUE)){install.packages('fda')} ; library('fda', warn.conflicts = FALSE)
if (! require('data.table', quietly = TRUE)){install.packages('data.table')} ; library('data.table', warn.conflicts = FALSE)

curves_FPCAclustering <- function(smooth_curves = fdobject,
                                 ncp = 10,
                                 clustering_method = "kmeans",
                                 ncluster = 2){
  
  # --- 1) On verifie la class de l'objet
  if(! is.fd(smooth_curves)){
    
    return(warning("Given data is not of class fd. See 'Data2fd' function in 'fda' package."))
  
  }else if(! clustering_method %in% c("kmeans", "hclust")){
      return(warning("Invalide clustering method name"))
      
  }else{
      # --- 2) FPCA computing
      res.fpca <- pca.fd(fdobj = smooth_curves, nharm = ncp, centerfns = TRUE)
    
      # --- 3) Compute euclidian distance on FPCA score matrix
      dist.fpca <- dist(res.fpca$scores, method = "euclidean")
    
      # --- 3.1) K-means
      if(clustering_method == "kmeans"){
      
        # Compute Kmeans clustering
        res.kmeans <- kmeans(dist.fpca, centers = ncluster, iter.max = 500)
        
        # config output as data.table object
        kmeans_cluster <- data.table(id_prod = smooth_curves$fdnames$reps,
                                     cluster_index = res.kmeans$cluster)
        
        # Return cluster index
        return(kmeans_cluster)
        
      # --- 3.2) hclust
    }else if(clustering_method == "hclust"){
      
        # Compute hclust clustering
        # Hierarchical aggregation method = "ward.D2"
        
        # --- Full dendro
        res.hclust <- hclust(dist.fpca, method = "ward.D2" )
        
        # --- Cut
        cah.cluster <- cutree(res.hclust, k = ncluster)
        
        # config output as data.table object
        cah_cluster <- data.table(id_prod = smooth_curves$fdnames$reps,
                                     cluster_index = cah.cluster)
        
        # Return cluster index
        return(cah_cluster)
    }
  }
}

