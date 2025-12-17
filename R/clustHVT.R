#' @name clustHVT
#' @title Performing Hierarchical Clustering Analysis
#' @description This is the main function to perform hierarchical clustering
#' analysis which determines optimal number of clusters, perform AGNES clustering
#' and plot the 2D cluster hvt plot.
#' @param data Data frame. A data frame intended for performing hierarchical clustering analysis.
#' @param trainHVT_results List.  A list object which is obtained as a result of trainHVT function.
#' @param scoreHVT_results List. A list object which is obtained as a result of scoreHVT function.
#' @param clusters_k Character.  A parameter that specifies the number of clusters for the provided data. 
#' The options include “champion,” “challenger,” or any integer between 1 and 20. 
#' Selecting “champion” will use the highest number of clusters recommended by the ‘NbClust’ function,
#' while “challenger” will use the second-highest recommendation. If a numerical value from 1 to 20
#' is provided, that exact number will be used as the number of clusters.
#' @param indices Character. The indices used for determining the optimal number of clusters in NbClust function.
#'  By default it uses 20 different indices.
#' @param clustering_method Character. The method used for clustering in both NbClust and hclust function. Defaults to ‘ward.D2’.
#' @param type Character. The type of output required. Default is 'default'. Other option is 'plot' which
#'  will return only the clustered heatmap.
#' @param domains.column Character. A vector of cluster names for the clustered heatmap.
#' Used only when type is 'plot'.
#' @param highlight_labels Vector. Numeric vector specifying problematic states and their neighboring states to be highlighted in the dendrogram. 
#' This argument is intended for internal use only. The default value is NULL.
#' @param only_dendro Logical. A logical string specifies whether to generate only the dendrogram or to include the results as well.
#' This argument is intended for internal use only. The default value is FALSE.
#' @return A list object that contains the hierarchical clustering results.
#' \item{[[1]] }{Summary of k suggested by all indices with plots} 
#' \item{[[2]] }{A dendrogram plot with the selected number of clusters} 
#' \item{[[3]] }{A 2D Cluster HVT Plotly visualization that colors cells according to clusters derived from AGNES clustering results. 
#' It is interactive, allowing users to view cell contents by hovering over them}
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Clustering_Analysis
#' @include clusterPlot.R
#' @importFrom utils data head tail
#' @importFrom stats as.dendrogram median 
#' @examples 
#'data("EuStockMarkets")
#'dataset <- data.frame(t = as.numeric(time(EuStockMarkets)),
#'                      DAX = EuStockMarkets[, "DAX"],
#'                      SMI = EuStockMarkets[, "SMI"],
#'                      CAC = EuStockMarkets[, "CAC"],
#'                      FTSE = EuStockMarkets[, "FTSE"])
#'rownames(EuStockMarkets) <- dataset$t
#'hvt.results<- trainHVT(dataset[-1],n_cells = 30, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE,quant_method = "kmeans")
#'scoring <- scoreHVT(dataset, hvt.results, analysis.plots = TRUE, names.column = dataset[,1])
#'centroid_data <- scoring$centroidData
#'hclust_data_1 <- centroid_data[,2:3]
#'clust.results <- clustHVT(data = hclust_data_1, 
#'                          trainHVT_results = hvt.results,
#'                          scoreHVT_results = scoring, 
#'                          clusters_k = 'champion', indices = 'hartigan')
#' @export clustHVT




clustHVT <- function(data, trainHVT_results, scoreHVT_results, clustering_method = 'ward.D2',
                     indices = c("kl", "ch", "hartigan", "cindex", "db", "silhouette", 
                                 "ratkowsky", "ball", "hubert", "dindex", "ptbiserial", 
                                 "gap", "frey", "mcclain", "gamma", "gplus", "tau", 
                                 "dunn", "sdindex", "sdbw"), 
                     clusters_k = "champion", type = "default", domains.column = NULL, highlight_labels = NULL, only_dendro = FALSE) {
  requireNamespace('NbClust')
  
  if (type == "plot"){
    
    if (is.null(data) || is.null(trainHVT_results) || is.null(domains.column)) {
      stop("For type 'plot',  the arguments `data`, `trainHVT_results` and `domains.column` are required.")
    }
    
    plot_a <- clusterPlot(dataset= data, hvt.results  = trainHVT_results, domains.column = domains.column )
    
    return(plot_a)
    
  } 
  else if (type == "default"){
    
  
  hclust_data <- data
  
  results_df <- c()
  
  
  # Function to run NbClust and extract results
  print_nbclust_results <- function(hclust_data, indices) {
    get_nbclust_result <- function(index) {
      res <- NbClust::NbClust(hclust_data, method = clustering_method, index = index)$Best.nc
      if (!is.null(res)) {
        return(data.frame(Index = index, 
                          Number_clusters = res["Number_clusters"], 
                          Value_Index = res["Value_Index"]))
      } else {
        return(data.frame(Index = index, 
                          Number_clusters = NA, 
                          Value_Index = NA))
      }
    }
    
    results_list <- lapply(indices, get_nbclust_result)
    results_df <<- do.call(rbind, results_list)
    
    cluster_summary <- table(results_df$Number_clusters)
    cat("** Among all indices:\n")
    lapply(seq_along(cluster_summary), function(i) {
      cat(sprintf("** * %d proposed %d as the best number of clusters\n", 
                  cluster_summary[i], as.numeric(names(cluster_summary)[i])))})
    
    
    best_clusters <- as.numeric(names(cluster_summary)[which.max(cluster_summary)])
    cat("*******************************************************************\n")
    cat("***** Conclusion *****\n")
    cat(sprintf("** * According to the majority rule, the best number of clusters is %d\n", best_clusters))
    cat("*******************************************************************\n")
    cat("                         Index     Number_clusters    Value_Index\n")
    cat("-------------------------------------------------------------------\n")
    apply(results_df, 1, function(row) {
      cat(sprintf("%24s  %15.4f  %15.4f\n", 
                  row["Index"], 
                  as.numeric(row["Number_clusters"]), 
                  as.numeric(row["Value_Index"])))
    })
    cat("*******************************************************************\n")
  }
  
  
  # Decide whether to run NbClust
  use_nbclust <- !is.null(indices) && length(indices) > 0 && clusters_k %in% c("champion","challenger")
  if (use_nbclust) {
    # If caller passed indices, use them; otherwise default set
    if (identical(indices, TRUE)) {
      indices <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette", "ratkowsky", "ball","hubert","dindex",
                   "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex", "sdbw")
    }
    print_nbclust_results(hclust_data, indices)
    cluster_summary <- table(results_df$Number_clusters)
    if (clusters_k == "champion") {
      no_of_clusters <- as.numeric(names(cluster_summary)[which.max(cluster_summary)])
    } else { # challenger
      sorted_summary <- sort(cluster_summary, decreasing = TRUE)
      no_of_clusters <- as.numeric(names(sorted_summary)[2])
    }
  } else {
    # Bypass NbClust and honor explicit numeric clusters_k
    if (is.numeric(clusters_k) && clusters_k >= 1 && clusters_k <= 20) {
      no_of_clusters <- as.integer(clusters_k)
    } else if (clusters_k %in% c("champion","challenger")) {
      stop("indices must be supplied to use 'champion' or 'challenger'")
    } else {
      stop("Invalid input for clusters_k. Use numeric 1..20, or provide indices with 'champion'/'challenger'.")
    }
  }
  # Ensure k is a single integer and within allowable range for this data
  no_of_clusters <- as.integer(no_of_clusters[1])
  n_items <- nrow(hclust_data)
  if (is.na(no_of_clusters) || n_items < 2) {
    stop("Clustering error: insufficient data for clustering")
  }
  # cutree requires 1 < k <= n_items; in practice k must be in [1, n_items]
  if (no_of_clusters < 1 || no_of_clusters > n_items) {
    stop(sprintf("Clustering error: k=%d out of range [1..%d]", no_of_clusters, n_items))
  }
  
  #########cell_id#####
  hvt_list <- trainHVT_results
  hvt_res1 <- hvt_list[[2]][[1]]$`1`
  hvt_res2 <- hvt_list[[3]]$summary$Cell.ID
  a <- seq_along(hvt_res1)
  b <- a[hvt_res2]
  b <- as.vector(b)
  hvt_res2 <- stats::na.omit(b)
  hclust_data_1 <- hclust_data
  rownames(hclust_data_1)<- hvt_res2

#browser()  
  
  # Perform hierarchical clustering
  hc <- stats::hclust(stats::dist(as.matrix(hclust_data_1)), method = clustering_method)
  clusters <- stats::cutree(hc, k = no_of_clusters)
  
# Replace the existing plot_dendrogram function with this:
  # plot_dendrogram <- function(hc_1, no_of_clusters_1) {
  #   function() {
  #     plot(hc_1, xlab = "Clusters", ylab = "Distance", sub ="")
  #     stats::rect.hclust(hc_1, k = no_of_clusters_1, border = grDevices::rainbow(no_of_clusters_1))
  #   }
  # }
  
  if(only_dendro){
    plot_dendrogram <- function(hc_1, no_of_clusters_1, highlight_labels = NULL) {
      
      function() {
        dend <- as.dendrogram(hc_1)
        
        if (!is.null(highlight_labels)) {
          labels_colors <- ifelse(labels(dend) %in% highlight_labels, "red", "black")
          dend <- dendextend::set(dend, "labels_col", labels_colors)
        }
        dend <- dendextend::set(dend, "labels_cex", 1)
        plot(dend, ylab = "Distance", xlab = "Clusters", leaflab = "perpendicular")
        dendextend::rect.dendrogram(dend, k = no_of_clusters_1, 
                                    border = grDevices::rainbow(no_of_clusters_1))
      }
    }
    
    
    a <- plot_dendrogram(hc_1 = hc,no_of_clusters_1 = no_of_clusters,highlight_labels = highlight_labels)
    
    output_list <- list(dendrogram = a)
    return(output_list)
  }else {
    plot_dendrogram <- function(hc_1, no_of_clusters_1, highlight_labels = NULL) {
      
      function() {
        dend <- as.dendrogram(hc_1)
        dend <- dendextend::set(dend, "labels_cex", 1)
        plot(dend, ylab = "Distance", xlab = "Clusters", leaflab = "perpendicular")
        dendextend::rect.dendrogram(dend, k = no_of_clusters_1, 
                                    border = grDevices::rainbow(no_of_clusters_1))
      }
    }
    
    
    
    a <- plot_dendrogram(hc_1 = hc,no_of_clusters_1 = no_of_clusters,highlight_labels = highlight_labels)
    
    
    # Prepare data for clusterPlotly
    cluster_data <- scoreHVT_results$centroidData %>% 
      dplyr::select("Cell.ID", "names.column") %>%
      mutate(clusters =  clusters)
    
    cluster_data <- cluster_data[order(cluster_data$Cell.ID), ]
    #browser()  
    
    b <- clusterPlot(dataset= cluster_data, hvt.results  = trainHVT_results, domains.column = "clusters" )
    
    output_list <- list(
      hc = hc,
      clusters = clusters,
      cluster_data =cluster_data,
      dendrogram = a,
      clusterplot = b
    )
    
    return(output_list)
  }
  }
}