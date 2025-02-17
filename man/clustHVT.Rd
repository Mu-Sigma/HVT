% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustHVT.R
\name{clustHVT}
\alias{clustHVT}
\title{Performing Hierarchical Clustering Analysis}
\usage{
clustHVT(
  data,
  trainHVT_results,
  scoreHVT_results,
  clustering_method = "ward.D2",
  indices,
  clusters_k = "champion",
  type = "default",
  domains.column
)
}
\arguments{
\item{data}{Data frame. A data frame intended for performing hierarchical clustering analysis.}

\item{trainHVT_results}{List.  A list object which is obtained as a result of trainHVT function.}

\item{scoreHVT_results}{List. A list object which is obtained as a result of scoreHVT function.}

\item{clustering_method}{Character. The method used for clustering in both NbClust and hclust function. Defaults to ‘ward.D2’.}

\item{indices}{Character. The indices used for determining the optimal number of clusters in NbClust function.
By default it uses 20 different indices.}

\item{clusters_k}{Character.  A parameter that specifies the number of clusters for the provided data. 
The options include “champion,” “challenger,” or any integer between 1 and 20. 
Selecting “champion” will use the highest number of clusters recommended by the ‘NbClust’ function,
while “challenger” will use the second-highest recommendation. If a numerical value from 1 to 20
is provided, that exact number will be used as the number of clusters.}

\item{type}{Character. The type of output required. Default is 'default'. Other option is 'plot' which
will return only the clustered heatmap.}

\item{domains.column}{Character. A vector of cluster names for the clustered heatmap.
Used only when type is 'plot'.}
}
\value{
A list object that contains the hierarchical clustering results.
\item{[[1]] }{Summary of k suggested by all indices with plots} 
\item{[[2]] }{A dendogram plot with the selected number of clusters} 
\item{[[3]] }{A 2D Cluster HVT Plotly visualization that colors cells according to clusters derived from AGNES clustering results. 
It is interactive, allowing users to view cell contents by hovering over them}
}
\description{
This is the main function to perform hierarchical clustering
analysis which determines optimal number of clusters, perform AGNES clustering
and plot the 2D cluster hvt plot.
}
\examples{
data("EuStockMarkets")
dataset <- data.frame(t = as.numeric(time(EuStockMarkets)),
                     DAX = EuStockMarkets[, "DAX"],
                     SMI = EuStockMarkets[, "SMI"],
                     CAC = EuStockMarkets[, "CAC"],
                     FTSE = EuStockMarkets[, "FTSE"])
rownames(EuStockMarkets) <- dataset$t
hvt.results<- trainHVT(dataset[-1],n_cells = 30, depth = 1, quant.err = 0.1,
                      distance_metric = "L1_Norm", error_metric = "max",
                      normalize = TRUE,quant_method = "kmeans")
scoring <- scoreHVT(dataset, hvt.results, analysis.plots = TRUE, names.column = dataset[,1])
centroid_data <- scoring$centroidData
hclust_data_1 <- centroid_data[,2:3]
clust.results <- clustHVT(data = hclust_data_1, 
                         trainHVT_results = hvt.results,
                         scoreHVT_results = scoring, 
                         clusters_k = 'champion', indices = 'hartigan')
}
\author{
Vishwavani <vishwavani@mu-sigma.com>
}
\keyword{Clustering_Analysis}
