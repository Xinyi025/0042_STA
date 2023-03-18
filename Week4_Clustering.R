# 4.1 K-means

mykmeans <- function(data, k, max.iter=10)
{
  # A function for carrying out k-means clustering
  # Inputs:
  #  data: data matrix/data frame with n rows and p columns, where n
  #  is the number of observations and p the number of variables 
  #  k: the number of clusters that are sought
  #  max.iter: the number of iterations before stopping the algorithm
  # Outputs:
  #  centres: the coordinates of the cluster centres
  #  clusters: the cluster that each data point is assigned to.
  
  require(fields)
  # randomly select centres
  centres <- data[sample(1:nrow(data), k),] 
  # create empty matrix to store distance calculations
  dists <- matrix(0, nrow(data), k+1) 
  for (i in 1:max.iter)
  {
    for(j in 1:k)
    {
      # Calculate Euclidean distance from points to cluster centres
      dists[,j] <- rdist(data, centres[j,,drop=F]) 
    }
    # Find nearest centre for each data point (minimum distance)
    dists[,k+1] <- apply(dists[,c(1:k)], 1, which.min) 
    # recalculate centres as mean of points contained in each cluster
    centres <- aggregate(data, list(dists[,k+1]), mean)[,-1]
  }
  return(list(centres=centres, clusters=dists[,k+1]))
}