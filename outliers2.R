plotGridCell <- function(currentBounds) {
  abline(v=currentBounds["x1"], col=3, lwd=1.5)
  abline(v=currentBounds["x2"], col=3, lwd=1.5)
  abline(h=currentBounds["y1"], col=3, lwd=1.5)
  abline(h=currentBounds["y2"], col=3, lwd=1.5)
}

cell2points <- function(i,j, xGrid, yGrid, hX, hY, df) {
  currentBounds <- c(x1=xGrid[i],
                     x2=xGrid[i]+hX,
                     y1=yGrid[j],
                     y2=yGrid[j]+hY)
  
  plotGridCell(currentBounds)
  
  df[df[1]>=currentBounds["x1"] & 
         df[1]<currentBounds["x2"] & 
         df[2]>=currentBounds["y1"] & 
         df[2]<currentBounds["y2"]
       ,]
}

getPointsFreqInCell <- function(i,j, xGrid, yGrid, hX, hY, df) {
  nrow(cell2points(i,j, xGrid, yGrid, hX, hY, df))
}

# pointtocell <- function() {
#   
# }

clearOutliers <- function(df, accuracy) {

  #1. convert to frequency martix m

  # calculate step(width) for grid cell
  hX <- (max(df[,1])-min(df[,1]))/accuracy
  hY <- (max(df[,2])-min(df[,2]))/accuracy
  
  # calculate bounds for the matrix (grid cell bounds to fit data into)
  xGrid <- vector()
  yGrid <- vector()
  for (i in 1:(accuracy+1))
  {
    xGrid[i] <- min(df[,1])+(i-1)*hX  
    yGrid[i] <- min(df[,2])+(i-1)*hY  
  }

  # calculate frequency of each point in each grid cell
  freqGrid <- matrix(ncol=(accuracy+1), nrow=(accuracy+1))
  
  for (i in 1:length(xGrid))
  {
    for (j in 1:length(yGrid))
    {
      freqCurrent <- getPointsFreqInCell(i,j, xGrid, yGrid, hX, hY, df)
      freqGrid[i,j] <- freqCurrent
    }
  }#!! wrong result -> fix
  
  
  #2. divide into clusters
  
  # make a 1-dimenson vector from the freq matrix
  freqGridVector <- as.vector(freqGrid + 1)
  
  #!!!TODO: Convert vector of all values to vector of 'alias' values:
  #Ex: 1,2,1,2,3 -> 1,2,3
  
  # logarithmize the vector to point out a bigger(more important) numbers in cluster
  logd <- log(freqGridVector)
  logd[is.infinite(logd)] <- 0  
    
#   plot(freqGridVector)
#   plot(logd)
  
  #apply k-means for vector to find 2 clusters in vector
  idx = kmeans(unique(sqrt(logd+1)),2)
#   print(idx$cluster)
  # plot how kmeans clustered the points
#   plot(idx$cluster, freqGridVector)
  
  # make matrix back to see the cluster number at each point spot
  freqGridClusters <- matrix(idx$cluster,nrow = (accuracy+1),ncol = (accuracy+1))

#   levelplot(freqGrid)
#   levelplot(freqGridClusters)
  
  
  #3. calculate cell index for each point in freqGridClusters
  result_coords <- which(freqGrid != (freqGridClusters == 2), TRUE)
  
  #4. determmine which cluster consists of outliers
  result <- df
  result <- result[is.na(result[1]),]
  
  for (i in 1:nrow(result_coords)) {    
   result <- rbind(result,
     cell2points(result_coords[i,][1], result_coords[i,][2], xGrid, yGrid, hX, hY, df))
  }
  colnames(result) <- colnames(df)
      
  result
}


cdata <- mtcars[,c('hp','wt')]
#plot given data
plot(cdata, col=2, pch = 3)

cdata_wo_outliers <- clearOutliers(cdata, 15)
#5. mark outliers
points(cdata_wo_outliers, col=4, pch=15)
nrow(cdata_wo_outliers)




### Unit test: output value statistics
##
# res <- vector(); for(i in 1:100) { res <- c(res, (source('~/.active-rstudio-document', echo=TRUE))$value) }
# sum(res==31) / length(res) *100

