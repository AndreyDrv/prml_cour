
detectOutliers <- function(df, accuracy) {

  #1. convert to frequency martix m
  
  
  # calculate step(width) for grid cell
  hX <- (max(df[,1])-min(df[,1]))/accuracy
  hY <- (max(df[,2])-min(df[,2]))/accuracy
  
  # calculate bounds for the matrix (grid cell bounds to fit data into)
  xGrid <- vector()
  yGrid <- vector()
  for (i in 1:accuracy)
  {
    xGrid[i] <- min(df[,1])+(i-1)*hX  
    yGrid[i] <- min(df[,2])+(i-1)*hY  
  }  

  # calculate frequency of each point in each grid cell
  freqGrid <- matrix(ncol=accuracy, nrow=accuracy)
  
  for (i in 1:accuracy)
  {
    for (j in 1:accuracy)
    {
      currentBounds <- c(x1=xGrid[i],
                         x2=xGrid[i]+hX,
                         y1=yGrid[j],
                         y2=yGrid[j]+hY)
      
      freqCurrent <- length(
        df[df[1]>currentBounds["x1"] & 
           df[1]<currentBounds["x2"] & 
           df[2]>currentBounds["y1"] & 
           df[2]<currentBounds["y2"]
           ]
        )
      
      freqGrid[i,j] <- freqCurrent
    }
  }#!! wrong result (to much values ) -> fix and refactor
  
  
  #2. divide into clusters
  
  # make a 1-dimenson vector from the freq matrix
  freqGridVector <- as.vector(freqGrid)
  
  #!!! remove zero values?? to avoid 0 as separate cluster
  
  # logarithmize the vector to point out a bigger(more important) numbers in cluster
  logd <- log(freqGridVector)
  logd[is.infinite(logd)] <- 0
  
  #apply k-means for vector to find 2 clusters in vector
  idx = kmeans(logd,2)
  
  # plot how kmeans clustered the points
  plot(idx$cluster, freqGridVector)
  
  
  # make matrix back to see the cluster number at each point spot
  freqGridClusters <- matrix(idx$cluster,nrow = accuracy,ncol = accuracy)
  
  
  #3. calculate cell index for each point in freqGridClusters
  
  #4. determmine which cluster consists of outliers
  
  
  
  #5. plot data and mark outliers
  
  
  
  
  
  
  
}


cdata <- mtcars$hp
detectOutliers(cdata, 10)





# function  [mlIn mrIn] = Outliers(ml, mr, h);
# 
# % ml, mr - corresponding points on left and right pics
# % h - number of cells on the plain
# 
# m = (ml'-mr');
# 
# %% create grid
# hx = (max(m(:,1))-min(m(:,1)))/h;
# hy = (max(m(:,2))-min(m(:,2)))/h;
# 
# for t=1:(h)
# xgrid(t) = min(m(:,1))+(t-1)*hx;
# end
# 
# for t=1:(h)
# ygrid(t) = min(m(:,2))+(t-1)*hy;
# end
# 
# edges{1} = xgrid;
# edges{2} = ygrid;
# 
# %% Calculate histogram
# N = hist3(m,'Edges',edges);
# 
# %% Divide into clusters
# N1 = reshape(N,h*h,1)+1;
# idx = kmeans(log(N1),2);
# idx = reshape(idx,h,h);
# 
# %% calculate cell index for each point in m
# for t=1:h
# for i=1:size(m,1)
# if (floor(min(m(:,1))+(t-1)*hx) <= m(i,1))&(m(i,1) <= ceil(min(m(:,1))+t*hx))
# x(i) = t;
# end
# if (floor(min(m(:,2))+(t-1)*hy) <= m(i,2))&(m(i,2) <= ceil(min(m(:,2))+t*hy))
# y(i) = t;   
# end
# end
# end
# 
# id = [x' y'];
# 
# %% determmine which cluster consists of outliers
# 
# %% mark outliers with 0
# mlIn = zeros(2,size(ml,2));
# mrIn = zeros(2,size(mr,2));
# 
# for i=1:size(m,1)
# if idx(id(i,1),id(i,2))==1
# n(i)=1;
# else
#   n(i)=2;
# mlIn(:,i) = ml(:,i);
# mrIn(:,i) = mr(:,i);
# end
# end
# 
# %% delete outliers
# mlIn(:,mlIn(1,:)==0)=[];
# mrIn(:,mrIn(1,:)==0)=[];
# 
# %% Plotting pictures
# 
# % figure;
# % scatter(m(:,1),m(:,2))
# % axis equal
# 
# % figure;
# % hist3(m,'Edges',edges);
# 
# % figure;
# % scatter(sort(N1),zeros(1,h*h)+1);
# % figure;
# % scatter(log(sort(N1)),zeros(1,h*h)+1);
# 
# figure;
# plot(m(n==1,1),m(n==1,2),'r.','MarkerSize',12)
# hold on
# plot(m(n==2,1),m(n==2,2),'b.','MarkerSize',12)
# hold off
