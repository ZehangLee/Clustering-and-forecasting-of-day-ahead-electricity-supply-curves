library(foreach)
library(doParallel)
library(doSNOW)
library(ffbase)
library(tidyverse)
library(mixtools)
library(LaplacesDemon)
library(cluster)
library(tictoc)


ffload(file="D:/Script/R_script/day_ahead market/day_ahead market")


stepwise_distance_w_plot <- function(p1, q1, #prices and quantities for curve 1
                                     p2, q2,#prices and quantities for curve 2
                                     l = 1, #l of the distance
                                     w_type = c("none", "max_price", "user_wfun"), #type of w(p)
                                     maxprice, # only for step_max. Maximum price considered.
                                     user_fun, # only for user_fun. Function that integrates up to one.
                                     plot = TRUE){
  
  # Define the two supply functions as R functions
  sfun1  <- stepfun(p1, q1, f = 1, right = T)
  sfun2  <- stepfun(p2, q2, f = 1, right = T)
  
  # Define the weighting function
  # Option 1: W(p) = 1. Last jumps are not considered
  if(w_type == "none"){
    
    # Define a grid of evaluation points
    p <- unique(sort(c(0, p1, p2))) #, Inf)))
    grid <- (p[-1]+p[-length(p)])/2
    
    # Define the weighting function W(p) = 1
    swfun  <- stepfun(-1, c(1, 1), f = 1, right = T)
    
    w <- sapply(1:c(length(p)-1), function(x) integrate(swfun, 
                                                        upper = p[-1][x], 
                                                        lower = p[-length(p)][x])$value)
    
    step_dist <- sum(abs(sfun1(grid)-sfun2(grid))^l*w)
  }
  # Option 2: W(p) = 1 until a p < maxprice
  if(w_type == "max_price"){
    if(missing(maxprice)){maxprice <- 10000}
    
    # Define a grid of evaluation points
    p <- unique(sort(c(0, p1, p2, maxprice)))
    grid <- (p[-1]+p[-length(p)])/2
    
    swfun  <- stepfun(maxprice, c(1, 0), f = 1, right = T)
    
    w <- sapply(1:c(length(p)-1), function(x) integrate(swfun, 
                                                        upper = p[-1][x], 
                                                        lower = p[-length(p)][x])$value)
    
    step_dist <- sum(abs(sfun1(grid)-sfun2(grid))^l*w)
  }
  
  if(w_type == "user_wfun"){
    swfun <- user_fun
    # Define a grid of evaluation points
    p <- unique(sort(c(0, p1, p2, Inf))) # W(t) goes to 0
    grid <- (p[-1]+p[-length(p)])/2
    
    w <- sapply(1:c(length(p)-1), function(x) integrate(swfun, 
                                                        upper = p[-1][x], 
                                                        lower = p[-length(p)][x])$value)
    
    step_dist <- sum(abs(sfun1(grid)-sfun2(grid))^l*w)
  }
  
  if(plot == TRUE & w_type %in% c("none", "max_price", "user_wfun")){
    layout(matrix(c(1, 1, 1, 1, 2, 2), nrow = 3, ncol = 2, byrow = T))
    
    if(missing(maxprice)){maxprice = 0}
    plot(sfun1, xlim = c(0, max(p1, p2, maxprice)), ylim = c(0, max(q1, q2)),
         ylab = "Quantity", xlab = "Price", 
         main = paste0("l", l," Stepwise Distance = ", step_dist))
    plot(sfun2, col = "blue",  xlim = c(0, max(p1, p2, maxprice)), add = TRUE)
    points(c(0, p1), q1, pch = 16)
    points(c(0, p2), q2, pch = 16, col = "blue")
    
    plot(swfun, xlim = c(0, max(p1, p2, maxprice)),
         ylab = "Weight", xlab = "Price", 
         main = paste0("Weigthing function: ", w_type))
  }
  
  return(step_dist)
}



n_curves= 365*24*5+24*2
Prices = as.matrix(day_ahead_supply_price[1:n_curves,3:700])
Quants = as.matrix(day_ahead_supply_cumsum[1:n_curves,3:700])


# pp = as.vector(Prices)
# pp = na.omit(pp)
# pp = pp[pp<127]  #127.4305


#mixmdl = normalmixEM(pp, k = 2, maxrestarts = 1000, verb = FALSE)
mixture.gaussian <- function(x) dnormm(x, p = c(mixmdl$lambda[1],1-mixmdl$lambda[1]), mu = mixmdl$mu, sigma = mixmdl$sigma)


start =  835e6 + 1
end = 840e6
inds_sub= inds_ff[,start:end]


cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = end - start +1
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

dist = foreach(i=1:iterations,.combine='c', .options.snow=opts,.packages = "LaplacesDemon") %dopar% {
  index1 = inds_sub[1,i]
  p1 = Prices[index1,]
  p1 = na.omit(p1)
  p1 = t(p1)
  p1 = c(p1)
  
  
  q1 = Quants[index1,]
  q1 = na.omit(q1)
  q1 = t(q1)
  q1 = c(q1)
  
  min_len = min(length(p1),length(q1))
  p1 = p1[1:min_len]
  q1 = q1[1:min_len]
  p1 = p1[-1]
  
  index2 = inds_sub[2,i]
  p2 = Prices[index2,]
  p2 = na.omit(p2) 
  p2 = t(p2)
  p2 = c(p2)
  
  
  q2 = Quants[index2,]
  q2 = na.omit(q2)
  q2 = t(q2)
  q2 = c(q2)
  
  min_len = min(length(p2),length(q2))
  p2 = p2[1:min_len]
  q2 = q2[1:min_len]
  p2 = p2[-1]
  
  stepwise_distance_w_plot(p1 = p1, 
                           q1 = q1,
                           p2 = p2,
                           q2 = q2,
                           l = 2,
                           w_type = "user_wfun",
                           user_fun = mixture.gaussian,
                           plot = F)
}
close(pb)
stopCluster(cl) 


save_file = paste("D:/Script/R_script/day_ahead market/day_ahead market/dist/dist",floor(end/1e6),".RDS",sep = "")
saveRDS(dist,file = save_file)



###################################################################################
# path = "distance/1-310"
# dist_list310 = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
#   map(readRDS) %>%
#   unlist(use.names = F)


path = "D:/Script/R_script/day_ahead market/day_ahead market/dist"
dist_list = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(use.names = F) 
#########################################################################


# 
# d = dist_list
# rm(dist_list)
# gc()
# 
# d = d/264297298.395996 # Divide by sd(Quants)^2
# d = sqrt(d) 

# d_ff = as.ff(d)
# ffsave(d_ff,file="D:/Script/R_script/day_ahead market/day_ahead market",add = T)

attributes(d) <- list(Size = 43848,
                      Labels = as.character(1:43848),
                      Diag = FALSE,
                      Upper = FALSE,
                      method = "user")

class(d) = "dist"

result=hclust(d,method = "average")

result$merge



quantile(result$height)
plot(density(result$height)) #the maxmum is such large that narrow the density curve into a peak
med = median(result$height)
plot(density(result$height[result$height<med]))
plot(density(log(result$height)))


groups <- cutree(result, k=5)
table(groups)

plot(result)
rect.hclust(result, k=5, border=1:5)


require(viridis)
require(dendextend)

# to visualize every subdendrogram
dend <- result %>%as.dendrogram %>%  
  set("labels_to_character") %>% color_branches(k=5)
dend_list <- get_subdendrograms(dend, 5)


par(mfrow = c(2,3))
plot(dend, main = "Original dendrogram")
sapply(dend_list, plot)

#############################################################################

# select best number of cluster using Average Silhouette Method
hc <- result

avg_sil = function(k) {
  cut_res = cutree(hc,k)
  ss = silhouette(cut_res, d)
  mean(ss[, 3])
}

k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- sapply(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")



# select best number of cluster using Within-Cluster-Sum of Squared Errors (WSS)

k = 100
cut_res = cutree(result,k)
cluster_memb = list()

for(i in 1:k){
  cluster_memb[[i]] = unname(which(cut_res==i))
}


save_file = paste("D:/Script/R_script/day_ahead market/day_ahead market/memb_in_cluster/cluster_memb_k_",k,".RDS",sep = "")
saveRDS(cluster_memb,file = save_file)



## 1. find centroid  of each clusters

# m is the number shown in the pairs (,m) and (m,)
# n is the size
find_index = function(m,n){
  
  if(m ==1){
    index = 1:(n-1)
  }
  else{
    
    first_index= m-1
    k = m-1
    
    gap = rep(n-m, k-1)
    offset = (m-2):1
    index1 = c(first_index, first_index + cumsum(gap + offset)) # all index of (,m)
    
    index2 = index1[length(index1)] + (n-m) + 1:(n-m) # all index of (m,)
    index = c(index1, index2)
  }
  return(index)
}




cluster_num = 100
path = paste("D:/Script/R_script/day_ahead market/day_ahead market/memb_in_cluster/cluster_memb_k_",cluster_num,".RDS",sep = "")
cluster_memb = readRDS(path)
cluster_stat = list()

for(cl in 1:cluster_num){
  memb_num= length(cluster_memb[[cl]])
  
  dist2neighbor = c()
  
  
  for(i in 1:memb_num){
    print(paste(cl,"-",i/memb_num))
    index1 = cluster_memb[[cl]][i]
    index2 = cluster_memb[[cl]][-i]
    index2 = index2 - (index2 > index1)
    
    location_in_dist_m = find_index(index1, 43848)[index2]
    dist2neighbor[i]= sum(d_ff[location_in_dist_m])
  }
  
  centroid_index = which.min(dist2neighbor)
  centroid = cluster_memb[[cl]][centroid_index]
  WSS = dist2neighbor[centroid_index]
  cluster_stat[[cl]]= c(centroid,WSS)
}



save_file = paste("D:/Script/R_script/day_ahead market/day_ahead market/cluster_stat_cl",cluster_num,".RDS",sep = "")
saveRDS(cluster_stat,file = save_file)


stats = paste("cluster_stat_cl",c(2,seq(10,100,10)),sep = "") 

Total_WSS = sapply(1:length(stats), function(k)  {
  x = get(stats[k])
  sum(sapply(1:length(x),function(i) x[[i]][2]))})
plot(c(2,seq(10,100,10)),Total_WSS,type = "b")
