library(foreach)
library(doParallel)
library(doSNOW)
library(ffbase)
library(tidyverse)
library(mixtools)
library(LaplacesDemon)


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


pp = as.vector(Prices)
pp = na.omit(pp)
pp = pp[pp<127]  #127.4305


mixmdl = normalmixEM(pp, k = 2, maxrestarts = 1000, verb = FALSE)
mixture.gaussian <- function(x) dnormm(x, p = c(mixmdl$lambda[1],1-mixmdl$lambda[1]), mu = mixmdl$mu, sigma = mixmdl$sigma)


start =  1
end = 100#950e6
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


save_file = paste("dist",floor(end/1e6),".RDS",sep = "")
saveRDS(dist,file = save_file)



###################################################################################
path = "distance/1-310"
dist_list310 = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(use.names = F)


path = "distance"
dist_list_after_310 = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(use.names = F) 




d = dist_list

attributes(d) <- list(Size = 43848,
                      Labels = as.character(1:43848),
                      Diag = FALSE,
                      Upper = FALSE,
                      method = "user")

class(d) = "dist"

result=hclust(d)
result$merge

