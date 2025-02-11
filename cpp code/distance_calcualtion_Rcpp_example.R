library(LaplacesDemon)
library(tictoc)
library(Rcpp)
library(foreach)
library(doParallel)
library(doSNOW)

sourceCpp("D:/Script/R_script/day_ahead market/day_ahead market/cpp code/somefile.cpp")

load("D:/Script/R_script/day_ahead market/day_ahead market/day_ahead_supply_cumsum.RData")
load("D:/Script/R_script/day_ahead market/day_ahead market/day_ahead_supply_price.RData")
n_curves= 365*24*5+24*2
Prices = as.matrix(day_ahead_supply_price[1:n_curves,3:700])
Quants = as.matrix(day_ahead_supply_cumsum[1:n_curves,3:700])

set.seed(1234)

ind1 = round(runif(10000, 1, 43848))
ind2 = round(runif(10000, 1, 43848))

any(ind1 == ind2)

aa=which(ind1 == ind2)
ind1[aa] = ind1[aa] +1
any(ind1 == ind2)

load("D:/Script/R_script/day_ahead market/day_ahead market/mixmdl.RData")
#mixmdl = normalmixEM(pp, k = 2, maxrestarts = 1000, verb = FALSE)
mixture.gaussian <- function(x) dnormm(x, p = c(mixmdl$lambda[1],1-mixmdl$lambda[1]), mu = mixmdl$mu, sigma = mixmdl$sigma)

#########################
# original R
########################


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
    #layout(matrix(c(1, 1, 1, 1, 2, 2), nrow = 3, ncol = 2, byrow = T))
    
    
    if(missing(maxprice)){maxprice = 0}
    plot(sfun1, xlim = c(0, max(p1, p2, maxprice)), ylim = c(0, max(q1, q2)),
         ylab = "Quantity", xlab = "Price", 
         main = paste0("Distance = ", step_dist)) #paste0("l", l," Stepwise Distance = ", step_dist))
    plot(sfun2, col = "blue",  xlim = c(0, max(p1, p2, maxprice)), add = TRUE)
    points(c(0, p1), q1, pch = 16)
    points(c(0, p2), q2, pch = 16, col = "blue")
    
    # plot(swfun, xlim = c(0, max(p1, p2, maxprice)),
    #      ylab = "Weight", xlab = "Price", 
    #      main = paste0("Weigthing function: ", w_type))
  }
  
  return(step_dist)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = length(ind1)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)


tic()
dist = foreach(i=1:iterations,.combine='c', .options.snow=opts,.packages = "LaplacesDemon") %dopar% {
  index1 = ind1[i]
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
  
  index2 = ind2[i]
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
toc()


dist

#######################
# Rcpp
#######################
# when using parallel, must compile the C++ per process or distribute the C++ code within an R package.
l=2
prob = mixmdl$lambda[1]
mu = mixmdl$mu
sigma = mixmdl$sigma

step_dist_cpp = c()

tic()
for(i in 1:length(ind1)){
  index1 = ind1[i]
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
  
  index2 = ind2[i]
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
  
  sfun1  <- stepfun(p1, q1, f = 1, right = T)
  sfun2  <- stepfun(p2, q2, f = 1, right = T)
  
  p <- unique(sort(c(0, p1, p2, Inf))) # W(t) goes to 0
  grid <- (p[-1]+p[-length(p)])/2
  
  w_cpp = sapply(1:c(length(p)-1), function(x) gm_intg(prob, mu[1], sigma[1], mu[2], sigma[2], 
                                                       upper = p[-1][x], 
                                                       lower = p[-length(p)][x]))
  res <- sum(abs(sfun1(grid)-sfun2(grid))^l*w_cpp)
  step_dist_cpp = c(step_dist_cpp,res)
  
}
toc()

identical(step_dist_cpp,dist)
max(abs(step_dist_cpp-dist))

# Parallel computing (6 cores) costs 156.87 sec when applied on 10,000 pairs of curve
# while the competitor using Rcpp costs 137.45 sec. The maximum difference between
# the respective distances is ~0.05

# In terms of 100,000 pairs of curves, parallel computing complete the 
# within 1520.73 sec, whereas rcpp procedure costs 1411.47 sec. The maximum difference
# is ~0.067




####################################################################
# now we build a package based on the cpp file

#library(Rcpp)
Rcpp.package.skeleton("MyGMMDist", example_code = FALSE,
                      cpp_files = c("D:/Script/R_script/day_ahead market/day_ahead market/cpp code/somefile.cpp"))

setwd("D:/Script/R_script/day_ahead market/day_ahead market/cpp code")
desc <- readLines("MyGMMDist/DESCRIPTION")
kk <- which(grepl("LinkingTo", desc))
desc[kk] <- paste0(desc[kk], ", RcppEigen, RcppNumerical")
writeLines(desc, "MyGMMDist/DESCRIPTION")

compileAttributes("MyGMMDist")

system("R CMD build MyGMMDist")
system("R CMD INSTALL MyGMMDist_1.0.tar.gz")





library(MyGMMDist)

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = length(ind1)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)


tic()
dist_cpp_parallel = foreach(i=1:iterations,.combine='c', .options.snow=opts,.packages = "MyGMMDist") %dopar% {
  index1 = ind1[i]
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
  
  index2 = ind2[i]
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
  
  sfun1  <- stepfun(p1, q1, f = 1, right = T)
  sfun2  <- stepfun(p2, q2, f = 1, right = T)
  
  p <- unique(sort(c(0, p1, p2, Inf))) # W(t) goes to 0
  grid <- (p[-1]+p[-length(p)])/2
  
  w_cpp = sapply(1:c(length(p)-1), function(x) MyGMMDist::gm_intg(prob, mu[1], sigma[1], mu[2], sigma[2], 
                                                       upper = p[-1][x], 
                                                       lower = p[-length(p)][x]))
  res <- sum(abs(sfun1(grid)-sfun2(grid))^l*w_cpp)
  res
}
close(pb)
stopCluster(cl) 
toc()


identical(dist_cpp_parallel,dist)
max(abs(dist_cpp_parallel-dist))

# under the seed 1234, this process only cost 60 sec to obtain 10,000 distance. It is the fastest approach.
