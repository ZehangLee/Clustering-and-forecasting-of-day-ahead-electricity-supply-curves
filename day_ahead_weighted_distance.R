library(foreach)
library(doParallel)
library(doSNOW)
library(ffbase)
library(tidyverse)
library(mixtools)
library(LaplacesDemon)
library(cluster)
library(tictoc)
library(dendextend)
library(dbscan)

ffload(file="D:/Script/R_script/day_ahead market/day_ahead market/ff_archive")


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

path = "D:/Script/R_script/day_ahead market/day_ahead market/dist"
dist_list = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(use.names = F) 
#########################################################################



d = dist_list
rm(dist_list)
gc()

d = d/264297298.395996 # Divide by sd(Quants)^2
d = sqrt(d) 

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

plot(result,main="cluster dendrogram with 30 clusters")
rect.hclust(result, k=30)


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

# check Nuber-of-cluster.pdf / analysis_code to see the choice of number of 
# clusters



#############################################################################


load("day_ahead_supply_cumsum.RData")
load("day_ahead_supply_price.RData")

n_curves= 365*24*5+24*2
tt = day_ahead_supply_price[1:n_curves,1:2]

Sys.setlocale("LC_TIME", "C")
tt$weekday = weekdays(as.Date(as.character(tt$date),format = "%d/%m/%Y"))
tt$date = as.Date(as.character(tt$date),format = "%d/%m/%Y")

rm("day_ahead_supply_cumsum")
rm("day_ahead_supply_price")

head(tt)

holiday_2016 = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/holidays/2016.csv",encoding="UTF-8")[,1]
holiday_2017 = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/holidays/2017.csv",encoding="UTF-8")[,1]
holiday_2018 = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/holidays/2018.csv",encoding="UTF-8")[,1]
holiday_2019 = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/holidays/2019.csv",encoding="UTF-8")[,1]
holiday_2020 = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/holidays/2020.csv",encoding="UTF-8")[,1]



holiday_2016 = paste(holiday_2016,"-2016",sep="")
holiday_2017 = paste(holiday_2017,"-2017",sep="")
holiday_2018 = paste(holiday_2018,"-2018",sep="")
holiday_2019 = paste(holiday_2019,"-2019",sep="")
holiday_2020 = paste(holiday_2020,"-2020",sep="")

holiday_2016 = unique(as.Date(holiday_2016,format = "%d-%b-%Y"))
holiday_2017 = unique(as.Date(holiday_2017,format = "%d-%b-%Y"))
holiday_2018 = unique(as.Date(holiday_2018,format = "%d-%b-%Y"))
holiday_2019 = unique(as.Date(holiday_2019,format = "%d-%b-%Y"))
holiday_2020 = unique(as.Date(holiday_2020,format = "%d-%b-%Y"))

tt$holiday = ifelse(tt$date %in% c(holiday_2016,holiday_2017,holiday_2018,holiday_2019,holiday_2020), 1, 0)
rm('holiday_2016','holiday_2017','holiday_2018','holiday_2019','holiday_2020')

solarpv = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealTimeGenerationSolarPV.csv",sep = ";")
solarpv = solarpv %>% separate(datetime,c("date","hour"),"T")
solarpv$hour =substr(solarpv$hour,1,2)
solarpv$hour = as.integer(solarpv$hour)
solarpv = arrange(solarpv,date,hour)
solarpv$date = as.Date(solarpv$date)
solarpv = solarpv[,c("date","hour","value")]
solarpv = solarpv %>% group_by(date) %>% mutate(ind = row_number())
solarpv = solarpv[,-2]

tt = as.tibble(tt)
tt = left_join(tt,solarpv,by=c("date", "hour"="ind"))
tt = select(tt,c("date","hour","weekday","holiday","value"))
colnames(tt)[5] = "gen_SolarPV"
rm(solarpv)


solartherma = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealTimeGenerationSolarTherma.csv",sep = ";")
solartherma = solartherma %>% separate(datetime,c("date","hour"),"T")
solartherma$hour =substr(solartherma$hour,1,2)
solartherma$hour = as.integer(solartherma$hour)
solartherma = arrange(solartherma,date,hour)
solartherma$date = as.Date(solartherma$date)
solartherma = solartherma[,c("date","hour","value")]
solartherma = solartherma %>% group_by(date) %>% mutate(ind = row_number())
solartherma = solartherma[,-2]


tt = left_join(tt,solartherma,by=c("date", "hour"="ind"))
colnames(tt)[6] = "gen_SolarTherma"
rm(solartherma)


wind = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealTimeGenerationWind.csv",sep = ";")
wind = wind %>% separate(datetime,c("date","hour"),"T")
wind$hour =substr(wind$hour,1,2)
wind$hour = as.integer(wind$hour)
wind = arrange(wind,date,hour)
wind$date = as.Date(wind$date)
wind = wind[,c("date","hour","value")]
wind = wind %>% group_by(date) %>% mutate(ind = row_number())
wind = wind[,-2]


tt = left_join(tt,wind,by=c("date", "hour"="ind"))
colnames(tt)[7] = "gen_Wind"
rm(wind)


nuclear = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealTimeGenerationNuclear.csv",sep = ";")
nuclear = nuclear %>% separate(datetime,c("date","hour"),"T")
nuclear$hour =substr(nuclear$hour,1,2)
nuclear$hour = as.integer(nuclear$hour)
nuclear = arrange(nuclear,date,hour)
nuclear$date = as.Date(nuclear$date)
nuclear = nuclear[,c("date","hour","value")]
nuclear = nuclear %>% group_by(date) %>% mutate(ind = row_number())
nuclear = nuclear[,-2]


tt = left_join(tt,nuclear,by=c("date", "hour"="ind"))
colnames(tt)[8] = "gen_Nuclear"
rm(nuclear)


demand = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealDemand.csv",sep = ";")
demand = demand %>% separate(datetime,c("date","hour"),"T") %>% filter(name == "Real demand")
demand$hour =substr(demand$hour,1,2)
demand$hour = as.integer(demand$hour)
demand = arrange(demand,date,hour)
demand$date = as.Date(demand$date)
demand = demand[,c("date","hour","value")]
demand = demand %>% group_by(date) %>% mutate(ind = row_number())
demand = demand[,-2]


tt = left_join(tt,demand,by=c("date", "hour"="ind"))
colnames(tt)[9] = "demand"
rm(demand)

pred_demand = read.csv("D:/Script/R_script/day_ahead market/day_ahead market/power_generation/RealDemand.csv",sep = ";")
pred_demand = pred_demand %>% separate(datetime,c("date","hour"),"T") %>% filter(name == "Forecasted demand")
pred_demand$hour =substr(pred_demand$hour,1,2)
pred_demand$hour = as.integer(pred_demand$hour)
pred_demand = arrange(pred_demand,date,hour)
pred_demand$date = as.Date(pred_demand$date)
pred_demand = pred_demand[,c("date","hour","value")]
pred_demand = pred_demand %>% group_by(date) %>% mutate(ind = row_number())
pred_demand = pred_demand[,-2]


tt = left_join(tt,pred_demand,by=c("date", "hour"="ind"))
colnames(tt)[10] = "pred_demand"
rm(pred_demand)

tt_gen = tt
save(tt_gen,file = "time&generation.RData")
