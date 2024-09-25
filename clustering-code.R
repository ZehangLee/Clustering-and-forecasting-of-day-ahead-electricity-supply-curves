library(foreach)
library(doParallel)
library(doSNOW)
library(tidyverse)
library(mixtools)
library(LaplacesDemon)
library(cluster)
library(tictoc)
library(dendextend)
library(fpc)

library(lubridate)
library(corrplot)
library(caret)
library(readxl)
library(randomForest)

setwd("D:\\Script\\R_script\\day_ahead market") # change the working path

#-----------------------------------------------------------------------------
# In the following section, we will calculate the weighted distance between eahc
# pair of curves. To do so, we define the next functions:
#   1) stepwise_distance_w_plot: calculating the distance of two given stepwise 
#                                curves. That is to obtain D(Cm, Cn) wehre D is 
#                                distance, and Cm, Cn the curves
#   2) find_index: locating the position of D(Cm, Cn) is a 1-dimensional distance
#                  VECTOR (not distance matrix!!!). Suppose we have 4 curves, then 
#                  each element of the vector is for distance between the pair of
#                  curve following the below combination
#                   | > combn(4,2)
#                   |      [,1] [,2] [,3] [,4] [,5] [,6]
#                   | [1,]    1    1    1    2    2    3
#                   | [2,]    2    3    4    3    4    4
#  3) mixture.gaussian: the weight function estimation from observed prices (some 
#                       trivial prices are disgarded)
#------------------------------------------------------------------------------

stepwise_distance_w_plot <- function(p1, q1, #prices and quantities for curve 1
                                     p2, q2,#prices and quantities for curve 2
                                     l = 2, #norm of the distance, l=2 is for weighted Euclidean distance
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
    index = c(index1, index2)[1:(n-1)]
  }
  return(index)
}

load('day_ahead_supply_cumsum.RData') # the quantities of curves after aggregation
load('day_ahead_supply_price.RData') # the prices of curves
n_curves= 365*24*5+24*2 # !!! REPLACE WITH A SMALLER VALUE LIKE 100 IF YOU ONLY NEED A QUICK PEEK AT THE OUTPUT!!!
Prices = as.matrix(day_ahead_supply_price[1:n_curves,3:700])
Quants = as.matrix(day_ahead_supply_cumsum[1:n_curves,3:700])


pp = as.vector(Prices)
pp = na.omit(pp)
pp = pp[pp<127] 

mixmdl = normalmixEM(pp, k = 2, maxrestarts = 1000, verb = FALSE)
mixture.gaussian <- function(x) dnormm(x, p = c(mixmdl$lambda[1],1-mixmdl$lambda[1]), mu = mixmdl$mu, sigma = mixmdl$sigma)

############################################################################################
# NOTE: THE FOLLOWING PROCEDURE CAN BE SLOW AND MEMORY INTENSIVE, USING HPC IF POSSIBLE !!!#
############################################################################################

inds_ff = combn(n_curves,2) # the vector will have 961,301,628 elements!
start =  1 # slides the inds_ff into smaller batches by change the value of start and end
end = choose(n_curves,2)
inds_sub= inds_ff[,start:end]

# we perform parallel computing to obtain the distances of multiple pairs of curves spontaneously
# if you prefer other computation framework, you can only use lines 171-212 which describe the calculation procedure

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

# automatically save the obtained distance vector
save_file = paste("D:/Script/R_script/day_ahead market/dist/dist",floor(end/1e6),".RDS",sep = "")
saveRDS(dist,file = save_file)

# when all distance vector are obtained, scan all RDS files to merge the complete vector
path = "D:/Script/R_script/day_ahead market/dist/"
dist_list = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(use.names = F) 


d = dist_list
rm(dist_list)
gc()

d = d/264297298.395996 # Divide by sd(Quants)^2
d = sqrt(d) 

# now constructe the distance matrix from the distance vector
attributes(d) <- list(Size = n_curves,
                      Labels = as.character(1:n_curves),
                      Diag = FALSE,
                      Upper = FALSE,
                      method = "user")

class(d) = "dist"




# ------------------------------------------------------------------------------
# In this part, we perform hierarchical clustering (using average linkage) to the 
# distance matrix.
# ------------------------------------------------------------------------------


h_avg <- hclust(d,method = "average")
gc()

hd <- as.dendrogram(h_avg)

plot(hd)

cc <- cutree(h_avg, h = 0.5)
table(cc)


library(dendextend)

dend <- h_avg %>%as.dendrogram  %>% color_branches(h=0.5) 
plot(dend)


dd = as.matrix(d)
gc()

# here we start to remove the outliers
td = cc %in% c(1:7) # !!!! replace this lines with td = cc %in% c(2:4) if you run for the first 100 curves !!!!

dd = dd[1:n_curves,td==TRUE]
gc()


dd = dd[td==TRUE,1:(dim(dd)[2])]
gc()

d_after_discard = as.dist(dd)
rm(dd)
gc()

h_avg_after_discard <- hclust(d_after_discard,method = "average")
gc()

hd_after_discard <- as.dendrogram(h_avg_after_discard)

plot(hd)

dend <- h_avg_after_discard %>%as.dendrogram %>%  
  set("labels_to_character") %>% color_branches(k=4)
dend_list <- get_subdendrograms(dend, 4)

par(mfrow=c(2,2))
sapply(dend_list, plot)


# the elbow plot function **fviz_nbclust** requires original distance matrix, not 
# matrix in customized format. we use the following lines to obtain elbow plot.
# the criteria for measuing the goodness of clustering are within.cluster.ss (within-cluster sum of squares) 
# and avg.silwidth (average  silhouette width)
cs= list()
for(k in 2:10){
  cc = cutree(h_avg_after_discard, k=k)
  cs[[k-1]] = cluster.stats(d_after_discard, cc)
}

wss_avg= c(cs[[1]]$within.cluster.ss,
           cs[[2]]$within.cluster.ss,
           cs[[3]]$within.cluster.ss,
           cs[[4]]$within.cluster.ss,
           cs[[5]]$within.cluster.ss,
           cs[[6]]$within.cluster.ss,
           cs[[7]]$within.cluster.ss,
           cs[[8]]$within.cluster.ss,
           cs[[9]]$within.cluster.ss)

sils_avg= c(cs[[1]]$avg.silwidth,
            cs[[2]]$avg.silwidth,
            cs[[3]]$avg.silwidth,
            cs[[4]]$avg.silwidth,
            cs[[5]]$avg.silwidth,
            cs[[6]]$avg.silwidth,
            cs[[7]]$avg.silwidth,
            cs[[8]]$avg.silwidth,
            cs[[9]]$avg.silwidth)

plot(2:10,wss_avg,type="b")
plot(2:10,sils_avg,type="b")


#-------------------------------------------------------------------------------
# Now perform the classification exercise. The dendrogram of hcluster with average 
# linkage is cut into 4 clusters. 

# We first compose a data frame tt by explantory variables. In particular, we consider the following ones
#      hour,weekday,month,year,holiday
#      gen_Solar,gen_Wind,gen_Nuclear: the generation by solar, wind and nuclear
#      pred_demand: the predicted demand
#      adjusted_GDP,
#      cluster_24lag: the cluster of curve from the same hour but previous day
#      max_price: the observed maximum prices
#      max_agg_quantity: the observed maximum aggregated quantities
#      quantity_at_zero_price: the aggregated quantities observed when price is 0
#      
# We setup two models, namely non-concurrent and concurrent models, considering the
# available of variables
#-------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)

load('day_ahead_supply_price.RData')
tt = ungroup(day_ahead_supply_price[1:n_curves,1:2])

rm("day_ahead_supply_price")

Sys.setlocale("LC_TIME", "C")
tt$weekday = weekdays(as.Date(as.character(tt$date),format = "%d/%m/%Y"))
tt$date = as.Date(as.character(tt$date),format = "%d/%m/%Y")

tt$weekday <- factor(tt$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

tt$month <- months(tt$date)
tt$month <- factor(tt$month, levels = month.name)

tt$year <- year(tt$date)
tt$year = factor(tt$year,levels = c("2016","2017","2018","2019","2020"))

tt$quarter <- quarters(tt$date)
tt$quarter = factor(tt$quarter, levels = c("Q1","Q2","Q3","Q4"))
levels(tt$quarter) =c("QI", "QII", "QIII", "QIV")
tt$yearquarter <- paste0(tt$year, tt$quarter)


holiday_2016 = read.csv("explainable features/holidays/2016.csv",encoding="UTF-8")[,1]
holiday_2017 = read.csv("explainable features/holidays/2017.csv",encoding="UTF-8")[,1]
holiday_2018 = read.csv("explainable features/holidays/2018.csv",encoding="UTF-8")[,1]
holiday_2019 = read.csv("explainable features/holidays/2019.csv",encoding="UTF-8")[,1]
holiday_2020 = read.csv("explainable features/holidays/2020.csv",encoding="UTF-8")[,1]



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
tt$holiday = factor(tt$holiday,level=c(0,1))

solarpv = read.csv("explainable features/RealTimeGenerationSolarPV.csv",sep = ";")
solarpv = solarpv %>% separate(datetime,c("date","hour"),"T")
solarpv$hour =substr(solarpv$hour,1,2)
solarpv$hour = as.integer(solarpv$hour)
solarpv = arrange(solarpv,date,hour)
solarpv$date = as.Date(solarpv$date)
solarpv = solarpv[,c("date","hour","value")]
solarpv = solarpv %>% group_by(date) %>% mutate(ind = row_number())
solarpv = solarpv[,-2]

tt = as_tibble(tt)
tt = left_join(tt,solarpv,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "gen_SolarPV"
tt = select(tt,c("date","hour"),everything())

rm(solarpv)

solartherma = read.csv("explainable features/RealTimeGenerationSolarTherma.csv",sep = ";")
solartherma = solartherma %>% separate(datetime,c("date","hour"),"T")
solartherma$hour =substr(solartherma$hour,1,2)
solartherma$hour = as.integer(solartherma$hour)
solartherma = arrange(solartherma,date,hour)
solartherma$date = as.Date(solartherma$date)
solartherma = solartherma[,c("date","hour","value")]
solartherma = solartherma %>% group_by(date) %>% mutate(ind = row_number())
solartherma = solartherma[,-2]


tt = left_join(tt,solartherma,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "gen_SolarTherma"
rm(solartherma)


wind = read.csv("explainable features/RealTimeGenerationWind.csv",sep = ";")
wind = wind %>% separate(datetime,c("date","hour"),"T")
wind$hour =substr(wind$hour,1,2)
wind$hour = as.integer(wind$hour)
wind = arrange(wind,date,hour)
wind$date = as.Date(wind$date)
wind = wind[,c("date","hour","value")]
wind = wind %>% group_by(date) %>% mutate(ind = row_number())
wind = wind[,-2]


tt = left_join(tt,wind,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "gen_Wind"
rm(wind)


nuclear = read.csv("explainable features/RealTimeGenerationNuclear.csv",sep = ";")
nuclear = nuclear %>% separate(datetime,c("date","hour"),"T")
nuclear$hour =substr(nuclear$hour,1,2)
nuclear$hour = as.integer(nuclear$hour)
nuclear = arrange(nuclear,date,hour)
nuclear$date = as.Date(nuclear$date)
nuclear = nuclear[,c("date","hour","value")]
nuclear = nuclear %>% group_by(date) %>% mutate(ind = row_number())
nuclear = nuclear[,-2]


tt = left_join(tt,nuclear,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "gen_Nuclear"
rm(nuclear)


demand = read.csv("explainable features/RealDemand.csv",sep = ";")
demand = demand %>% separate(datetime,c("date","hour"),"T") %>% filter(name == "Real demand")
demand$hour =substr(demand$hour,1,2)
demand$hour = as.integer(demand$hour)
demand = arrange(demand,date,hour)
demand$date = as.Date(demand$date)
demand = demand[,c("date","hour","value")]
demand = demand %>% group_by(date) %>% mutate(ind = row_number())
demand = demand[,-2]


tt = left_join(tt,demand,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "demand"
rm(demand)

pred_demand = read.csv("explainable features/RealDemand.csv",sep = ";")
pred_demand = pred_demand %>% separate(datetime,c("date","hour"),"T") %>% filter(name == "Forecasted demand")
pred_demand$hour =substr(pred_demand$hour,1,2)
pred_demand$hour = as.integer(pred_demand$hour)
pred_demand = arrange(pred_demand,date,hour)
pred_demand$date = as.Date(pred_demand$date)
pred_demand = pred_demand[,c("date","hour","value")]
pred_demand = pred_demand %>% group_by(date) %>% mutate(ind = row_number())
pred_demand = pred_demand[,-2]


tt = left_join(tt,pred_demand,by=c("date", "hour"="ind"))
colnames(tt)[ncol(tt)] = "pred_demand"
rm(pred_demand)


GDP = read.csv("explainable features/GDP.csv",encoding = "UTF-8")
colnames(GDP)
tt = left_join(tt,GDP,by=c("yearquarter"))


load("day_ahead_supply_price.RData")
load("day_ahead_supply_cumsum.RData")
max_price = sapply(1:n_curves, function(i) max(day_ahead_supply_price[i,3:700],na.rm = T))
max_agg_quantity =  sapply(1:n_curves, function(i) max(day_ahead_supply_cumsum[i,3:700],na.rm = T))
quantity_at_zero_price = day_ahead_supply_cumsum[1:n_curves,3]

tt= cbind(tt,max_price,max_agg_quantity,quantity_at_zero_price)
colnames(tt)[19] = "quantity_at_zero_price" 




# 1. add a explanatory features as the label from t-24, but we are interested in time t, 
#    therefore this model is named non-concurrent



#####################################################################
#note: for the outliers at average linkage, we group them into their# 
#      closer clusters(measured by distance to the centroid)        #
#####################################################################

ind_cc = as.integer(labels(d_after_discard))

cc_avg = cutree(h_avg_after_discard, k=4)

inds_outlier = setdiff(1:n_curves,ind_cc)
dm_after_discard = as.matrix(d_after_discard)
gc() 

centroids= sapply(1:4, function(i) {
  d_rsum = rowSums(dm_after_discard[cc_avg==i,cc_avg==i])
  j = unname(which.min(d_rsum))
  ind_cc[cc_avg==i][j]
})

dm = as.matrix(d)
gc()

dd = dm[centroids,inds_outlier]
outlier_to_near_centroids = apply(dd, MARGIN = 2, which.min)


ind_outliers = setdiff(1:n_curves,ind_cc)
index = c(ind_outliers,ind_cc)
o = order(index)
identical(as.integer(names(outlier_to_near_centroids)),ind_outliers)
cluster = c(unname(outlier_to_near_centroids) ,unname(cc_avg))
cluster = cluster[o]

cluster_24lag = cluster[c(1:24,1:(n_curves -24))]
tt$cluster_24lag = cluster_24lag
tt$cluster = cluster


tt = tt[ind_cc,]
tt$month = as.factor(tt$month)
tt$year = as.factor(tt$year)
tt$weekday = as.factor(tt$weekday)
tt$cluster = as.factor(tt$cluster)
tt$cluster_24lag = as.factor(tt$cluster_24lag)
tt$gen_Solar = tt$gen_SolarPV + tt$gen_SolarTherma


tt = tt %>% select(hour,weekday,month,year,holiday, 
                   gen_Solar,gen_Wind,gen_Nuclear,
                   pred_demand,adjusted_GDP,
                   cluster_24lag,
                   cluster,
                   max_price,
                   max_agg_quantity,
                   quantity_at_zero_price)




set.seed(123)
rf.train1 <- randomForest(cluster ~.- max_agg_quantity - max_price - quantity_at_zero_price, data=tt,
                          ntree=500,mtry=10,importance=TRUE, 
                          do.trace=T,
                          cutoff=table(tt$cluster)/length(tt$cluster))
colnames(rf.train1$importance)[5:6]= c('Mean Decrease Accuracy','Mean Decrease Gini')
varImpPlot(rf.train1,main = "Variable importance of non-concurrent RF model")

gg_1 = ggplot(tt, aes(x=cluster,y = gen_Wind,fill = cluster))+
  geom_boxplot() +
  stat_summary(fun=mean) +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Wind power generation by clusters")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5))
gg_1

gg_2 = ggplot(tt, aes(factor(cluster_24lag)))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  scale_y_continuous(labels = scales::percent) +facet_grid(cluster ~ .)+
  xlab('Lagged cluster labels') +
  ylab('Percent of Group, %') + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Lagged labels percentage within each cluster")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5)) 
gg_2




# 2.  with the previous variables and geometrical features as max_price,
#     max_agg_quantity,quantity_at_zero_price. In this model,we use the 
#     information from our t, so this model is called concurrent model

set.seed(123)
rf.train2 <- randomForest(cluster ~., data=tt,
                          ntree=500,mtry=10,importance=TRUE, 
                          do.trace=T,
                          cutoff=table(tt$cluster)/length(tt$cluster))
colnames(rf.train2$importance)[5:6]= c('Mean Decrease Accuracy','Mean Decrease Gini')
varImpPlot(rf.train2,main = "Variable importance of concurrent RF model")


gg_3 = ggplot(tt, aes(factor(hour)))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),fill = "white",colour ='black') +
  scale_y_continuous(labels = scales::percent) +facet_grid(cluster ~ .)+
  ylab('Percent of Group, %') +
  xlab("Hour")+ coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Hours percentage within each cluster")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5)) 
gg_3




gg_4 = ggplot(tt, aes(x=cluster,y = max_agg_quantity,fill = cluster))+
  geom_boxplot() +
  stat_summary(fun=mean) +
  theme(legend.position="none")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Max quantity by cluster") + ylab('Max quantities') +xlab('Cluster')+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5)) 
gg_4

tt$month = as.integer(factor(tt$month, levels = month.name))
tt$month =  as.factor(tt$month)
gg7 = ggplot(tt, aes(month))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  scale_y_continuous(labels = scales::percent) +facet_grid(cluster ~ .)+
  ylab('Percent of Group, %') + coord_flip() + ggtitle("Months percentage within each cluster") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Month')+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5)) 
gg7

gg8 = ggplot(tt, aes(x=cluster,y = quantity_at_zero_price,fill = cluster))+
  geom_boxplot() +
  stat_summary(fun=mean) +
  theme(legend.position="none")+ ggtitle("Quantities at zero prices by clusters") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab('Quantities at zero prices')+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5)) 
gg8
