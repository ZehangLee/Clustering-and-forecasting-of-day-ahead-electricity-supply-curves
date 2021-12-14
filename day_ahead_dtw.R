library(ff)
library(vroom)
library(tidyverse)
library(doSNOW)
library(foreach)



dat = read.csv.ffdf(file="D:/day-ahead_data/day_ahead.csv",)
head(dat)
dat_df = as.data.frame(dat)


colnames(dat_df) = c("hour","date","country","unit","type","quantity","price","casada")
day_ahead_supply = dat_df %>% select(hour,
                           date, 
                           type,
                           quantity,
                           price) %>%
                          filter(type == "V")  %>%
                          select(-type) %>% 
                          group_by(hour,
                                     date, 
                                     price) %>%
                           arrange(price,by_group=T) %>%
                           summarise(quantity = sum(quantity)) %>% 
                           mutate(cumsum_quantity= cumsum(quantity))




day_ahead_supply_price = day_ahead_supply %>% select(hour,date,price) %>%
  group_by(hour,date) %>%
  mutate(temp= 1) %>%
  mutate(ind= paste("x",cumsum(temp),sep = "")) %>%
  select(-temp) %>%
  spread(key=ind,value= price) %>%
  select(paste('x',1:(ncol(.)-2),sep="")) %>%
  arrange(date,hour)

day_ahead_supply_cumsum = day_ahead_supply %>% select(hour,date,cumsum_quantity) %>%
  group_by(hour,date) %>%
  mutate(temp= 1) %>%
  mutate(ind= paste("x",cumsum(temp),sep = "")) %>%
  select(-temp) %>%
  spread(key=ind,value= cumsum_quantity) %>%
  select(paste('x',1:(ncol(.)-2),sep="")) %>%
  arrange(date,hour)


day_ahead_supply_cumsum[day_ahead_supply_cumsum$date=="29/02/2016",]


save(day_ahead_supply_price,file="day_ahead_supply_price.RData")
save(day_ahead_supply_cumsum,file= "day_ahead_supply_cumsum.RData")
                 



#########################################################################
#
#DTW distance
#
library(doSNOW)
library(foreach)

Ncurves = 365*24+24

Prices = as.matrix(day_ahead_supply_price[1:Ncurves,3:700])
Quants = as.matrix(day_ahead_supply_cumsum[1:Ncurves,3:700])
rm('day_ahead_supply_cumsum','day_ahead_supply_price')
gc()




q.sd = sd(Quants, na.rm = T)
p.sd = sd(Prices, na.rm = T)
inds = combn(seq_len(Ncurves), 2)


start = 1
end = 10
inds_sub= inds[,start:end]


comb <- function(...) {
  mapply(cbind, ..., SIMPLIFY=FALSE)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = end - start +1
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)


dist_dtw = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="dtw")%dopar%{
  index1 = inds_sub[1,i]
  index2 = inds_sub[2,i]
  
  p1 = na.omit(Prices[index1,])
  q1 = na.omit(Quants[index1,])
  p2 = na.omit(Prices[index2,])
  q2 = na.omit(Quants[index2,])
  
  q1.sd = q1/q.sd
  q2.sd = q2/q.sd
  p1.sd = p1/p.sd
  p2.sd = p2/p.sd
  
  d12 = dist(cbind(p1.sd,q1.sd), cbind(p2.sd,q2.sd))
  
  alignment <- dtw(d12,distance.only = TRUE)
  
  dist2curve = alignment$distance
  normdist2curve = alignment$normalizedDistance
  list(dist = dist2curve, normdist = normdist2curve)
}

close(pb)
stopCluster(cl) 

save_file = paste("dist",floor(end/1e6),".RDS",sep = "")
saveRDS(dist_dtw,file = save_file)


path = "dist"
dist_list = list.files(path=path,pattern = ".RDS",full.names = T) %>% 
  map(readRDS) %>%
  unlist(recursive= F,use.names = F) 


dist = dist_list[seq(1,75,by=2)]
normdist = dist_list[seq(2,76,by=2)]


sum(sapply(1:38, function(x) identical(dist[x], dist_list[2*x-1])))
sum(sapply(1:38, function(x) identical(normdist[x], dist_list[2*x])))


dist=unlist(dist)
normdist=unlist(normdist)


attributes(dist) <- list(Size = 8784,
                      Labels = as.character(1:8784),
                      Diag = FALSE,
                      Upper = FALSE,
                      method = "user")

class(dist) = "dist"
result1=hclust(dist)
plot(result)

attributes(normdist) <- list(Size = 8784,
                         Labels = as.character(1:8784),
                         Diag = FALSE,
                         Upper = FALSE,
                         method = "user")

class(normdist) = "dist"
result2=hclust(normdist)
plot(result)




#Ncurves = 365*24*5+24*2

#Prices = as.matrix(day_ahead_supply_price[1:Ncurves,3:700])
#Quants = as.matrix(day_ahead_supply_cumsum[1:Ncurves,3:700])


#write.csv(Prices,file="Prices.csv",row.names = F)
#write.csv(Quants,file="Quants.csv",row.names = F)
