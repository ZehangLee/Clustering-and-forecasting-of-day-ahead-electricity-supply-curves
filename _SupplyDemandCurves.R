library(ffbase)
library(vroom)
library(tidyverse)
library(doSNOW)
library(foreach)



dat = read.csv.ffdf(file="D:/day-ahead_data/day_ahead.csv",)
head(dat)
dat_df = as.data.frame(dat)
dat_df = dat_df[1:139219144,]

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





###################################################################################

library(ffbase)
library(vroom)
library(tidyverse)
library(doSNOW)
library(foreach)



dat = read.csv.ffdf(file="D:/day-ahead_data/day_ahead.csv",)
head(dat)
dat_df = as.data.frame(dat)
dat_df = dat_df[1:139219144,]


colnames(dat_df) = c("hour","date","country","unit","type","quantity","price","casada")
day_ahead_demand = dat_df %>% select(hour,
                                     date, 
                                     type,
                                     quantity,
                                     price,
                                     casada) %>%
  filter(type == "C")  %>%
  select(-type) %>% 
  group_by(hour,
           date, 
           price) %>%
  summarise(quantity = sum(quantity)) %>% 
  ungroup() %>%
  group_by(hour,
           date) %>%
  arrange(date,hour,desc(price),by_group= T) %>%
  mutate(cumsum_quantity= cumsum(quantity))




day_ahead_demand_price = day_ahead_demand %>% select(hour,date,price) %>%
  group_by(hour,date) %>%
  mutate(temp= 1) %>%
  mutate(ind= paste("x",cumsum(temp),sep = "")) %>%
  select(-temp) %>%
  spread(key=ind,value= price) %>%
  select(paste('x',1:(ncol(.)-2),sep="")) %>%
  arrange(date,hour)

day_ahead_demand_cumsum = day_ahead_demand %>% select(hour,date,cumsum_quantity) %>%
  group_by(hour,date) %>%
  mutate(temp= 1) %>%
  mutate(ind= paste("x",cumsum(temp),sep = "")) %>%
  select(-temp) %>%
  spread(key=ind,value= cumsum_quantity) %>%
  select(paste('x',1:(ncol(.)-2),sep="")) %>%
  arrange(date,hour)


day_ahead_demand_cumsum[day_ahead_demand_cumsum$date=="29/02/2016",]


save(day_ahead_demand_price,file="day_ahead_demand_price.RData")
save(day_ahead_demand_cumsum,file= "day_ahead_demand_cumsum.RData")



