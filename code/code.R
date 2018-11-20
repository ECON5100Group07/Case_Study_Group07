library(tidyverse)
library(haven)
library(dplyr)
library(GGally)

#import files
sec0a <- read_dta("raw_data/sec0a.dta")  #data about survey info
sec2a <- read_dta("raw_data/sec2a.dta")  #data about education
sec8b <- read_dta("raw_data/sec8b.dta") #data about unit measure
agg2 <- read_dta("data/aggregates/agg2.dta") #data about household agriculture income
subagg23 <- read_dta("data/aggregates/subagg23.dta") #data about expenditures on crop input per household


summary(sec0a)
summary(sec2a)
count(sec2a)


sec2a %>% group_by(sec2a$clust) %>% summarize(count=n())
sec2a %>% group_by(sec2a$clust,sec2a$nh) %>% summarize(count=n())
sec2a %>% group_by(sec2a$clust, sec2a$nh,sec2a$pid) %>% summarize(count=n())


sec2a %>%
  group_by(sec2a$clust) %>% 
  ggplot(aes(x = sec2a$clust)) + geom_bar() + ggtitle("number of person for each cluster") 
  
