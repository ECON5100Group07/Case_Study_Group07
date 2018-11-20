library(tidyverse)
library(haven)
library(dplyr)
library(GGally)

sec0a <- read_dta("raw_data/sec0a.dta")  #data about survey info
sec2a <- read_dta("raw_data/sec2a.dta")  #data about education
summary(sec2a)
count(sec2a)


sec2a %>% group_by(sec2a$clust) %>% summarize(count=n())
sec2a %>% group_by(sec2a$clust,sec2a$nh) %>% summarize(count=n())
sec2a %>% group_by(sec2a$clust, sec2a$nh,sec2a$pid) %>% summarize(count=n())


sec2a %>%
  group_by(sec2a$clust) %>% 
  ggplot(aes(x = sec2a$clust)) + geom_bar() + ggtitle("number of person for each cluster") 
  
