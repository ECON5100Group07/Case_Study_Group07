library(tidyverse)
library(here)
library(GGally)
library(ggplot2)
library(haven)
library(dplyr)


#import useful dataset

#our focus on crop income as main profit:
#INC10 PSV1 = Revenue from sale of cash crops-main outlet
#CROPSV2 = Revenue from sale of cash crops-other outlet (both at level of each individual crop)
subagg13_crop <- read_dta(here("raw_data/aggregates","subagg13.dta")) #Cash crops 
subagg16 <- read_dta(here("raw_data/aggregates","subagg16.dta")) #Sale of transformed crop prod


#expenditure on crop inputs:
#EXP3 LANDEXP = Expenditure on renting farm land (at level of individual farm)
#EXP4 CROPEXP = Expenditure on crop inputs (at level of each individual input)
#EXP5 LIVEXP = Expenditure on livestock inputs (at level of each individual input)
#EXP6 FDPREXP1 = Labour costs on food processing
#FDPREXP2 = Other costs on food processing (both at level of each individual product)


#Household  employment wage income and food expenditutres:
#agg1 Total wage income main job – past 12 months
#SUBAGG6 FD = Total wage income paid in food, HO = Total wage income paid in housing, GD = Total wage income paid in other forms
agg1 <- read_dta(here("raw_data/aggregates","agg1.dta"))
subagg6 <- read_dta(here("raw_data/aggregates","subagg6.dta"))
wage_income_per_hh <- agg1 %>% mutate(ID = paste(clust,nh,sep="_"),totemp = ifelse(j1tot>1e+100,NA,totemp)) %>% 
  group_by(clust) %>% summarise(aveincome =sum(totemp)/count(nh))
View(wage_income_per_hh)
food_expense_per_hh <- subagg6 %>% mutate(ID = paste(clust,nh,sep="_"),fd = ifelse(fd>1e+100,NA,fd)) %>% 
  group_by(clust) %>% summarise(avefd =sum(fd)/count(nh))
View(food_expense_per_hh)
wage_fdexpense <- merge(x = wage_income_per_hh, y = food_expense_per_hh, by = "clust", all = TRUE)
View(wage_fdexpense)
cor_wage_fdexpense <- cor(wage_fdexpense$aveincome, wage_fdexpense$avefd)
regression_wage_fdexpense <- lm(avefd ~ aveincome, data = wage_fdexpense)
summary(regression_wage_fdexpense)
ggplot(wage_fdexpense, 
       aes(x = aveincome, y = avefd)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  xlab("Food Expenses") +
  ylab("Main Job Income") +
  ggtitle("Relationship between Employment Income and Food Expenditures")

ggsave(here("figures", "regression_wage_fdexpense.png"))

#Household education:
# household member education
# map education qualification to education level
sec2a_educ <- read_dta(here("raw_data","sec2a.dta"))
educ_level_map <- data.frame("s2aq3" = c(1:14, 96),
                             "s2aq3l" = c(1, 2, rep(3, 4), rep(4, 8), 5))

hhm_educ <- sec2a_educ %>%
  select(nh, pid, clust, s2aq3) %>%
  left_join(educ_level_map) %>%
  select(-s2aq3) %>%
  mutate(s2aq3l = factor(s2aq3l,
                         levels = as.character(c(1:5)),
                         labels = c("None", "Basic Education", "Secondary Education",
                                    "Tertiary Education", "Other")))

View(hhm_educ)

#fdexpense_wage_educ <- wage_fdexpense %>%
  #select("clust", "aveincome", "avefd")
 # full_join(hhm_educ, by=c("clust")) %>%
  #group_by(clust) %>%
  #summarise(
           # noneEducPercent = sum(s2aq3l == "None", na.rm = TRUE)/n(),
            #basicEducPercent = sum(s2aq3l == "Basic Education", na.rm = TRUE)/n(),
            #secEducPercent = sum(s2aq3l == "Secondary Education", na.rm = TRUE)/n(),
            #terEducPercent = sum(s2aq3l == "Tertiary Education", na.rm = TRUE)/n(),
            #otherEducPercent = sum(s2aq3l == "Other", na.rm = TRUE)/n())
#View(fdexpense_wage_educ)


## ----import dataset------------------------------------------------------------------
agg1 <- read_dta(here("raw_data/aggregates","agg1.dta"))
agg2 <- read_dta(here("raw_data/aggregates","agg2.dta"))
sec8b <- read_dta(here("raw_data","sec8b.dta"))
sec0a <- read_dta(here("raw_data","sec0a.dta"))
sec1 <- read_dta(here("raw_data","sec1.dta"))
sec2a <- read_dta(here("raw_data","sec2a.dta"))
sec8a2 <- read_dta(here("raw_data","sec8a2.dta"))
sec8a3 < read_dta(here("raw_data","sec8a3.dta"))
sec8c1 <- read_dta(here("raw_data","sec8c1.dta"))
sec8c2 <- read_dta(here("raw_data","sec8c2.dta"))

## ----calculate agricultural profit per area unit (the y variable)----------------------------------
# check correlation between agricultural income and corrected agricultural income
cor(agg2$agri1, agg2$agri1c)
cor(agg2$agri2, agg2$agri2c)

# identify count of NAs in agg2, no na found
colSums(is.na(agg2))

# map land size units to acre with a multiplier (s8bq4bm)
# the "Other" unit is treated as NA
#### huibo: A Poles is equivalent to 210 feet by 210 feet or 70 by 70 yards, which is equivalent to one Acre
#### huibo: Nine (9) Ropes are equivalent to actual Acre, ie. a Rope is equivalent to one-ninth (1/9) of an Acre
land_size_unit_map <- data.frame("s8bq4b" = as.double(c(1:4)),
                                 "s8bq4bm" = c(1, 1, 0.1111, 0)) #### huibo


#summary(sec8b)
# convert all land size to acre
land_size_info <- sec8b %>%
  select(clust, nh, s8bq4a, s8bq4b) %>%
  #### huibo: if unit >3, treat it as 4, which is "Others"
  mutate(s8bq4b = ifelse(s8bq4b > 3, 4, s8bq4b)) %>%
  left_join(land_size_unit_map, by = "s8bq4b") %>%
  # s8bq4ac is corrected s8bq4a, which is land size in acre
  mutate(s8bq4ac = s8bq4a * s8bq4bm) %>%
  # convert NA to 0 to calculate sum of land size for each household
  #### huibo
  #replace(., is.na(.), 0) %>%
  group_by(clust, nh) %>%
  summarise(landSize = sum(s8bq4ac)) %>%
  # convert 0 land size back to NA because it will be used as denominator
  ##### huibo: why not filter "0" rows directly
  #mutate(landSize = ifelse(landSize == 0, NA, landSize))
  filter(landSize!=0) 

# calculate household agri profit per acre
# because the above correlations are both 1,
# use only "agri1" and "agri2" to calculate agricultural profit

#### huibo resolve warnings
attr(agg2$clust, "label") <- "Enumeration Area number"
attr(agg2$nh, "label") <- "Household ID"

hh_profit_info <- agg2 %>%
  inner_join(land_size_info, by = c("clust", "nh")) %>%
  transmute(clust = clust, nh = nh,
            profit = (agri1 - hhagdepn) / landSize) #%>% ##### huibo: only use agri1-hhagdepn
#profit = (agri1 + argi2 - hhagdepn) / landSize) #%>%
#filter(!is.na(profit)) #### huibo: can't be na here since landSize==0 is filtered above

attr(hh_profit_info$profit, "label") <- "HH agri profit per acre"

## ----tidy household information----------------------------------------------------------
# function to replace unknown value and factorize data
cleanAndFactorize <- function(data, threshold, replaceValue, levels, labels) {
  data <- ifelse(data > threshold, replaceValue, data)
  data <- factor(data, levels=levels, labels=labels)
  return(data)
}

# household basic information
hh_basic_info <- sec0a %>%
  select(region, district, clust, nh, reslan, ez:loc3) %>%
  mutate(reslan = cleanAndFactorize(reslan, 1e+100, 99,
                                    levels = as.character(c(1:8, 99)),
                                    labels = c("English", "Akan", "Ewe", "Ga-Adangbe",
                                               "Dagbani", "Hausa", "Nzema", "Other", "Unknown")),
         ez = factor(ez,
                     levels = as.character(c(1:3)),
                     labels = c("Coastal", "Forest", "Savannah")),
         loc2 = factor(loc2,
                       levels = as.character(c(1,2)),
                       labels = c("Urban", "Rural")),
         loc5 = factor(loc5, 
                       levels = as.character(c(1:5)),
                       labels = c("Accra", "OtherUrban", "RuralCoastal",
                                  "RuralForest", "RuralSavannah")),
         loc3 = factor(loc3,
                       levels = as.character(c(1:3)),
                       labels = c("Accra", "OtherUrban", "Rural"))
  )

## ----tidy household member information---------------------------------------------------
# household member information
colSums(is.na(sec1))


hhm_info <- sec1 %>%
  select(clust, nh, pid, sex, agey) %>%
  ##### huibo: refer to G4repot - P27
  ##### huibo: why not factorize agey to <15, >=15 & <65, >=65
  filter(agey >= 15 & agey <= 65) %>%
  mutate(sex = factor(sex,
                      levels = as.character(c(1,2)),
                      labels = c("Male", "Female"))
  )

# household member education information
# map education qualification to education level
educ_level_map <- data.frame("s2aq3" = c(1:14, 96),
                             "s2aq3l" = c(0, 1, rep(2, 4), rep(3, 8), 1.5))
### huibo resolve warnings
attr(educ_level_map$s2aq3, "label") <- "Highest educ qualification"
attr(educ_level_map$s2aq3, "format.stata") <- "%10.0g"
attributes(sec2a$s2aq3)
### huibo: Check NAs
colSums(is.na(sec2a))

hhm_educ <- sec2a %>%
  select(clust, nh, pid, s2aq3) %>%
  left_join(educ_level_map, by = "s2aq3") %>%
  select(-s2aq3) #%>%
##replace(., is.na(.), 0) #####HUIBO NO NA FOUND
# mutate(s2aq3l = factor(s2aq3l,
#                        levels = as.character(c(1:5)),
#                        labels = c("None", "Basic Education", "Secondary Education",
#                                   "Tertiary Education", "Other")))

# hh_member_info <- hhm_info %>%
#   full_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
#   group_by(clust, nh) %>%
#   summarise(malePercent = sum(sex == "Male")/n(),
#             femalePercent = sum(sex == "Female")/n(),
#             avgAge = mean(agey),
#             noneEducPercent = sum(s2aq3l == "None", na.rm = TRUE)/n(),
#             basicEducPercent = sum(s2aq3l == "Basic Education", na.rm = TRUE)/n(),
#             secEducPercent = sum(s2aq3l == "Secondary Education", na.rm = TRUE)/n(),
#             terEducPercent = sum(s2aq3l == "Tertiary Education", na.rm = TRUE)/n(),
#             otherEducPercent = sum(s2aq3l == "Other", na.rm = TRUE)/n())
# hh_member_info <- hhm_info %>%
#   left_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
#   group_by(clust, nh) %>%
#   summarise(maleCount = sum(sex == "Male"),
#             femaleCount = sum(sex == "Female"),
#             avgAge = mean(agey),
#             noneEducCount = sum(s2aq3l == "None", na.rm = TRUE),
#             basicEducCount = sum(s2aq3l == "Basic Education", na.rm = TRUE),
#             secEducCount = sum(s2aq3l == "Secondary Education", na.rm = TRUE),
#             terEducCount = sum(s2aq3l == "Tertiary Education", na.rm = TRUE),
#             otherEducCount = sum(s2aq3l == "Other", na.rm = TRUE))
hh_member_info <- hhm_info %>%
  left_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
  #replace(., is.na(.), 0) %>%
  group_by(clust, nh) %>%
  summarise(maleCount = sum(sex == "Male"),
            femaleCount = sum(sex == "Female"),
            avgAge = mean(agey),
            avgEdu = mean(s2aq3l))

## ----tidy agricultural characteristics information---------------------------------------
# spread livestock count and count livestock type
# since there are too many missing value in livestock unit of messaure (s8aq22b),
# we assume same livestock type has same unit,
# and we only take livestock count (s8aq22a) into consideration

colSums(is.na(sec8a2))

hh_livestock_info <- sec8a2 %>%
  select(clust, nh, livstcd, s8aq22a) %>%
  group_by(clust, nh) %>%
  mutate(livstcdTypeCount = n()) %>%
  spread(key = livstcd,
         value = s8aq22a,
         fill = 0,
         sep = "")

# count household agric equipment type
# not including count of each equipment because of too many missing value (s8aq34)

colSums(is.na(sec8a3))

hh_equip_info <- sec8a3 %>%
  select(clust, nh, eqcdown) %>%
  group_by(clust, nh) %>%
  summarise(equipTypeCount = n())

# spread household havested crop count and count havested crop type

colSums(is.na(sec8c1))

hh_crop_info <- sec8c1 %>%
  select(clust, nh, cropcd, s8cq3a, s8cq17a, s8cq17b) %>%
  replace(., . > 1e+100, 0) %>%
  group_by(clust, nh, cropcd) %>%
  # sum up quantity of same crop type
  summarise(s8cq3ac = sum(s8cq3a)) %>% 
  group_by(clust, nh) %>%
  mutate(cropTypeCount = n()) %>%
  spread(key = cropcd,
         value = s8cq3ac,
         fill = 0,
         sep = "")

# spread household havested root count and count havested root type

colSums(is.na(sec8c2))

hh_root_info <- sec8c2 %>%
  select(clust, nh, rootcd, s8cq21a) %>%
  replace(., . > 1e+100, 0) %>%
  group_by(clust, nh, rootcd) %>%
  # sum up quantity of same root type
  summarise(s8cq21ac = sum(s8cq21a)) %>% 
  group_by(clust, nh) %>%
  mutate(rootTypeCount = n()) %>%
  spread(key = rootcd,
         value = s8cq21ac,
         fill = 0,
         sep = "")

hh_agri_info <- hh_livestock_info %>%
  full_join(hh_equip_info, by=c("clust", "nh")) %>%
  full_join(hh_crop_info, by=c("clust", "nh")) %>%
  full_join(hh_root_info, by=c("clust", "nh")) %>%
  replace(., is.na(.), 0)

## ----combine all information and fit model-----------------------------------------------
hh_all_info <- hh_basic_info %>%
  full_join(hh_member_info, by=c("clust", "nh")) %>%
  full_join(hh_agri_info, by=c("clust", "nh")) %>%
  replace(., is.na(.), 0)

hh_profit <- hh_profit_info %>%
  left_join(hh_all_info, by=c("clust", "nh"))

# code below here is not finalized, it needs more test and tweak
library(MASS)

hh_profit_model_full <- lm(profit ~ . - clust - nh - region - district,
                           data = hh_profit)
summary(hh_profit_model_full)
hh_profit_model_full_step <- stepAIC(hh_profit_model_full, direction = "both", trace = FALSE)
summary(hh_profit_model_full_step)

hh_profit_model_sub1 <- lm(profit ~ reslan + ez + livstcd5 + livstcd11 + cropcd4 + cropcd10 + cropcd11 + cropcd25 + rootcd18,
                           data = hh_profit)
summary(hh_profit_model_sub1)
hh_profit_model_basic <- lm(profit ~ ez + loc2 + loc5 + loc3,
                            data = hh_profit)
summary(hh_profit_model_basic)

# hh_profit_model_educ_count <- lm(profit ~ noneEducCount + basicEducCount + secEducCount + terEducCount + otherEducCount,
#                            data = hh_profit)

# hh_profit_model_educ_percent <- lm(profit ~ noneEducPercent + basicEducPercent + secEducPercent + terEducPercent + otherEducPercent,
#                                    data = hh_profit)

hh_profit_1 <- hh_profit %>%
  filter(profit <= 5e+08 & profit >= 0)

hh_profit_model_educ_score <- lm(profit ~ avgEdu, data = hh_profit_1)
summary(hh_profit_model_educ_score)
ggplot(hh_profit_1, mapping = aes(avgEdu, profit)) +
  geom_point()



