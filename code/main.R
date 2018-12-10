library(tidyverse)
library(haven)
library(car)
library(here)

setwd(here())

source(here("code","helperFunc.R"))

## ----read raw data------------------------------------------------------------------
# list of data file pairs to read, first is subfolder, second is file name
datafiles <- list(c("aggregates/", "agg2"),
                  c("", "sec8b"),
                  c("", "sec0a"),
                  c("", "sec1"),
                  c("", "sec2a"),
                  c("", "sec8a2"),
                  c("", "sec8a3"),
                  c("", "sec8c1"),
                  c("", "sec8c2"),
                  c("community/", "cs2"))

invisible(sapply(datafiles, readRawData))

## ----calculate agricultural profit per area unit (the y variable)-------------------
# map land size units to acre with a multiplier (s8bq4bm)
# the "Other" unit is treated as NA
table(sec8b$s8bq4b)
land_size_unit_map <- data.frame("s8bq4b" = as.double(c(1:4)),
                                 "s8bq4bm" = c(1, 1, 1/9, NA))

# convert all land size to acre
land_size_info <- sec8b %>%
  select(clust, nh, s8bq4a, s8bq4b) %>%
  mutate(s8bq4a = ifelse(s8bq4a > 1e+100, NA, s8bq4a),
         s8bq4b = ifelse(s8bq4b > 1e+100, NA, s8bq4b)) %>%
  left_join(land_size_unit_map) %>%
  # s8bq4ac is corrected s8bq4a, which is land size in acre
  mutate(s8bq4ac = s8bq4a * s8bq4bm) %>%
  # convert NA to 0 to calculate sum of land size for each household
  replace(., is.na(.), 0) %>%
  group_by(clust, nh) %>%
  summarise(landSize = sum(s8bq4ac)) %>%
  # remove household with small land size
  filter(landSize >= 0.5)

# identify count of NAs in agg2
colSums(is.na(agg2))

# check correlation between agricultural income and corrected agricultural income
cor(agg2$agri1, agg2$agri1c)

# resolve warnings during join
attr(agg2$clust, "label") <- "Enumeration Area number"
attr(agg2$nh, "label") <- "Household ID"

# calculate household agri profit per acre
# because the above correlations are both 1,
# use only "agri1" to calculate agricultural profit
hh_profit_info <- agg2 %>%
  select(clust, nh, agri1, hhagdepn) %>%
  filter(agri1 != 0) %>%
  inner_join(land_size_info, by = c("clust", "nh")) %>%
  mutate(profit = (agri1 - hhagdepn) / landSize) %>%
  select(-agri1, -hhagdepn, -landSize)

attr(hh_profit_info$profit, "label") <- "HH agri profit"

## ----tidy household basic information----------------------------------------------------------
# household basic information
hh_basic_info <- sec0a %>%
  select(region, district, eanum, clust, nh, reslan, ez:loc3) %>%
  mutate(reslan = cleanAndFactorize(reslan, 1e+100, 99,
                                    levels = as.character(c(1:8, 99)),
                                    labels = c("English", "Akan", "Ewe", "GaAdangbe",
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
hhm_info <- sec1 %>%
  select(clust, nh, pid, sex, agey, rel) %>%
  group_by(clust, nh) %>%
  mutate(female = sex == 2,
         age = agey,
         avgAge = mean(agey),
         maxAge = max(agey),
         minAge = min(agey)
  ) %>%
  select(-sex, -agey)

# household member education information
# map education qualification to education level
educ_level_map <- data.frame("s2aq3" = c(1:14, 96),
                             "s2aq3l" = c(1, 2, rep(3, 4), rep(4, 8), 5))

# resolve warnings
attr(educ_level_map$s2aq3, "label") <- "Highest educ qualification"
attr(educ_level_map$s2aq3, "format.stata") <- "%10.0g"

# Check NAs
colSums(is.na(sec2a))

hhm_educ <- sec2a %>%
  select(clust, nh, pid, s2aq1, s2aq3) %>%
  # if never attended school (s2aq1 == 2), set education to None (s2aq3 = 1)
  mutate(s2aq3 = ifelse(s2aq1 == 2, 1, s2aq3)) %>%
  left_join(educ_level_map) %>%
  mutate(educ = factor(s2aq3l,
                       levels = as.character(c(1:5)),
                       labels = c("None", "BasicEducation", "SecondaryEducation",
                                  "TertiaryEducation", "Other"))) %>%
  select(-s2aq1, -s2aq3, -s2aq3l)

hh_head_info <- hhm_info %>%
  inner_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
  filter(rel == 1) %>% # head of household
  select(-pid, -rel)

## ----tidy agricultural characteristics information---------------------------------------
table(sec8a2$s8aq22b)
# spread livestock count and count livestock type
# since there are too many missing value in livestock unit of messaure (s8aq22b),
# we assume same livestock type has same unit,
# and we only take livestock count (s8aq22a) into consideration
hh_livestock_info <- sec8a2 %>%
  select(clust, nh, livstcd, s8aq22a) %>%
  group_by(clust, nh) %>%
  mutate(livstcdTypeCount = n()) %>%
  spread(key = livstcd,
         value = s8aq22a,
         fill = 0,
         sep = "")
summary(hh_livestock_info)

# spread agricultural equipment count and count agricultural equipment type
hh_equip_info <- sec8a3 %>%
  select(clust, nh, eqcdown, s8aq34) %>%
  replace(., . > 1e+100, 0) %>%
  filter(s8aq34 != 0) %>%
  group_by(clust, nh) %>%
  mutate(equipTypeCount = n()) %>%
  spread(key = eqcdown,
         value = s8aq34,
         fill = 0,
         sep = "")
summary(hh_equip_info)

# spread household havested crop count and count havested crop type
# some households have multiple entries of same crop type, for example:
(sec8c1 %>% select(clust, nh, cropcd, s8cq3a))[c(377, 378),]
# so need to sum up quantity of same crop type
hh_crop_info <- sec8c1 %>%
  select(clust, nh, cropcd, s8cq3a) %>%
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
summary(hh_crop_info)

# spread household havested root count and count havested root type
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
summary(hh_root_info)

hh_agri_info <- hh_livestock_info %>%
  full_join(hh_equip_info, by=c("clust", "nh")) %>%
  full_join(hh_crop_info, by=c("clust", "nh")) %>%
  full_join(hh_root_info, by=c("clust", "nh")) %>%
  replace(., is.na(.), 0)

## ----tidy community information----------------------------------------------------------
hh_comm_info <- cs2 %>%
  select(region, district, eanum, s2q19, s2q23) %>%
  mutate(market = ifelse(s2q19 == 1, TRUE, FALSE),
         transport = ifelse(s2q23 == 1, TRUE, FALSE)) %>%
  select(-s2q19, -s2q23)

## ----combine all information-------------------------------------------------------------
hh_all_info <- hh_basic_info %>%
  inner_join(hh_head_info, by=c("clust", "nh")) %>%
  inner_join(hh_comm_info, by=c("region", "district", "eanum")) %>%
  inner_join(hh_agri_info, by=c("clust", "nh"))

hh_profit <- hh_profit_info %>%
  inner_join(hh_all_info, by=c("clust", "nh")) %>%
  select(-region, -district, -eanum, -clust, -nh)

# filter out rural and urban data from hh_profit
hh_profit_rural <- hh_profit %>%
  filter(loc2 == "Rural")
# remove factor type column with only one value
hh_profit_rural <- Filter(function(x) !isSingleValueFactorColumn(x), hh_profit_rural)

hh_profit_urban <- hh_profit %>%
  filter(loc2 == "Urban")
# remove factor type column with only one value
hh_profit_urban <- Filter(function(x) !isSingleValueFactorColumn(x), hh_profit_urban)

## ----fit model and regression diagnostics--------------------------------------------------------------
# unrestricted model
hh_profit_model_ur <- lm(profit ~ .,
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_ur)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_ur, "lightblue", "UR"))

# restricted model with significant variables
hh_profit_model_r1 <- lm(profit ~ reslan + ez + age + market + livstcd5 + livstcd6 + livstcd7 + livstcd10 +
                           equipTypeCount + eqcdown61 + cropcd5 + cropcd8 + cropcd11 + cropcd25 + cropcd29 +
                           rootcd7 + rootcd18 + rootcd20 + rootcd27 + rootcd33 + rootcd36,
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r1)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_r1, "darkblue", "R1"))

# restricted model with top features from agricultural characteristics information
hh_profit_agri <- hh_profit[, -c(2:14)] # get agricultural characteristics variables
all_correlations <- findAbsoluteCorrelation(hh_profit_agri)
hh_profit_agri_topFeatures <- hh_profit[c("profit", all_correlations$colName[1:15])]
hh_profit_model_topfeatures <- lm(profit ~ . , data = hh_profit_agri_topFeatures)
checkCorrVarAndTestHnull(hh_profit_model_topfeatures)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_topfeatures, "purple", "TOP"))

# unrestricted model for rural area
hh_profit_model_rural <- lm(profit ~ .,
                            data = hh_profit_rural)
checkCorrVarAndTestHnull(hh_profit_model_rural)
suppressWarnings(plotResiduals(hh_profit_rural, hh_profit_model_rural, "green", "RURAL"))

# unrestricted model for urban area
hh_profit_model_urban <- lm(profit ~ .,
                            data = hh_profit_urban)
checkCorrVarAndTestHnull(hh_profit_model_urban)
# check data size
dim(hh_profit_urban)

# restricted model with only education and local characteristic information
hh_profit_model_r2 <- lm(profit ~ educ + ez + loc2 + loc5 + loc3 + market + transport, 
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r2)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_r2, "yellow", "R2"))

# remove loc5 and loc3 and fit model again
hh_profit_model_r3 <- lm(profit ~ educ + ez + loc2 + market + transport, 
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r3)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_r3, "orange", "R3"))

# restricted model with educ * age
hh_profit_model_r4 <- lm(profit ~ educ * age + female + ez + loc2 + market + transport, 
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r4)
suppressWarnings(plotResiduals(hh_profit, hh_profit_model_r4, "pink", "R4"))

