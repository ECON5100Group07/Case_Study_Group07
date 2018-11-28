library(tidyverse)
library(haven)
library(car)

## ----change this directory to your own folder containing glss4 folder---------------
setwd("/Users/zhangsiqi/econ_5100/Assignment/Case_Study")   #???? put under raw_data folder

## ----read all data------------------------------------------------------------------
# function to read all data
readData <- function(path, namePattern) {
  files <- list.files(path, full.names = TRUE, pattern = ".dta")
  for (i in 1:length(files)) {
    file <- files[i]
    data <- read_dta(file)
    name <- gsub(namePattern, "", file)
    assign(name, data, envir = .GlobalEnv)
  }
}

readData("./glss4/", "./glss4/+|.dta")
readData("./glss4/aggregates/", "./glss4/aggregates/+|.dta")
readData("./glss4/community/", "./glss4/community/+|.dta")
readData("./glss4/prices/", "./glss4/prices/+|.dta")

## ----calculate agricultural profit per area unit (the y variable)----------------------------------
# check correlation between agricultural income and corrected agricultural income
cor(agg2$agri1, agg2$agri1c)
cor(agg2$agri2, agg2$agri2c)

# identify count of NAs in agg2
colSums(is.na(agg2))

# map land size units to acre with a multiplier (s8bq4bm)
# the "Other" unit is treated as NA
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
  filter(landSize >= 1)

# resolve warnings
attr(agg2$clust, "label") <- "Enumeration Area number"
attr(agg2$nh, "label") <- "Household ID"

# calculate household agri profit per acre
# because the above correlations are both 1,
# use only "agri1" and "agri2" to calculate agricultural profit
hh_profit_info <- agg2 %>%
  select(clust, nh, agri1, hhagdepn) %>%
  filter(agri1 != 0) %>%
  inner_join(land_size_info, by = c("clust", "nh")) %>%
  mutate(profit = agri1 / landSize) %>%
  select(-agri1, -hhagdepn, -landSize)

attr(hh_profit_info$profit, "label") <- "HH agri profit"

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
hhm_info <- sec1 %>%
  select(clust, nh, pid, sex, agey, rel) %>%
  filter(agey >= 15 & agey <= 65) %>%
  mutate(female = sex == 2,
         age = agey) %>%
  select(-sex, -agey)

# household member education information
# map education qualification to education level
educ_level_map <- data.frame("s2aq3" = c(1:14, 96),
                             "s2aq3l" = c(1, 2, rep(3, 4), rep(4, 8), 5))

# resolve warnings
attr(educ_level_map$s2aq3, "label") <- "Highest educ qualification"
attr(educ_level_map$s2aq3, "format.stata") <- "%10.0g"
attributes(sec2a$s2aq3)
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
  filter(rel == 1) %>%
  select(-pid, -rel)

## ----tidy agricultural characteristics information---------------------------------------
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

# count household agric equipment type
# not including count of each equipment because of too many missing value (s8aq34)
hh_equip_info <- sec8a3 %>%
  select(clust, nh, eqcdown) %>%
  group_by(clust, nh) %>%
  summarise(equipTypeCount = n())

# spread household havested crop count and count havested crop type
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
  inner_join(hh_head_info, by=c("clust", "nh")) #%>%
  #inner_join(hh_agri_info, by=c("clust", "nh")) #%>%
  #replace(., is.na(.), 0)

hh_profit <- hh_profit_info %>%
  inner_join(hh_all_info, by=c("clust", "nh")) %>%
  select(-region, -district, -clust, -nh)

# function to check correlated variables and test null hypothesis
aliasAndTestHnull <- function(model) {
  model_summary <- summary(model)
  print("===================== model summary =======================", quote = F)
  print(model_summary)
  model_alias <- alias(model)
  print("===================== model alias =========================", quote = F)
  print(model_alias)
  # if there is no correlated variables, test hypothesis
  if (is.null(model_alias$Complete)) {
    model_coef <- rownames(model_summary$coefficients)[-1]
    hnull <- paste0(model_coef, rep(" = 0", length(model_coef)))
    print("===================== hypothesis test =====================", quote = F)
    linearHypothesis(model, hnull)
  } else {
    warning("There are correlated variables. See above model alias")
  }
}
# fit model and test hypothesis
hh_profit_model_ur <- lm(profit ~ .,
                         data = hh_profit)
aliasAndTestHnull(hh_profit_model_ur)

# remove correlated variables
hh_profit_model_r1 <- lm(profit ~ . - loc5 - loc3,
                         data = hh_profit)
aliasAndTestHnull(hh_profit_model_r1)

## ----back up code--------------------------------------------------------------------------
# ## ----tidy household member information---------------------------------------------------
# # household member information
# hhm_info <- sec1 %>%
#   select(clust, nh, pid, sex, agey) %>%
#   filter(agey >= 15 & agey <= 60) %>%
#   mutate(sex = factor(sex,
#                       levels = as.character(c(1,2)),
#                       labels = c("Male", "Female"))
#   )
# 
# # household member education information
# # map education qualification to education level
# educ_level_map <- data.frame("s2aq3" = c(1:14, 96),
#                              "s2aq3l" = c(0, 1, rep(2, 4), rep(3, 8), 1.5))
# 
# hhm_educ <- sec2a %>%
#   select(clust, nh, pid, s2aq3) %>%
#   left_join(educ_level_map) %>%
#   select(-s2aq3) %>%
#   replace(., is.na(.), 0)
# # mutate(s2aq3l = factor(s2aq3l,
# #                        levels = as.character(c(1:5)),
# #                        labels = c("None", "Basic Education", "Secondary Education",
# #                                   "Tertiary Education", "Other")))
# 
# # hh_member_info <- hhm_info %>%
# #   full_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
# #   group_by(clust, nh) %>%
# #   summarise(malePercent = sum(sex == "Male")/n(),
# #             femalePercent = sum(sex == "Female")/n(),
# #             avgAge = mean(agey),
# #             noneEducPercent = sum(s2aq3l == "None", na.rm = TRUE)/n(),
# #             basicEducPercent = sum(s2aq3l == "Basic Education", na.rm = TRUE)/n(),
# #             secEducPercent = sum(s2aq3l == "Secondary Education", na.rm = TRUE)/n(),
# #             terEducPercent = sum(s2aq3l == "Tertiary Education", na.rm = TRUE)/n(),
# #             otherEducPercent = sum(s2aq3l == "Other", na.rm = TRUE)/n())
# # hh_member_info <- hhm_info %>%
# #   left_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
# #   group_by(clust, nh) %>%
# #   summarise(maleCount = sum(sex == "Male"),
# #             femaleCount = sum(sex == "Female"),
# #             avgAge = mean(agey),
# #             noneEducCount = sum(s2aq3l == "None", na.rm = TRUE),
# #             basicEducCount = sum(s2aq3l == "Basic Education", na.rm = TRUE),
# #             secEducCount = sum(s2aq3l == "Secondary Education", na.rm = TRUE),
# #             terEducCount = sum(s2aq3l == "Tertiary Education", na.rm = TRUE),
# #             otherEducCount = sum(s2aq3l == "Other", na.rm = TRUE))
# hh_member_info <- hhm_info %>%
#   left_join(hhm_educ, by=c("clust", "nh", "pid")) %>%
#   replace(., is.na(.), 0) %>%
#   group_by(clust, nh) %>%
#   summarise(maleCount = sum(sex == "Male"),
#             femaleCount = sum(sex == "Female"),
#             avgAge = mean(agey),
#             avgEdu = mean(s2aq3l))