library(tidyverse)
library(haven)
library(car)
library(here)

## ----set directory to this project--------------------------------------------------
setwd(here())

## ----read all data------------------------------------------------------------------
agg2 <- read_dta(here("raw_data/aggregates","agg2.dta"))
sec8b <- read_dta(here("raw_data","sec8b.dta"))
sec0a <- read_dta(here("raw_data","sec0a.dta"))
sec1 <- read_dta(here("raw_data","sec1.dta"))
sec2a <- read_dta(here("raw_data","sec2a.dta"))
sec8a2 <- read_dta(here("raw_data","sec8a2.dta"))
sec8a3 <- read_dta(here("raw_data","sec8a3.dta"))
sec8c1 <- read_dta(here("raw_data","sec8c1.dta"))
sec8c2 <- read_dta(here("raw_data","sec8c2.dta"))
cs2 <- read_dta(here("raw_data/community","cs2.dta"))

## ----calculate agricultural profit per area unit (the y variable)-------------------
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
  # remove household with small land size
  filter(landSize >= 0.5)

# resolve warnings
attr(agg2$clust, "label") <- "Enumeration Area number"
attr(agg2$nh, "label") <- "Household ID"

# check correlation between agricultural income and corrected agricultural income
cor(agg2$agri1, agg2$agri1c)

# calculate household agri profit per acre
# because the above correlations are both 1,
# use only "agri1" to calculate agricultural profit
hh_profit_info <- agg2 %>%
  select(clust, nh, agri1) %>%
  filter(agri1 != 0) %>%
  inner_join(land_size_info, by = c("clust", "nh")) %>%
  mutate(profit = agri1 / landSize) %>%
  select(-agri1, -landSize)

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
  #filter(agey >= 15 & agey <= 65) %>%
  group_by(clust,nh) %>%
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
         sep = "") %>%
  mutate(sqrtLivstcd1 = sqrt(livstcd1 + 1),
         sqrtLivstcd2 = sqrt(livstcd2 + 1),
         sqrtLivstcd3 = sqrt(livstcd3 + 1),
         sqrtLivstcd4 = sqrt(livstcd4 + 1),
         sqrtLivstcd5 = sqrt(livstcd5 + 1),
         sqrtLivstcd6 = sqrt(livstcd6 + 1),
         sqrtLivstcd7 = sqrt(livstcd7 + 1),
         sqrtLivstcd8 = sqrt(livstcd8 + 1),
         sqrtLivstcd9 = sqrt(livstcd9 + 1),
         sqrtLivstcd10 = sqrt(livstcd10 + 1),
         sqrtLivstcd11 = sqrt(livstcd11 + 1),
         sqrtLivstcd12 = sqrt(livstcd12 + 1),
         sqrLivstcd1 = (livstcd1)^2,
         sqrLivstcd2 = (livstcd2)^2,
         sqrLivstcd3 = (livstcd3)^2,
         sqrLivstcd4 = (livstcd4)^2,
         sqrLivstcd5 = (livstcd5)^2,
         sqrLivstcd6 = (livstcd6)^2,
         sqrLivstcd7 = (livstcd7)^2,
         sqrLivstcd8 = (livstcd8)^2,
         sqrLivstcd9 = (livstcd9)^2,
         sqrLivstcd10 = (livstcd10)^2,
         sqrLivstcd11 = (livstcd11)^2,
         sqrLivstcd12 = (livstcd12)^2)
summary(hh_livestock_info)


# count household agric equipment type
# not including count of each equipment because of too many missing value (s8aq34)
hh_equip_info <- sec8a3 %>%
  select(clust, nh, eqcdown, s8aq34) %>%
  replace(., . > 1e+100, 0)%>%
  filter(s8aq34 != 0) %>%
  group_by(clust, nh) %>%
  mutate(equipTypeCount = n()) %>%
  spread(key = eqcdown,
         value = s8aq34,
         fill = 0,
         sep = "") %>%
  mutate(sqrteqcdown21 = sqrt(eqcdown21 + 1),
         sqrteqcdown22 = sqrt(eqcdown22 + 1),
         sqrteqcdown31 = sqrt(eqcdown31 + 1),
         sqrteqcdown51 = sqrt(eqcdown51 + 1),
         sqrteqcdown61 = sqrt(eqcdown61 + 1),
         sqrteqcdown62 = sqrt(eqcdown62 + 1),
         sqrteqcdown63 = sqrt(eqcdown63 + 1),
         sqrteqcdown64 = sqrt(eqcdown64 + 1),
         sqrteqcdown65 = sqrt(eqcdown65 + 1),
         sqreqcdown21 = (eqcdown21)^2,
         sqreqcdown22 = (eqcdown22)^2,
         sqreqcdown31 = (eqcdown31)^2,
         sqreqcdown51 = (eqcdown51)^2,
         sqreqcdown61 = (eqcdown61)^2,
         sqreqcdown62 = (eqcdown62)^2,
         sqreqcdown63 = (eqcdown63)^2,
         sqreqcdown64 = (eqcdown64)^2,
         sqreqcdown65 = (eqcdown65)^2)
summary(hh_equip_info) 
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
         sep = "") %>%
  mutate(sqrtcropcd8 = sqrt(cropcd8 + 1),
         sqrtcropcd11 = sqrt(cropcd11 + 1),
         sqrtcropcd25 = sqrt(cropcd25 + 1),
         sqrcropcd8 = (cropcd8 )^2,
         sqrcropcd11 = (cropcd11 )^2,
         sqrcropcd25 = (cropcd25)^2)
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

## ----combine all information and fit model-----------------------------------------------
hh_all_info <- hh_basic_info %>%
  inner_join(hh_head_info, by=c("clust", "nh")) %>%
  inner_join(hh_comm_info, by=c("region", "district", "eanum")) %>%
  inner_join(hh_agri_info, by=c("clust", "nh"))

hh_profit <- hh_profit_info %>%
  inner_join(hh_all_info, by=c("clust", "nh")) %>%
  select(-region, -district, -eanum, -clust, -nh)

####filter out rural and urban data from hh_profit
hh_profit_rural <- hh_profit %>%
  filter(loc2 == "Rural")

hh_profit_urban <- hh_profit %>%
  filter(loc2 == "Urban")

# function to check correlated variables and test null hypothesis
checkCorrVarAndTestHnull <- function(model) {
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
# fit unrestricted model and test hypothesis
hh_profit_model_ur <- lm(profit ~ .,
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_ur)

# fit rural model and test hypothesis
hh_profit_model_rural <- lm(profit ~ .,
                            data = hh_profit_rural)
checkCorrVarAndTestHnull(hh_profit_model_rural)

# fit urban model and test hypothesis
hh_profit_model_urban <- lm(profit ~ .,
                            data = hh_urban)
checkCorrVarAndTestHnull(hh_profit_model_urban)

# fit restricted model and test hypothesis
hh_profit_model_r1 <- lm(profit ~ reslan + ez + educ + market + livstcd5 + livstcd6 +
                           equipTypeCount + cropTypeCount + cropcd8 + cropcd11 +
                           cropcd25 + rootcd8 + rootcd18 + rootcd20 + rootcd27 +
                           + rootcd33,
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r1)

# fit restricted model with only education and local characteristic information and test hypothesis
hh_profit_model_r2 <- lm(profit ~ educ + ez + loc2 + loc5 + loc3 + market + transport, 
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r2)

# remove loc5 and loc3 and fit model again
hh_profit_model_r3 <- lm(profit ~ educ + ez + loc2 + market + transport, 
                         data = hh_profit)
checkCorrVarAndTestHnull(hh_profit_model_r3)

## ----regression diagnostics-----------------------------------------------
#01.unrestricted model:
# Standardised residuals
hh_profit <- hh_profit %>%
  mutate(
    stand_res = rstandard(hh_profit_model_ur)
  )
hh_profit %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")
ggsave(here("figures", "diag_ur_stand_res.png"))

# Constant variance
hh_profit$fitted <- hh_profit_model_ur$fitted.values
hh_profit$residuals <- hh_profit_model_ur$residuals
hh_profit  %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 
ggsave(here("figures", "diag_ur_homoskedasticity.png"))

#02restricted model:
# Standardised residuals
hh_profit <- hh_profit %>%
  mutate(
    stand_res = rstandard(hh_profit_model_r1)
  )
hh_profit %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")
ggsave(here("figures", "diag_r1_stand_res.png"))

# Constant variance
hh_profit$fitted <- hh_profit_model_r1$fitted.values
hh_profit$residuals <- hh_profit_model_r1$residuals
hh_profit  %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 
ggsave(here("figures", "diag_r1_homoskedasticity.png"))

#03restricted model with only education and local characteristic information :
# Standardised residuals
hh_profit <- hh_profit %>%
  mutate(
    stand_res = rstandard(hh_profit_model_r2)
  )
hh_profit %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")
ggsave(here("figures", "diag_r2_stand_res.png"))

# Constant variance
hh_profit$fitted <- hh_profit_model_r2$fitted.values
hh_profit$residuals <- hh_profit_model_r2$residuals
hh_profit  %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 
ggsave(here("figures", "diag_r2_homoskedasticity.png"))

#04 restricted model after removing loc5 and loc3  :
# Standardised residuals
hh_profit <- hh_profit %>%
  mutate(
    stand_res = rstandard(hh_profit_model_r3)
  )
hh_profit %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")
ggsave(here("figures", "diag_r3_stand_res.png"))

# Constant variance
hh_profit$fitted <- hh_profit_model_r3$fitted.values
hh_profit$residuals <- hh_profit_model_r3$residuals
hh_profit  %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point() 
ggsave(here("figures", "diag_r3_homoskedasticity.png"))

