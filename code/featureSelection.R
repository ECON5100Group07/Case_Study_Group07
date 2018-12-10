library(tidyverse)
library(haven)


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

readData("./raw_data", "./raw_data/+|.dta")
readData("./raw_data/aggregates/", "./raw_data/aggregates/+|.dta")
readData("./raw_data/community/", "./raw_data/community/+|.dta")
readData("./raw_data/prices/", "./raw_data/prices/+|.dta")

###### calculate profit: begin ######
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
  select(clust, nh, agri1, hhagdepn) %>%
  filter(agri1 != 0) %>%
  inner_join(land_size_info, by = c("clust", "nh")) %>%
  mutate(profit = (agri1 - hhagdepn) / landSize) %>%
  select(-agri1, -hhagdepn, -landSize)

attr(hh_profit_info$profit, "label") <- "HH agri profit"

### calculate profit: end ###

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


### aggregate by (clust, nh), generate (mean, sum) of one feature ######

GetAggFeature <- function(data, dataCol){
  aggData <- data %>% 
     select(c("clust", "nh", dataCol)) %>%
     group_by(clust, nh) %>% 
     summarise_at(c(3), funs(mean, sum))
  
  names(aggData)[3] <- paste(dataCol, colnames(aggData)[3], sep="_")
  names(aggData)[4] <- paste(dataCol, colnames(aggData)[4], sep="_")

  return (aggData)
}

### add feature to table ###
AddFeature <- function(data, feature){
  all_features <- data %>%
    left_join(feature, by=c("clust", "nh"))  %>%
    replace(., is.na(.), 0)
  return (all_features)
}

### add feature from file in batch mode ###
AddAggFeaturesFromFiles <- function(data, path, namePattern) {
  files <- list.files(path, full.names = TRUE, pattern = namePattern)
  for (i in 1:length(files)) {
    file <- files[i]
    data <- AddFeature(data, read_dta(file))
  }
  return (data)
}


### generate all features from aggregates data ###
all_features <- hh_basic_info %>%
  AddFeature(GetAggFeature(exp1, "educexp"))  %>%
  AddFeature(GetAggFeature(exp11, "nfinp"))  %>%
  AddFeature(GetAggFeature(exp10, "foodexp"))  %>%
  AddFeature(GetAggFeature(exp11, "nfinp"))  %>%
  AddFeature(GetAggFeature(exp12, "eqdepn"))  %>%
  AddFeature(GetAggFeature(exp13, "assdepn"))  %>%
  AddFeature(GetAggFeature(exp14, "remitexp"))  %>%
  AddFeature(exp15)  %>%
  AddFeature(GetAggFeature(exp16, "useval"))  %>%
  AddFeature(exp17)  %>%
  AddFeature(exp18)  %>%
  AddFeature(exp19)  %>%
  AddFeature(exp2)  %>%
  AddFeature(exp20)  %>%
  AddFeature(exp21)  %>%
  AddFeature(GetAggFeature(exp3, "landexp"))  %>%
  AddFeature(GetAggFeature(exp4, "cropexp"))  %>%
  AddFeature(GetAggFeature(exp5, "livexp"))  %>%
  AddFeature(GetAggFeature(exp6, "fdprexp1"))   %>%#miss one column
  AddFeature(GetAggFeature(exp7, "hp"))  %>%
  AddFeature(GetAggFeature(exp8, "yrexp"))  %>%
  AddFeature(GetAggFeature(exp9, "dayexp"))  %>%
  AddFeature(GetAggFeature(inc1, "schol"))  %>%
  AddFeature(GetAggFeature(inc10, "cropsv1"))   %>%#miss one column
  AddFeature(GetAggFeature(inc11, "rootsv"))  %>%
  AddFeature(GetAggFeature(inc12, "othaginc"))  %>%
  AddFeature(GetAggFeature(inc13, "inctrcrp"))  %>%
  AddFeature(GetAggFeature(inc14, "incnfc"))   %>%#miss 7 columns
  AddFeature(GetAggFeature(inc15, "increm"))  %>%
  AddFeature(inc16)  %>%
  AddFeature(GetAggFeature(inc2, "j1cash"))  %>% #miss more columns
  AddFeature(GetAggFeature(inc3, "j2cash"))  %>% #miss more columns
  AddFeature(GetAggFeature(inc4, "j3cash"))  %>% #miss more columns
  AddFeature(GetAggFeature(inc5, "j4cash"))  %>% #miss more columns
  AddFeature(inc6)  %>%
  AddFeature(inc7)  %>%
  AddFeature(GetAggFeature(inc8, "incliv"))  %>%
  AddFeature(GetAggFeature(inc9, "inceq"))  %>%
  AddAggFeaturesFromFiles("./raw_data/aggregates/", "^subagg[0-9]*.dta") %>%
  AddAggFeaturesFromFiles("./raw_data/aggregates/", "^agg[0-9]*.dta")



# household basic information
# ## ----tidy community information----------------------------------------------------------

hh_profit <- hh_profit_info %>%
 inner_join(all_features, by=c("clust", "nh")) %>%
 select(-region, -district, -eanum, -clust, -nh, -reslan, -ez, -loc2, -loc3, -loc5) 
 


## correlation ###
findCorrelation <- function(a) {
  df <- data.frame(index = (NA), colName=(NA), correlation = (NA))
  for (i in 1:ncol(a)) {
    correlationP <- cor(a[i], a[1])
    row <- c(i, colnames(a[i]),correlationP)
    df<- rbind(df, row) 
  }
  df <- df %>%
    filter(!is.na(colName))
  df <- df[order(df$correlation, decreasing = T),]
  return (df)
}

all_correlations <- findCorrelation(hh_profit)

topFeatures <- hh_profit[c(all_correlations$colName[1:15])]
hh_model_topfeatures <- lm(profit ~ ., data = topFeatures)
summary(hh_model_topfeatures)

allCorrelationsFiltered <- all_correlations %>%
  filter(colName !="agri1" 
         &colName !="agri1c"
         &colName !="agri2"
         &colName !="agri2c"
         &colName !="inctrcrp_sum"
         &colName !="inctrcrp_mean"
         &colName !="crpinc1"
         &colName !="crpinc2" 
         &colName !="rootinc"
         &colName !="incothag" 
         &colName !="trcrpinc"
         &colName !="homepro" 
         &colName !="expland" 
         &colName !="excrop" 
         &colName !="exliv" 
         &colName !="fdprexp1_mean" 
         &colName !="fdprexp1_sum" 
         &colName !="expfdpr1" 
         &colName !="expfdpr2" 
         &colName !="depneq")

topFeaturesFiltered <- hh_profit[c(allCorrelationsFiltered$colName[1:15])]
hh_model_topfeatures_filtered <- lm(profit ~ ., data = topFeaturesFiltered)
summary(hh_model_topfeatures_filtered)
