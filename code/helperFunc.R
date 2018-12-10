# read raw data
readRawData <- function(datafile) {
  subfolder <- datafile[1]
  filename <- datafile[2]
  path <- paste0(subfolder, filename, ".dta")
  data <- read_dta(here("raw_data", path))
  assign(filename, data, envir = .GlobalEnv)
}

# replace unknown value and factorize data
cleanAndFactorize <- function(data, threshold, replaceValue, levels, labels) {
  data <- ifelse(data > threshold, replaceValue, data)
  data <- factor(data, levels=levels, labels=labels)
  return(data)
}

# check if a column is of type factor and has only one value
isSingleValueFactorColumn <- function(x) {
  return (is.factor(x) && length(unique(x)) == 1)
}

# check correlated variables and test null hypothesis
checkCorrVarAndTestHnull <- function(model) {
  model_summary <- summary(model)
  writeLines("===================== model summary =======================")
  print(model_summary)
  model_alias <- alias(model)
  # if there is no correlated variables, test hypothesis
  if (is.null(model_alias$Complete)) {
    model_coef <- rownames(model_summary$coefficients)[-1]
    hnull <- paste0(model_coef, rep(" = 0", length(model_coef)))
    writeLines("===================== hypothesis test =====================")
    linearHypothesis(model, hnull)
  } else {
    writeLines("===================== model alias =========================")
    print(model_alias)
    warning("There are correlated variables. See above model alias.")
  }
}

# find absolute correlation values
findAbsoluteCorrelation <- function(inputFeatures) {
  df <- data.frame(index = integer(), colName=character(), correlation = double())
  for (i in 2:ncol(inputFeatures)) {
    correlationP <- abs(cor(inputFeatures[i], inputFeatures[1]))
    row <- data.frame(index = i, colName = colnames(inputFeatures[i]), correlation = unname(correlationP), stringsAsFactors = F)
    df <- rbind.data.frame(df, row)
  }
  df <- df %>%
    filter(!is.na(correlation))
  df <- df[order(df$correlation, decreasing = T),]
  return (df)
}

# plot standard residuals and residuals vs fitted values
plotResiduals <- function(data, model, fill, modelName) {
  # standard residuals
  data <- data %>%
    mutate(
      stand_res = rstandard(model)
    )
  p1 <- data %>% ggplot(aes(x = stand_res)) +
    geom_histogram(colour = "black", fill = fill) +
    xlab("Standardized residuals") +
    ggtitle(paste("Standardized residuals of", modelName, "model", sep = " "))
  suppressMessages(ggsave(here("figures", paste("diag", tolower(modelName), "stand_res.png", sep = "_"))))
  
  # residuals vs fitted values
  data$fitted <- model$fitted.values
  data$residuals <- model$residuals
  p2 <- data %>% ggplot(aes(x = fitted, y = residuals)) +
    geom_point(colour = fill) + ggtitle(paste("Homoskedasticity of", modelName, "model", sep = " "))
  suppressMessages(ggsave(here("figures", paste("diag", tolower(modelName), "homoskedasticity.png", sep = "_"))))
  
  suppressMessages(print(p1))
  suppressMessages(print(p2))
}
