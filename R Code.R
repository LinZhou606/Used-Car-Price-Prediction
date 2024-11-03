library(dplyr)
library(tidyverse)
library(randomForest)
library(ranger)
library(xgboost)
library(skimr)
library(caret)
library(stringr)

# read data and Preprocessing=========================
setwd("/Users/eva/Desktop/5200 Applied Analytics Frameworks and Methods I/7. Predictive Analysis Competition Launch")
analysisData = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')
str(analysisData)
skim(analysisData)

# 1 Data Tidying=========================================
# identifying missing values
analysisData[analysisData == ""] <- NA
missing_values <- sapply(analysisData, function(x) sum(is.na(x))/length(x)) * 100

# parsing---------------------------------------------
# converts listed_date
analysisData <- analysisData %>%
  mutate(
    listed_date = as.Date(listed_date),
    list_year = as.integer(format(listed_date, "%Y")), # Convert to integer
    list_month = as.integer(format(listed_date, "%m")), # Convert to integer to remove leading zero
    list_day = as.integer(format(listed_date, "%d")) # Convert to integer
  )
# For categorical columns, impute with the mode (most common value)
analysisData$is_cpo[is.na(analysisData$is_cpo) | analysisData$is_cpo == ""] <- "False"
analysisData$franchise_make[is.na(analysisData$franchise_make) | analysisData$franchise_make == ""] <- "not franchise"
# transmission_display
analysisData$transmission_speed <- ifelse(grepl("\\d+", analysisData$transmission_display), 
                                          as.numeric(gsub("\\D", "", analysisData$transmission_display)), 
                                          0)
# description
analysisData$description_length <- nchar(as.character(analysisData$description))
discount_keywords <- c("off", "discount", "below", "save", "reduction", "deal", "special")
analysisData$is_discount <- sapply(analysisData$description, function(x) {
  any(sapply(discount_keywords, function(kw) grepl(kw, tolower(x), fixed = TRUE)))
})
analysisData$is_discount <- as.integer(analysisData$is_discount)
# major_options
analysisData$options_count <- sapply(strsplit(as.character(analysisData$major_options), ","), function(x) length(gsub("'", "", x)))
analysisData$option_is_premium <- sapply(analysisData$major_options, function(x) {
  grepl("Premium", x)
})
analysisData$option_is_premium <- as.integer(analysisData$option_is_premium)
# engine_type
extract_cylinder_config <- function(engine_type) {
  # extract the first letter
  return(substring(engine_type, 1, 1))
}
extract_cylinder_number <- function(engine_type) {
  # extract all the numbers
  return(as.integer(gsub("[^0-9]", "", engine_type)))
}
analysisData$cylinders_configuration <- sapply(analysisData$engine_type, extract_cylinder_config)
analysisData$cylinders_number <- sapply(analysisData$engine_type, extract_cylinder_number)
# converts columns of character type to factors
analysisData <- analysisData %>%
  mutate_if(is.character, as.factor)
categorical_cols <- sapply(analysisData, is.factor)
analysisData[, categorical_cols] <- lapply(analysisData[, categorical_cols], function(x) {
  if(sum(is.na(x)) > 0) x <- replace(x, is.na(x), names(sort(table(x), decreasing = TRUE))[1])
  return(x)
})
# For numeric columns, using median for numeric columns
analysisData$owner_count[is.na(analysisData$owner_count) | analysisData$owner_count == ""] <- 0
numeric_cols <- sapply(analysisData, is.numeric)
analysisData[, numeric_cols] <- lapply(analysisData[, numeric_cols], function(x) {
  if(sum(is.na(x)) > 0) x <- replace(x, is.na(x), median(x, na.rm = TRUE))
  return(x)
})
# converts Boolean variables stored as characters to numeric values (1 and 0)
analysisData <- analysisData %>%
  mutate_if(is.factor, function(x) {
    if(all(x %in% c("True", "False"), na.rm = TRUE)) {
      as.integer(as.logical(x))
    } else {
      x
    }
  })
# Extracting numeric data from 'power'(Horsepower and RPM) and 'torque'(Torque and RPM)
analysisData$power_hp <- as.numeric(gsub(" hp.*", "", analysisData$power))
analysisData$power_rpm <- sapply(analysisData$power, function(x) {
  rpm <- gsub(".*@ ", "", x)  # delete all content before "@"
  rpm <- gsub(" RPM", "", rpm)  # delete "RPM"
  as.numeric(gsub(",", "", rpm))  # remove commas and convert to numeric values
})
analysisData$torque_lbft <- as.numeric(gsub(" lb-ft.*", "", analysisData$torque))
analysisData$torque_rpm <- sapply(analysisData$torque, function(x) {
  rpm <- gsub(".*@ ", "", x)  
  rpm <- gsub(" RPM", "", rpm) 
  as.numeric(gsub(",", "", rpm))  
})

# Creating a new feature 'car_age' from the year 
analysisData$car_age <- 2023 - analysisData$year 
analysisData <- analysisData %>% select(-power, -torque, -year, -id, -listed_date, -engine_type, -description, -major_options, -wheel_system_display, -transmission_display)

# grouping---------------------------------------------
freq_threshold <- 0.01
# Group 'model_name'
model_freq <- analysisData %>% group_by(model_name) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_models <- model_freq %>% filter(Frequency < freq_threshold) %>% pull(model_name)
analysisData$model_name_grouped <- ifelse(analysisData$model_name %in% infrequent_models, 'Other', as.character(analysisData$model_name))

# Group 'trim_name'
# trim_name
analysisData$trim_name <- str_extract(analysisData$trim_name, "^[^ ]*")
replace_terms <- c("Luxury", "Luxe", "Premier","High")
for(term in replace_terms) {
  analysisData$trim_name <- str_replace_all(analysisData$trim_name, term, "Premium")
}
replace_terms <- c("Classic", "Ultimate")
for(term in replace_terms) {
  analysisData$trim_name <- str_replace_all(analysisData$trim_name, term, "Essence")
}

trim_freq <- analysisData %>% group_by(trim_name) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_trims <- trim_freq %>% filter(Frequency < freq_threshold) %>% pull(trim_name)
analysisData$trim_name_grouped <- ifelse(analysisData$trim_name %in% infrequent_trims, 'Other', as.character(analysisData$trim_name))

# Group 'interior_color'
# interior_color
analysisData$interior_color <- tolower(analysisData$interior_color)
colors_to_standardize <- c("black", "gray", "brown", "red", "white","stone","dark","light","ebony","parchment","charcoal","blue","almond")
for(color in colors_to_standardize) {
  analysisData$interior_color <- ifelse(grepl(color, analysisData$interior_color), color, analysisData$interior_color)
}
# Replace "No Color" and "Unspecified" with "None"
analysisData$interior_color <- str_replace_all(analysisData$interior_color, "no color", "none")
analysisData$interior_color <- str_replace_all(analysisData$interior_color, "unspecified", "none")
analysisData$interior_color <- str_replace_all(analysisData$interior_color, "\\(.*?\\)", "")
analysisData$interior_color <- str_trim(analysisData$interior_color)

color_freq <- analysisData %>% group_by(interior_color) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_colors <- color_freq %>% filter(Frequency < freq_threshold) %>% pull(interior_color)
analysisData$interior_color_grouped <- ifelse(analysisData$interior_color %in% infrequent_colors, 'Other', as.character(analysisData$interior_color))

# Group 'exterior_color'
# exterior_color
replace_color_name <- function(color_name, dataframe, column_name) {
  # Converts color names to lowercase
  color_name_lower <- tolower(color_name)
  pattern <- paste0(".*", color_name_lower, ".*")  
  dataframe[[column_name]] <- str_replace_all(tolower(dataframe[[column_name]]), pattern, color_name_lower)
  return(dataframe)
}
# Define the colors to standardize in the exterior_color column
colors_to_standardize_exterior <- c("white", "blue", "red", "metallic", "black", "grey", "gray", "silver", "pearl", "coat", "orange", "yellow", "green")
for(color in colors_to_standardize_exterior) {
  analysisData <- replace_color_name(color, analysisData, "exterior_color")
}

color_freq <- analysisData %>% group_by(exterior_color) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_colors <- color_freq %>% filter(Frequency < freq_threshold) %>% pull(exterior_color)
analysisData$exterior_color_grouped <- ifelse(analysisData$exterior_color %in% infrequent_colors, 'Other', as.character(analysisData$exterior_color))

analysisData <- analysisData %>% select(-model_name, -trim_name, -interior_color, -exterior_color)

# 2 get feature importance=========================================
# randomForest 
library(ranger)
formula <- as.formula("price ~ .")
rangerModel <- ranger(
  formula,
  data = analysisData,
  importance = 'impurity',  # This computes the importance of each variable
  num.trees = 1000
)
importance <- rangerModel$variable.importance
importance_sorted <- sort(importance, decreasing = TRUE)
top_features <- names(importance_sorted)[1:41]
print(top_features)

# 3 get Model Selection and Training=========================================
# split data set
set.seed(123) 
trainingRows <- createDataPartition(analysisData$price, p = 0.8, list = FALSE)
#trainData <- analysisData[trainingRows, c(top_features, "price")]
#testData <- analysisData[-trainingRows, c(top_features, "price")]
trainData <- analysisData[ , c(top_features, "price")]
# Model Evaluation and Tuning
set.seed(617)
rfModel <- ranger(price ~ ., data = trainData, num.trees = 1000, mtry = 14)
print(rfModel)


#=================================================================================
###scoringData===========================================================================
scoringDataid = scoringData$id
# 1 Data Tidying=========================================
# Identifying missing values
scoringData[scoringData == ""] <- NA
missing_values <- sapply(scoringData, function(x) sum(is.na(x))/length(x)) * 100

# parsing---------------------------------------------
# converts listed_date
scoringData <- scoringData %>%
  mutate(
    listed_date = as.Date(listed_date),
    list_year = as.integer(format(listed_date, "%Y")), # Convert to integer
    list_month = as.integer(format(listed_date, "%m")), # Convert to integer to remove leading zero
    list_day = as.integer(format(listed_date, "%d")) # Convert to integer
  )
# For categorical columns, impute with the mode (most common value)
scoringData$is_cpo[is.na(scoringData$is_cpo) | scoringData$is_cpo == ""] <- "False"
scoringData$franchise_make[is.na(scoringData$franchise_make) | scoringData$franchise_make == ""] <- "not franchise"
# transmission_display
scoringData$transmission_speed <- ifelse(grepl("\\d+", scoringData$transmission_display), 
                                          as.numeric(gsub("\\D", "", scoringData$transmission_display)), 
                                          0)
# description
scoringData$description_length <- nchar(as.character(scoringData$description))
discount_keywords <- c("off", "discount", "below", "save", "reduction", "deal", "special")
scoringData$is_discount <- sapply(scoringData$description, function(x) {
  any(sapply(discount_keywords, function(kw) grepl(kw, tolower(x), fixed = TRUE)))
})
scoringData$is_discount <- as.integer(scoringData$is_discount)
# major_options
scoringData$options_count <- sapply(strsplit(as.character(scoringData$major_options), ","), function(x) length(gsub("'", "", x)))
scoringData$option_is_premium <- sapply(scoringData$major_options, function(x) {
  grepl("Premium", x)
})
scoringData$option_is_premium <- as.integer(scoringData$option_is_premium)
# engine_type
extract_cylinder_config <- function(engine_type) {
  # extract the first letter
  return(substring(engine_type, 1, 1))
}
extract_cylinder_number <- function(engine_type) {
  # extract all the numbers
  return(as.integer(gsub("[^0-9]", "", engine_type)))
}
scoringData$cylinders_configuration <- sapply(scoringData$engine_type, extract_cylinder_config)
scoringData$cylinders_number <- sapply(scoringData$engine_type, extract_cylinder_number)
# converts columns of character type to factors
scoringData <- scoringData %>%
  mutate_if(is.character, as.factor)
categorical_cols <- sapply(scoringData, is.factor)
scoringData[, categorical_cols] <- lapply(scoringData[, categorical_cols], function(x) {
  if(sum(is.na(x)) > 0) x <- replace(x, is.na(x), names(sort(table(x), decreasing = TRUE))[1])
  return(x)
})
# For numeric columns, using median for numeric columns
scoringData$owner_count[is.na(scoringData$owner_count) | scoringData$owner_count == ""] <- 0
numeric_cols <- sapply(scoringData, is.numeric)
scoringData[, numeric_cols] <- lapply(scoringData[, numeric_cols], function(x) {
  if(sum(is.na(x)) > 0) x <- replace(x, is.na(x), median(x, na.rm = TRUE))
  return(x)
})
# converts Boolean variables stored as characters to numeric values (1 and 0)
scoringData <- scoringData %>%
  mutate_if(is.factor, function(x) {
    if(all(x %in% c("True", "False"), na.rm = TRUE)) {
      as.integer(as.logical(x))
    } else {
      x
    }
  })
# Extracting numeric data from 'power'(Horsepower and RPM) and 'torque'(Torque and RPM)
scoringData$power_hp <- as.numeric(gsub(" hp.*", "", scoringData$power))
scoringData$power_rpm <- sapply(scoringData$power, function(x) {
  rpm <- gsub(".*@ ", "", x)  # delete all content before "@"
  rpm <- gsub(" RPM", "", rpm)  # delete "RPM"
  as.numeric(gsub(",", "", rpm))  # remove commas and convert to numeric values
})
scoringData$torque_lbft <- as.numeric(gsub(" lb-ft.*", "", scoringData$torque))
scoringData$torque_rpm <- sapply(scoringData$torque, function(x) {
  rpm <- gsub(".*@ ", "", x)  
  rpm <- gsub(" RPM", "", rpm) 
  as.numeric(gsub(",", "", rpm))  
})

# Creating a new feature 'car_age' from the year 
scoringData$car_age <- 2023 - scoringData$year 
scoringData <- scoringData %>% select(-power, -torque, -year, -id, -listed_date, -engine_type, -description, -major_options, -wheel_system_display, -transmission_display)

# grouping---------------------------------------------
freq_threshold <- 0.01
# Group 'model_name'
model_freq <- scoringData %>% group_by(model_name) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_models <- model_freq %>% filter(Frequency < freq_threshold) %>% pull(model_name)
scoringData$model_name_grouped <- ifelse(scoringData$model_name %in% infrequent_models, 'Other', as.character(scoringData$model_name))

# Group 'trim_name'
# trim_name
scoringData$trim_name <- str_extract(scoringData$trim_name, "^[^ ]*")
replace_terms <- c("Luxury", "Luxe", "Premier","High")
for(term in replace_terms) {
  scoringData$trim_name <- str_replace_all(scoringData$trim_name, term, "Premium")
}
replace_terms <- c("Classic", "Ultimate")
for(term in replace_terms) {
  scoringData$trim_name <- str_replace_all(scoringData$trim_name, term, "Essence")
}

trim_freq <- scoringData %>% group_by(trim_name) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_trims <- trim_freq %>% filter(Frequency < freq_threshold) %>% pull(trim_name)
scoringData$trim_name_grouped <- ifelse(scoringData$trim_name %in% infrequent_trims, 'Other', as.character(scoringData$trim_name))

# Group 'interior_color'
# interior_color
scoringData$interior_color <- tolower(scoringData$interior_color)
colors_to_standardize <- c("black", "gray", "brown", "red", "white","stone","dark","light","ebony","parchment","charcoal","blue","almond")
for(color in colors_to_standardize) {
  scoringData$interior_color <- ifelse(grepl(color, scoringData$interior_color), color, scoringData$interior_color)
}
# Replace "No Color" and "Unspecified" with "None"
scoringData$interior_color <- str_replace_all(scoringData$interior_color, "no color", "none")
scoringData$interior_color <- str_replace_all(scoringData$interior_color, "unspecified", "none")
scoringData$interior_color <- str_replace_all(scoringData$interior_color, "\\(.*?\\)", "")
scoringData$interior_color <- str_trim(scoringData$interior_color)

color_freq <- scoringData %>% group_by(interior_color) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_colors <- color_freq %>% filter(Frequency < freq_threshold) %>% pull(interior_color)
scoringData$interior_color_grouped <- ifelse(scoringData$interior_color %in% infrequent_colors, 'Other', as.character(scoringData$interior_color))

# Group 'exterior_color'
# exterior_color
replace_color_name <- function(color_name, dataframe, column_name) {
  # Converts color names to lowercase
  color_name_lower <- tolower(color_name)
  pattern <- paste0(".*", color_name_lower, ".*")  
  dataframe[[column_name]] <- str_replace_all(tolower(dataframe[[column_name]]), pattern, color_name_lower)
  return(dataframe)
}
# Define the colors to standardize in the exterior_color column
colors_to_standardize_exterior <- c("white", "blue", "red", "metallic", "black", "grey", "gray", "silver", "pearl", "coat", "orange", "yellow", "green")
for(color in colors_to_standardize_exterior) {
  scoringData <- replace_color_name(color, scoringData, "exterior_color")
}

color_freq <- scoringData %>% group_by(exterior_color) %>% summarise(Count = n()) %>%
  mutate(Frequency = Count / sum(Count))
infrequent_colors <- color_freq %>% filter(Frequency < freq_threshold) %>% pull(exterior_color)
scoringData$exterior_color_grouped <- ifelse(scoringData$exterior_color %in% infrequent_colors, 'Other', as.character(scoringData$exterior_color))

scoringData <- scoringData %>% select(-model_name, -trim_name, -interior_color, -exterior_color)

# 2 apply model to generate predictions=========================================
finalPredictions <- predict(rfModel, scoringData[ , c(top_features)])
# construct submission from predictions
submission <- data.frame(id = scoringDataid, price = finalPredictions)
colnames(submission)[2] <- "price"
write.csv(submission, 'submission.csv', row.names = FALSE)
