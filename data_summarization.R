# Import necessary libraries
library(readr)
library(dplyr)
library(e1071)

# Define statistical mode
stat_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Loading Data
PIMA_PATH <- "Diabetes-data.csv"
load_dataset <- function(file_path, col_names = TRUE) {
  data_frame <- read_csv(file_path, col_names)
  return(data_frame)
}
data_summarization <- function(data_frame) {
  data_frame[data_frame == "?"] <- NA
  data_frame[data_frame == ""] <- NA
  data_frame %>%
    mutate(across(where(is.character), as.numeric)) %>%
    summarise(
      Count = n(),
      Median_Pregnancies = median(Pregnancies, na.rm = TRUE),
      Mode_Pregnancies = stat_mode(Pregnancies),
      Average_Age = mean(Age, na.rm = TRUE),
      Median_Age = median(Age, na.rm = TRUE),
      Mode_Age = stat_mode(Age),
      
      Average_Blood_Pressure = mean(BloodPressure, na.rm = TRUE),
      Median_Blood_Pressure = median(BloodPressure, na.rm = TRUE),
      Mode_Blood_Pressure = stat_mode(BloodPressure),
      Skewness_Blood_Pressure = skewness(BloodPressure, na.rm = TRUE),
      
      Average_Skin_Thickness = mean(SkinThickness, na.rm = TRUE),
      Average_Insulin = mean(Insulin, na.rm = TRUE),
      Skewness_Insulin = skewness(Insulin, na.rm = TRUE),
      
      Average_Glucose = mean(Glucose, na.rm = TRUE),
      Skewness_Glucose = skewness(Glucose, na.rm = TRUE),
      
      Average_BMI = mean(BMI, na.rm = TRUE),
      Median_BMI = median(BMI, na.rm = TRUE),
      Mode_BMI = stat_mode(BMI),
      Skewness_BMI = skewness(BMI, na.rm = TRUE),
      
      Average_Pedigree = mean(DiabetesPedigreeFunction, na.rm = TRUE),
      Median_Pedigree = median(DiabetesPedigreeFunction, na.rm = TRUE),
      Skewness_Pedigree = skewness(DiabetesPedigreeFunction, na.rm = TRUE)
    )
}

runner <- function() {
  pima_df <- load_dataset(PIMA_PATH)
  
  summarised_data  <- data_summarization(pima_df)
  
  cat("Summary of Pima Tribe Diabetes Dataset:", "\n", "\n")
  glimpse(summarised_data)
}

runner()
