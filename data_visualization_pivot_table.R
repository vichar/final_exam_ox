library(dplyr)
library(tidyr)
library(readr)
library(styler)
library(reshape2)
library(ggplot2)


PIMA_PATH <- "Diabetes-data.csv"
#' Load the PIMA Diabetes Dataset
#'
#' Loads the PIMA dataset from a CSV file using `readr::read_csv`.
#'
#' @param file_path String. Path to the dataset file.
#' @param col_names Logical. Indicates if the first row contains column names. Default is TRUE.
#' @param display_type Logical. If TRUE, displays column type messages. Default is FALSE.
#'
#' @return A data frame containing the loaded data.
#'
#' @examples
#' load_dataset("Diabetes-data.csv")
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}


#' Calculate the Mode of a Vector
#'
#' Returns the most frequent value in a vector. If the input is empty after NA removal, returns NA.
#'
#' @param x A numeric or character vector.
#' @param na.rm Logical. Whether to remove missing values. Default is FALSE.
#'
#' @return A scalar value representing the mode.
#'
#' @examples
#' stat_mode(c(1, 2, 2, 3))  # Returns 2
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Clean Missing Data from Numeric Columns
#'
#' Identifies columns with missing values. If more than 20% of values are missing, the column is dropped.
#' Otherwise, missing values are replaced with the minimum of that column.
#'
#' @param data_frame A data frame with possible missing values.
#'
#' @return A cleaned data frame with imputed or removed columns.
#'
#' @examples
#' clean_missing_numeric_columns(pima_df)
clean_missing_numeric_columns <- function(data_frame) {
  incomplete_numeric_columns <- c()
  for (col in names(data_frame)) {
    if (any(is.na(data_frame[[col]]))) {
      incomplete_numeric_columns <- c(incomplete_numeric_columns, col)
    }
  }
  for (col in incomplete_numeric_columns) {
    total_count <- length(data_frame[[col]])
    na_count <- sum(is.na(data_frame[[col]]))
    if ((na_count / total_count) > 0.20) {
      data_frame[[col]] <- NULL
    } else {
      min_value <- min(data_frame[[col]])
      data_frame[[col]][is.na(data_frame[[col]])] <- min_value
    }
  }
  return(data_frame)
}

#' Categorize Individuals by BMI
#'
#' Assigns BMI categories ("Underweight", "Normal Weight", "Overweight") to each row in the dataset
#' based on the BMI value.
#'
#' @param data_frame A data frame containing a BMI column.
#'
#' @return A data frame with an additional column 'Category' indicating weight category.
#'
#' @examples
#' append_table_with_category(pima_df)
append_table_with_category <- function(data_frame) {
  for (row in 1:nrow(data_frame)) {
    weight_value <- data_frame[row, "BMI"]
    category_value <- if (is.na(weight_value)) {
      NA_character_
    } else if (weight_value < 23) {
      "Underweight"
    } else if (weight_value <= 29) {
      "Normal Weight"
    } else {
      "Overweight"
    }

    data_frame[row, "Category"] <- category_value
  }
  return(data_frame)
}

#' Identify and Retain Rows Without Zero in Numeric Columns
#'
#' Iterates through all numeric columns (except 'Outcome' and 'Pregnancies') and retains all rows.
#' Intended for placeholder or validation logic.
#'
#' @param data_frame A data frame to process.
#'
#' @return The input data frame. No rows are removed in the current implementation.
#'
#' @examples
#' clean_row_with_zero(pima_df)
clean_row_with_zero <- function(data_frame) {
for (row in 1:nrow(data_frame)) {
for (col in names(data_frame)) {
if (!(col %in% c("Outcome", "Pregnancies")) &&
is.numeric(data_frame[[col]])) {
if (!is.na(data_frame[row, col]) && data_frame[row, col] == 0) {
value <- data_frame[row, col]
if (!is.na(value) && value == 0) {
data_frame[row] <- NA # Set the entire row to NA if a zero is found
}
}
}
}
}
data_frame <- na.omit(data_frame)
return(data_frame)}


#' Filter and Convert BMI Column
#'
#' Replaces invalid BMI entries ('?' or blank) with NA, converts to numeric, and filters out NA or zero values.
#'
#' @param data_frame A data frame containing a BMI column.
#'
#' @return A filtered data frame with valid numeric BMI values only.
#'
#' @examples
#' filter_weight_columns(pima_df)
filter_weight_columns <- function(data_frame) {
  data_frame$BMI[data_frame$BMI == "?"] <- NA
  data_frame$BMI[data_frame$BMI == ""] <- NA
  data_frame$BMI <- as.numeric(data_frame$BMI)
  data_frame <- data_frame %>%
    filter(!is.na(BMI), BMI != 0)
  return(data_frame)
}

#' Main Runner Function for Processing and Aggregation
#'
#' Loads and preprocesses the PIMA dataset, assigns BMI categories, and computes the percentage of
#' diabetic individuals within each category. Prints a pivot table summarizing the result.
#'
#' @return No return value. Prints the output pivot table to the console.
#'
#' @examples
#' runner()
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_row_with_zero(pima)
  pima <- clean_missing_numeric_columns(pima)
  pima <- filter_weight_columns(pima)
  pima <- append_table_with_category(pima)
  pima_outcome_pivot <- pima %>%
    group_by(Category) %>%
    summarise(
      Count = n(),
      Diabetes_Outcome = sum(Outcome == 1, na.rm = TRUE),
      Percentage = (Diabetes_Outcome / Count) * 100
    )
  print(pima_outcome_pivot)
}

runner()
