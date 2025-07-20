library(readr)

PIMA_PATH <- "Diabetes-data.csv"

#' Load PIMA Diabetes Dataset
#'
#' This function loads the PIMA Diabetes dataset from a specified CSV file path using `readr::read_csv`.
#'
#' @param file_path String path to the CSV file.
#' @param col_names Logical indicating if the first row contains column names (default is TRUE).
#'
#' @return A data frame containing the loaded dataset.
#'
#' @examples
#' load_dataset("Diabetes-data.csv")
load_dataset <- function(file_path, col_names = TRUE) {
  data_frame <- read_csv(file_path, col_names)
  return(data_frame)
}

#' Compute Statistical Mode
#'
#' This function computes the mode (most frequent value) of a vector, ignoring NAs by default.
#'
#' @param x A vector (numeric, character, or factor).
#' @param na.rm Logical indicating whether to ignore missing values. Default is FALSE.
#'
#' @return A single value representing the mode, or NA if no mode is found.
#'
#' @examples
#' stat_mode(c(1, 2, 2, 3, 3, 3, 4))
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Clean Missing Numeric Columns
#'
#' This function drops columns with more than 20% missing values (NAs) and replaces remaining NAs
#' in numeric columns with the column's minimum non-NA value.
#'
#' @param data_frame A data frame with possible missing values.
#'
#' @return A cleaned data frame with no columns over 20% NA and all remaining NAs filled with column minimums.
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

#' Remove Rows Containing Zero Values in Key Columns
#'
#' This function scans each row and removes rows where any numeric column 
#' (except "Outcome" and "Pregnancies") contains a zero.
#'
#' @param data_frame A numeric data frame to clean.
#'
#' @return A data frame excluding rows with zero values in key numeric columns.
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
return(data_frame)
}

#' Run Full Data Cleaning Pipeline
#'
#' This function loads the PIMA dataset, cleans it by removing columns with too many missing values,
#' replaces missing values, and drops rows with zero values in key columns. Then it prints a sample.
#'
#' @return None (prints cleaned data to console).
#'
#' @examples
#' runner()
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
  cat("PIMA Sample Data after cleaning:", "\n")
  print.data.frame(clean_row_with_zero(pima))
}

runner()