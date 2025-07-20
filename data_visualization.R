library(dplyr)
library(tidyr)
library(readr)
library(styler)

PIMA_PATH <- "Diabetes-data.csv"
#' Load the PIMA Diabetes Dataset
#'
#' Loads a CSV file containing the PIMA diabetes dataset using `readr::read_csv`.
#'
#' @param file_path Path to the CSV file.
#' @param col_names Logical. Whether the first row contains column names. Default is TRUE.
#' @param display_type Logical. Whether to display column type messages. Default is FALSE.
#'
#' @return A data frame containing the loaded dataset.
#'
#' @examples
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}

#' Calculate the Statistical Mode
#'
#' Computes the mode (most frequent value) of a numeric or character vector.
#'
#' @param x A vector (numeric, character, or factor).
#' @param na.rm Logical. Whether to ignore missing values. Default is FALSE.
#'
#' @return The most frequent value in the vector, or NA if input is empty after NA removal.
#'
#' @examples
#' stat_mode(c(1, 2, 2, 3))
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Clean Columns with Missing Values
#'
#' Identifies columns with missing values. If more than 20% of the values in a column are missing,
#' the column is dropped. Otherwise, missing values are replaced with the column's minimum value.
#'
#' @param data_frame A data frame containing numeric columns with missing values.
#'
#' @return A cleaned data frame with imputed or removed columns as appropriate.
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


#' Remove Rows with Zero Values in Key Columns
#'
#' Iterates through the data frame and flags rows where any numeric column (except for 'Outcome' or 'Pregnancies')
#' contains a zero. The rows are retained in the current implementation but flagged for exclusion logic.
#'
#' @param data_frame A data frame to be processed.
#'
#' @return The original data frame (currently no rows are dropped, logic to be completed).
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

#' Execute Full Preprocessing and Generate Summary Table
#'
#' Loads, cleans, and preprocesses the PIMA dataset. Cleans missing values, removes problematic rows,
#' and generates a pivot table grouped by `Outcome` showing mean and median for each numeric variable.
#'
#' @return No return value. Prints summary table to console.
#'
#' @examples
#' runner()
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
  pima <- clean_row_with_zero(pima)
  pima_outcome_pivot <- pima %>%
    group_by(Outcome) %>%
    summarise(across(
      where(is.numeric) & !matches("Outcome"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
  cat("\nPIMA Outcome Pivot Table:", "\n")
  print.data.frame(pima_outcome_pivot)
}
runner()
