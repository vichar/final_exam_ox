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
#' @param col_names Logical. Indicates whether the first row contains column names. Default is TRUE.
#' @param display_type Logical. If TRUE, displays column type messages. Default is FALSE.
#'
#' @return A data frame containing the loaded data.
#'
#' @examples
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}


#' Calculate the Mode of a Vector
#'
#' Computes the most frequently occurring value in a vector.
#'
#' @param x A numeric or character vector.
#' @param na.rm Logical. If TRUE, missing values are removed. Default is FALSE.
#'
#' @return The mode (most frequent value), or NA if input is empty after NA removal.
#'
#' @examples
#' stat_mode(c(1, 1, 2, 3))  # returns 1
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Clean Columns with Missing Data
#'
#' Drops columns with more than 20% missing values. Fills remaining missing values
#' with the minimum value of that column.
#'
#' @param data_frame A data frame with possible missing values.
#'
#' @return A cleaned data frame with NA values handled or dropped.
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

#' Remove Rows with Zeroes in Numeric Variables
#'
#' Scans all numeric variables (excluding 'Outcome' and 'Pregnancies') and flags rows
#' containing 0 values for review. Currently retains all rows.
#'
#' @param data_frame A data frame to be checked.
#'
#' @return The input data frame (rows are currently not removed).
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
            break
          }
        }
      }
    }
  }
  data_frame <- na.omit(data_frame)
  return(data_frame)
}

#' Main Processing Pipeline for PIMA Dataset
#'
#' Executes full data preparation: loading, cleaning missing values, filtering rows,
#' and generating a correlation heatmap.
#'
#' Steps include:
#' - Reading the dataset
#' - Removing columns with >20% missing values
#' - Imputing remaining NAs with column minimums
#' - Dropping rows with zero values in numeric columns
#' - Computing pairwise correlations (excluding NA-only columns)
#' - Displaying a heatmap of variable correlations using ggplot2
#'
#' @return No return value. Displays a correlation heatmap using ggplot2.
#'
#' @examples
#' runner()
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
  pima <- clean_row_with_zero(pima)
  pima <- pima %>%
    select(where(is.numeric)) %>%
    select(where(~ !all(is.na(.x))))
  corelation_matrix <- cor(pima, use = "complete.obs")
  corelation_dataframe <- as.data.frame(as.table(corelation_matrix)) %>%
    filter(Var1 != Var2) %>%
    arrange(desc(Freq))
  colnames(corelation_dataframe) <- c("Var1", "Var2", "Correlation")
  ggplot(corelation_dataframe, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue",
                         mid = "yellow",
                         high = "red") +
    labs(title = "Correlation Heatmap of PIMA Variables", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
runner()
