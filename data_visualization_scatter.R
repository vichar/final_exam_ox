library(dplyr)
library(tidyr)
library(readr)
library(styler)
library(reshape2)
library(ggplot2)


PIMA_PATH <- "Diabetes-data.csv"
#' Load the PIMA Diabetes Dataset
#'
#' Reads a CSV file into a data frame using readr. Optionally displays column type information.
#'
#' @param file_path String. Path to the CSV file.
#' @param col_names Logical. Whether the file contains header names. Default_
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}


#' Calculate the Mode of a Vector
#'
#' Returns the most frequent (modal) value in the vector, ignoring NA values.
#'
#' @param x A numeric or character vector.
#' @param na.rm Logical. If TRUE, missing values are removed before calculation.
#'
#' @return The mode (most common value) of the vector. Returns NA if no values are present.
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Handle Missing Values in Numeric Columns
#'
#' Checks for missing values in each column. If more than 20% are missing, the column is dropped.
#' Otherwise, missing values are replaced with the column's minimum value.
#'
#' @param data_frame A data frame with possible NA values.
#'
#' @return A cleaned data frame with no columns having >20% missing values, and missing values imputed.
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

#' Placeholder Loop for Zero-Value Checking
#'
#' Iterates through numeric columns (except 'Outcome' and 'Pregnancies') to examine rows with 0 values.
#' Does not currently filter out rows — this may be intended for validation or inspection logic.
#'
#' @param data_frame The input data frame to be checked.
#'
#' @return The unchanged input data frame.
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
#' Placeholder Loop for Zero-Value Checking
#'
#' Iterates through numeric columns (except 'Outcome' and 'Pregnancies') to examine rows with 0 values.
#' Does not currently filter out rows — this may be intended for validation or inspection logic.
#'
#' @param data_frame The input data frame to be checked.
#'
#' @return The unchanged input data frame.
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
  pima <- clean_row_with_zero(pima)
  pima[pima == "?"] <- NA
  pima[pima == ""] <- NA
  pima$Glucose <- as.numeric(pima$Glucose)
  pima <- pima %>%
    filter(!is.na(Glucose), !is.na(BMI))
  pima <- pima %>% mutate(GlucoseGroup = cut(Glucose, breaks = seq(0, 200, by = 10)))
  ggplot(pima, aes(
    x = Glucose,
    y = BMI,
    color = factor(Outcome)
  )) +
    geom_jitter(
      width = 1.5,
      height = 0,
      alpha = 0.7,
      size = 2
    ) +
    labs(
      title = "Scatter Plot of Glucose vs Insulin",
      x = "Glucose Level",
      y = "Insulin Level",
      color = "Diabetes Outcome"
    ) +
    scale_color_manual(
      values = c("0" = "blue", "1" = "red"),
      name = "Diabetes  Outcome",
      labels = c("Non-Diabetic", "Diabetic")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
runner()
