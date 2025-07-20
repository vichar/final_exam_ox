library(dplyr)
library(tidyr)
library(readr)
library(styler)
library(reshape2)
library(ggplot2)
library(GGally)

PIMA_PATH <- "Diabetes-data.csv"

#' Load the PIMA Diabetes Dataset
#'
#' Reads a CSV file into a data frame using `read_csv`. Optionally shows column types.
#'
#' @param file_path Path to the dataset CSV file.
#' @param col_names Logical; if TRUE, treats first row as column headers.
#' @param display_type Logical; if TRUE, displays column type messages.
#'
#' @return A data frame containing the loaded data.
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}


#' Calculate the Statistical Mode of a Vector
#'
#' Returns the most frequently occurring value (mode) in the input vector.
#'
#' @param x A numeric or character vector.
#' @param na.rm Logical; if TRUE, removes NA values before computation.
#'
#' @return The most common value in the vector, or NA if the input is empty.
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}

#' Clean Columns with Missing Numeric Data
#'
#' Checks each column for NA values. Drops the column if more than 20% are missing; otherwise, replaces NA with the column's minimum value.
#'
#' @param data_frame A data frame to process.
#'
#' @return A cleaned data frame with fewer or no missing numeric values.
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

#' Inspect for Zeroes in Numeric Columns
#'
#' Loops through each row and checks numeric columns (except Outcome and Pregnancies) for zeroes. Currently retains all data — placeholder logic.
#'
#' @param data_frame A data frame to validate.
#'
#' @return The original data frame, unchanged.

clean_row_with_zero <- function(data_frame) {
  for (row in 1:nrow(data_frame)) {
    for (col in names(data_frame)) {
      if (!(col %in% c("Outcome", "Pregnancies")) &&
        is.numeric(data_frame[[col]])) {
        if (!is.na(data_frame[row, col]) && data_frame[row, col] == 0) {
          value <- data_frame[row, col]
          if (!is.na(value) && value == 0) {
            break
          }
        }
      }
    }
  }
  return(data_frame)
}

#' Clean and Convert Glucose, BMI, and Related Columns
#'
#' Replaces invalid entries (`?` and blanks) with NA, coerces specified columns to numeric, and bins Glucose values into deciles.
#'
#' @param data_frame A data frame containing Glucose and BMI fields.
#'
#' @return A cleaned and transformed data frame with a GlucoseGroup column.
clean_up_glucose_bmi <- function(data_frame) {
  data_frame[data_frame == "?"] <- NA
  data_frame[data_frame == ""] <- NA
  data_frame$Glucose <- as.numeric(data_frame$Glucose)
  data_frame$DiabetesPedigreeFunction <- as.numeric(as.character(data_frame$DiabetesPedigreeFunction))
  data_frame$Age <- as.numeric(as.character(data_frame$Age))

  data_frame <- data_frame %>%
    filter(!is.na(Glucose), !is.na(BMI))
  data_frame <- data_frame %>% mutate(GlucoseGroup = cut(Glucose, breaks = seq(0, 200, by = 10)))
  return(data_frame)
}

#' Run the Full Data Cleaning and Scatter Matrix Plot
#'
#' Loads and preprocesses the PIMA dataset, including cleaning, NA handling, and binning.
#' Produces a scatter matrix plot (pairplot) to visualize variable relationships by diabetes status.
#'
#' @return A `ggpairs` object visualizing the dataset. Also plots it to the graphics device.
runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
  pima <- clean_row_with_zero(pima)
  pima <- clean_up_glucose_bmi(pima)

  ggpairs(
    data = pima,
    columns = c(
      "Pregnancies",
      "Glucose",
      "BloodPressure",
      "SkinThickness",
      "BMI",
      "DiabetesPedigreeFunction",
      "Age"
    ),
    aes(color = factor(Outcome), alpha = 0.5),
    lower = list(continuous = wrap("points", alpha = 1.5)),
    diag = list(continuous = wrap("barDiag", bins = 15)),
    upper = list(continuous = wrap("cor", size = 3))
  ) +
    theme_minimal() +
    labs(title = "Scatter Matrix of PIMA Indian Diabetes Dataset", subtitle = "Showing relationships between Pregnancies,Glucose,BloodPressure,SkinThickness,BMI,DiabetesPedigreeFunction,Age")
}
runner()
