library(dplyr)
library(tidyr)
library(readr)
library(styler)
library(reshape2)
library(ggplot2)

# Loading Data

PIMA_PATH <- "Diabetes-data.csv"
load_dataset <- function(file_path,
                         col_names = TRUE,
                         display_type = FALSE) {
  data_frame <- read_csv(file_path, col_names = col_names, show_col_types = display_type)
  return(data_frame)
}


# Define statistical mode
stat_mode <- function(x, na.rm = FALSE) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  model_value <- names(sort(table(value = x), decreasing = TRUE)[1])
}


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
