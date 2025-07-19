library(dplyr)
library(tidyr)
library(readr)
library(styler)

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

runner <- function() {
  pima <- load_dataset(PIMA_PATH)
  pima <- clean_missing_numeric_columns(pima)
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
