# Loading Data

library(readr)

PIMA_PATH <- "Diabetes-data.csv"
load_dataset <- function(file_path, col_names = TRUE) {
  data_frame <- read_csv(file_path, col_names)
  return(data_frame)
}

pima <- load_dataset(PIMA_PATH)

cat("PIMA Sample Data:", "\n")
print(head(pima))

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

print(head(clean_missing_numeric_columns(pima)))



# Check the structure of the dataset
locate_complete_numeric_columns <- function(data_frame) {
  numeric_columns <- c()
  for (col in names(data_frame)) {
    if (is.numeric(data_frame[[col]])) {
      numeric_columns <- c(numeric_columns, col)
    }
  }
  return(numeric_columns)
}

locate_populate_empty_cells <- function(data_frame) {
  categorical_columns <- c()
  for (col in names(data_frame)) {
    if (is.character(data_frame[[col]]) ||
        is.factor(data_frame[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  for (col in categorical_columns) {
    # Replace empty strings with NA
    data_frame[[col]][data_frame[[col]] == ""] <- NA
    # Optionally, you can replace NA with the mode of the column
    mode_value <- as.character(stat_mode(data_frame[[col]], na.rm = TRUE))
    data_frame[[col]][is.na(data_frame[[col]])] <- mode_value
    data_frame[[col]][data_frame[[col]] == ""] <- NA
  }
  print.data.frame(data_frame)
}

numeric_complete_columns <- locate_complete_numeric_columns(pima)
print("Numeric Complete Columns:")
print.data.frame(numeric_complete_columns)
numeric_incomplete_columns <- locate_incomplete_numeric_columns(pima)
print("Numeric Incomplete Columns:")
print.data.frame(numeric_incomplete_columns)
data_set = locate_populate_empty_cells(pima)
print("Data after populating empty cells:")
print.data.frame(data_set)
