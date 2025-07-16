
# Loading Data 

library(readr)

PIMA_PATH <- "Diabetes-data.csv"
load_dataset <- function(file_path, col_names = TRUE) {
  data_frame <-read_csv(file_path, col_names)
  return(data_frame)
}


pima <- load_dataset(PIMA_PATH)

#print.data.frame(pima)

# Check the structure of the dataset
locate_complete_numberic_columns <- function(data_frame) {
  numeric_columns <- c() 
  for (col in names(data_frame)) {
    if(is.numeric(data_frame[[col]])) {
      numeric_columns <- c(numeric_columns, col)
    }
  }
  return(numeric_columns)
}

locate_incomplete_numberic_columns <- function(data_frame) {
  incomplete_numeric_columns <- c() 
  for (col in names(data_frame)) {
    if(any(is.na(data_frame[[col]]))) {
      incomplete_numeric_columns <- c(incomplete_numeric_columns, col)
    }
  }
  for (col in incomplete_numeric_columns) {
    median_value < median(data_frame[[col]], na.rm = TRUE)
    data_frame[[col]][is.na(data_frame[[col]])] <- median_value
  }
  print.data.frame(data_frame)
}

locate_populate_empty_cells <- function(data_frame) {
  categorical_columns <- c() 
  for (col in names(data_frame)) {
    if (is.character(data_frame[[col]]) || is.factor(data_frame[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  for (col in categorical_columns) {
   data_frame[[col]][data_frame[[col]] == ""] <- NA
  }
  print(categorical_columns)
}

numeric_complete_columns <- locate_complete_numberic_columns(pima)
print("Numeric Complete Columns:")
cat(numeric_complete_columns,"\n")
#numeric_incomplete_columns <- 
locate_incomplete_numberic_columns(pima)
#print("Numeric Incomplete Columns:")
#cat(numeric_incomplete_columns,"\n")
locate_populate_empty_cells(pima)
