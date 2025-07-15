
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
#print(numeric_complete_columns)
#print(names(pima))

numeric_complete_columns <- locate_complete_numberic_columns(pima)
remaining_columns <- setdiff(names(pima), numeric_complete_columns)
print(remaining_columns)






