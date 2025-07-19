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
    scale_fill_gradient2(
      low = "blue",
      mid = "yellow",
      high = "red"
    ) +
    labs(title = "Correlation Heatmap of PIMA Variables", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
runner()
