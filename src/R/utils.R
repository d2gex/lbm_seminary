library("data.table")
library("dplyr")
library("readr")
library("readxl")
library("ggpubr")
library("openxlsx")
library("stringr")

read_sheets_from_excel <- function(file) {
  sheet_names <- excel_sheets(file)
  sheet_details <- lapply(sheet_names, function(x) {
    as.data.frame(read_excel(file, x))
  })
  names(sheet_details) <- sheet_names
  return(sheet_details)
}

write_sheets_to_excel <- function(df_list, filename, append = FALSE) {
  # @formatter:off
  #' write a list of dataframes as individual sheets into excel. If append = TRUE, the sheets are appended
  #' to the underlying existing file
  # @formatter:on
  if (append & file.exists(filename)) {
    existing_list <- read_sheets_from_excel(filename)
    df_list <- concat_list_with_repl(existing_list, df_list)
  }
  write.xlsx(df_list, filename)
}


transpose <- function(data, row_as_numeric = TRUE, as_matrix = TRUE) {

  if (row_as_numeric) {
    col_names <- unlist(lapply(names(data), function(x) {
      gsub("[^0-9]", "", x)
    }))
    names(data) <- col_names
  }
  data_t <- t(data)
  # Get first row as column names
  colnames(data_t) <- data_t[1,]
  data_t <- data_t[2:nrow(data_t),]

  if (as_matrix) {
    return(data_t)
  }
  return(as.data.frame(data_t))

}

df_to_unname_matrix <- function(data, from_col = NULL, to_col = NULL) {
  from_col <- ifelse(is.null(from_col), 1, from_col)
  to_col <- ifelse(is.null(to_col), length(names(data)), to_col)
  return(
    unname(as.matrix(data[, from_col:to_col]))
  )
}

df_to_named_matrix <- function(data) {
  mat <- as.matrix(data[, -1])
  rownames(mat) <- data[[1]]
  return(mat)
}

build_grid_title <- function(title, size = 15, just = "centre") {
  tgrob <- text_grob(title, size = size, just = just)
  return(ggarrange(plotlist = list(as_ggplot(tgrob)),
                   ncol = 1,
                   nrow = 1))
}