#' Small Cells Check
#'
#' Small Cells Check checks a given table for small sells then adds a small_cells table to the MetaData of the table object
#'
#' Checks the categorical table within the TableOne param (CatTable field) for small cells. A small cell is a category where
#' the number of people in the category (n) is less than the value specified by the small_size param. The freq field within
#' each variable has the n values.
#'
#' @param passed_table The object outputted by the CreateTableOne function of the tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param small_size What value constitutes a small size cell. Default value is 6.
#' @param print If TRUE prints the small_size metadata in a human readable format
#' @param table_type Specifies the type of the table that is passed to the function
#'
#' @return The passed_table object with a new object in the Metadata object called small_cells. small_cells is a dataframe with 2 columns
#' variable_name and factors and the rows are all the categorical variables whose one or more factors have small cells.
#'
#'
#'@export
small_size_cells <- function(passed_table,
                             small_size = 6,
                             print = TRUE,
                             table_type = "TableOne") {
  source("R\\formats\\table-one.R")
  if (table_type == "TableOne") {
    small_size_table <- table_one_small_cells(passed_table, small_size)
  } else {
    stop(
      cat(
        "Table type ",
        table_type,
        " is not a valid table type or is not yet supported "
      ),
      "Unsupported Type"
    )
  }
  print(small_size_table)
  passed_table$MetaData[["small_cells"]] <- small_size_table
  if (print) {
    print("THINKING")
    print(passed_table$MetaData$small_cells)
  }
  return(passed_table)
}
