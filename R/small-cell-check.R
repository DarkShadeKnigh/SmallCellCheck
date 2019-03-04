#' Check for Small Cells
#'
#' Small Cells Check checks a given table for small sells then adds a smallCells table to the MetaData of the table object
#'
#' Checks the categorical table within the TableOne param (CatTable field) for small cells. A small cell is a category where
#' the number of people in the category (n) is less than the value specified by the smallSize param. The freq field within
#' each variable has the n values.
#'
#' @param passedTable The object outputted by the CreateTableOne function of the tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param smallSize What value constitutes a small size cell. Default value is 6.
#' @param print If TRUE prints the smallSize metadata in a human readable format
#' @param tableType Specifies the type of the table that is passed to the function
#'
#' @return The passedTable object with a new object in the Metadata object called smallCells. smallCells is a dataframe with 2 columns
#' variableName and factors and the rows are all the categorical variables whose one or more factors have small cells.
#'
#'
#' @export
CheckSmallCells <- function(passedTable,
                            smallSize = 6,
                            print = TRUE,
                            tableType = "TableOne") {
  # Chosing Table procesing function -----------------------------------------------------------

  # Handles TableOne type tables
  if (tableType == "TableOne") {
    smallSizeTable <- CheckSmallCellsInTableOne(passedTable, smallSize)
    # In case an unsupported table type is used this error is thrown
  } else {
    stop(
      cat(
        "Table type ",
        tableType,
        " is not a valid table type or is not yet supported "
      ),
      "Unsupported Type"
    )
  }

  # Outputing the created Table function --------------------------------------------------------

  # Writes the created table into the MetaData object of the passed table
  passedTable$MetaData[["smallCells"]] <- smallSizeTable
  # Prints the table if the print is requested
  if (print) {
    print(passedTable$MetaData$smallCells)
  }
  return(passedTable)
}
