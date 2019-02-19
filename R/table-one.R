#' Small Cells Check for TableOne
#'
#' Check for Small Cells inside a TableOne format Table
#'
#' Check The CatTable list for all possible small cells this also checks for all levels similar to showAllLevels in the CatTable from TableOne
#' documentation available here:
#'
#' @param table_one The object outputted by the CreateTableOne function of the tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param small_size What value constitutes a small size cell. Default value is 6.
#'
#' @return data frame with 2 columns variable_name and factors. This only adds the variables that contain small cells for easy identification. It
#' returns an empty table when no small cells are present
table_one_small_cells <- function(table_one,
                                  small_size = 6) {
  smallCellFound <- FALSE
  varNames <- attr(table_one$CatTable[[1]], "names")
  counter <- 1
  freqVector <- character()
  detectedSmallCells <- data.frame(variable_name = "bla")
  detectedSmallCells$factors <- list(c(1, 2, 3))
  for (j in table_one$CatTable[[1]]) {
    for (row in 1:nrow(j)) {
      frequent <- j[row, "freq"]
      levName  <- j[row, "level"]
      if (frequent < small_size) {
        smallCellFound <- TRUE
        freqVector <- c(freqVector, levName)
      }
    }
    if (smallCellFound) {
      tmp <- data.frame(variable_name = varNames[counter])
      tmp$factors <- list(freqVector)
      detectedSmallCells <- rbind(detectedSmallCells, tmp)
      smallCellFound <- FALSE
    }
    counter <- counter + 1
    freqVector <- NULL
  }
  detectedSmallCells <- detectedSmallCells[-c(1), ]
  rownames(detectedSmallCells) <- NULL
  return(detectedSmallCells)
}
