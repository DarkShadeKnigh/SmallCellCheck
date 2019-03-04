#' Check for Small Cells in TableOne
#'
#' Check for Small Cells inside a TableOne format Table
#'
#' Check The CatTable list for all possible small cells this also checks for all levels similar to showAllLevels in the CatTable from TableOne
#' documentation available here:
#'
#' @param tableOne The object outputted by the CreateTableOne function of the tableone package. The documentation is available here
#' https://cran.r-project.org/web/packages/tableone/index.html.
#' @param smallSize What value constitutes a small size cell. Default value is 6.
#'
#' @return data frame with 2 columns variableName and factors. This only adds the variables that contain small cells for easy identification. It
#' returns an empty table when no small cells are present
CheckSmallCellsInTableOne <- function(tableOne,
                                      smallSize = 6) {
  # Variable declaration ----------------------------------------------------------------------------

  variablesChecked <- 0
  levelsChecked <- 0
  variablesFound <- 0
  levelsFound <- 0
  smallCellFound <- FALSE
  varNames <- attr(tableOne$CatTable[[1]], "names")
  counter <- 1
  freqVector <- character()
  # Creates a first row in a data frame due to rbind function not working on an empty dataframe
  # A dummy row is used because first row is unknown at time of creation
  detectedSmallCells <- data.frame(variableName = "DummyRow")
  detectedSmallCells$factors <- list(c(1, 2, 3))

  # Small Cell detection ----------------------------------------------------------------------------

  for (j in tableOne$CatTable[[1]]) {
    variablesChecked <- variablesChecked + 1
    for (row in 1:nrow(j)) {
      levelsChecked <- levelsChecked + 1
      frequent <- j[row, "freq"]
      levName <- j[row, "level"]
      if (frequent < smallSize) {
        smallCellFound <- TRUE
        levelsFound <- levelsFound + 1
        freqVector <- c(freqVector, levName)
      }
    }
    if (smallCellFound) {
      variablesFound <- variablesFound + 1
      # Creates a temporary dataframe with data for the table that was read then that dataframe is added
      tmp <- data.frame(variableName = varNames[counter])
      tmp$factors <- list(freqVector)
      detectedSmallCells <- rbind(detectedSmallCells, tmp)
      smallCellFound <- FALSE
    }
    counter <- counter + 1
    freqVector <- NULL
  }
  # Removes the dummy row from the return dataframe as well as resets the row count
  cat(variablesChecked,
      " variables with ",
      levelsChecked,
      " levels checked.\n\n")
  cat(
    variablesFound,
    " variables with ",
    levelsFound,
    " levels have cells <",
    smallSize,
    " counts.\n\n"
  )
  detectedSmallCells <- detectedSmallCells[-c(1),]
  rownames(detectedSmallCells) <- NULL
  return(detectedSmallCells)
}
