#' Check for Small Cells in TableOne
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
check_for_small_size_cells_in_table_one <- function(table_one,
                                                    small_size = 6) {

  # Variable declaration ----------------------------------------------------------------------------

  variables_checked <- 0
  levels_checked <- 0
  variables_found <- 0
  levels_found <- 0
  small_cell_found <- FALSE
  var_names <- attr(table_one$CatTable[[1]], "names")
  counter <- 1
  freq_vector <- character()
  # Creates a first row in a data frame due to rbind function not working on an empty dataframe
  # A dummy row is used because first row is unknown at time of creation
  detected_small_cells <- data.frame(variable_name = "bla")
  detected_small_cells$factors <- list(c(1, 2, 3))

  # Small Cell detection ----------------------------------------------------------------------------

  for (j in table_one$CatTable[[1]]) {
    variables_checked <- variables_checked + 1
    for (row in 1:nrow(j)) {
      levels_checked <- levels_checked + 1
      frequent <- j[row, "freq"]
      lev_name <- j[row, "level"]
      if (frequent < small_size) {
        small_cell_found <- TRUE
        levels_found <- levels_found + 1
        freq_vector <- c(freq_vector, lev_name)
      }
    }
    if (small_cell_found) {
      variables_found <- variables_found + 1
      # Creates a temporary dataframe with data for the table that was read then that dataframe is added
      tmp <- data.frame(variable_name = var_names[counter])
      tmp$factors <- list(freq_vector)
      detected_small_cells <- rbind(detected_small_cells, tmp)
      small_cell_found <- FALSE
    }
    counter <- counter + 1
    freq_vector <- NULL
  }
  # Removes the dummy row from the return dataframe as well as resets the row count
  cat(variables_checked, " variables with ", levels_checked, " levels checked.\n\n")
  cat(variables_found, " variables with ", levels_found, " levels have cells <", small_size, " counts.\n\n")
  detected_small_cells <- detected_small_cells[-c(1), ]
  rownames(detected_small_cells) <- NULL
  return(detected_small_cells)
}
