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
  small_cell_found <- FALSE
  var_names <- attr(table_one$CatTable[[1]], "names")
  counter <- 1
  freq_vector <- character()
  detected_small_cells <- data.frame(variable_name = "bla")
  detected_small_cells$factors <- list(c(1, 2, 3))
  for (j in table_one$CatTable[[1]]) {
    for (row in 1:nrow(j)) {
      frequent <- j[row, "freq"]
      lev_name <- j[row, "level"]
      if (frequent < small_size) {
        small_cell_found <- TRUE
        freq_vector <- c(freq_vector, lev_name)
      }
    }
    if (small_cell_found) {
      tmp <- data.frame(variable_name = var_names[counter])
      tmp$factors <- list(freq_vector)
      detected_small_cells <- rbind(detected_small_cells, tmp)
      small_cell_found <- FALSE
    }
    counter <- counter + 1
    freq_vector <- NULL
  }
  detected_small_cells <- detected_small_cells[-c(1), ]
  rownames(detected_small_cells) <- NULL
  return(detected_small_cells)
}
