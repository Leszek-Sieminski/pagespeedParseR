#' Sorting data frame's columns alphabetically to effectively bind them later
#'
#' @param x data frane which columns need to be sorted
#'
#' @return data frame
fun_ps_basic_sort <- function(x){
  # capturing column names
  colnames_v <- colnames(x)

  # sorting column names alphabetically
  colnames_sorted <- sort(colnames_v)

  # deleting baseline columns (they need to go first!)
  colnames_sorted <- colnames_sorted[!colnames_sorted %in% c('device', 'title', 'url', 'status_code')]

  # adding baseline columns at the beggining of the df
  colnames_sorted <- c(c('device', 'title', 'url', 'status_code'), colnames_sorted)

  # sorting the df columns alphabetically
  sorted_df <- x[, colnames_sorted]
  return(sorted_df)
}
