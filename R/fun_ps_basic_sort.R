#' Sorting data frame's columns alphabetically to effectively bind them later
#'
#' @param x data frane which columns need to be sorted
#'
#' @return data frame
fun_ps_basic_sort <- function(x){
  colnames_v <- colnames(x) # capturing column names
  colnames_sorted <- sort(colnames_v) # sorting column names alphabetically

  # deleting baseline columns (they need to go first!)
  colnames_sorted <- colnames_sorted[!colnames_sorted %in% c('device', 'title', 'url', 'status_code')]

  # adding baseline columns at the beggining of the df
  colnames_sorted <- c(c('device', 'title', 'url', 'status_code'), colnames_sorted)
  sorted_df <- x[, colnames_sorted] # sorting the df columns alphabetically
  return(sorted_df)
}
