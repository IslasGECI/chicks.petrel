#' @export
recapture_summary_2_matrix <- function(summary) {
  random_seed <- 2
  set.seed(random_seed)
  matriz <- captures_in_matrix(summary)
  matrix_recaptures <- recaptures_in_matrix(summary)
  matriz <- matriz + matrix_recaptures
  return(matriz)
}
