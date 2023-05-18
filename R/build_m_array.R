get_Ns_from_matrix_history <- function(matrix_history) {
  get_Ms_from_matrix_history(matrix_history) / get_alphas_from_matrix_history(matrix_history)
}

get_Ms_from_matrix_history <- function(matrix_history) {
  numerador <- (get_ns_from_matrix_history(matrix_history) + 1) * get_z_from_matrix_history(matrix_history)
  denominador <- get_r_i_from_matrix_history(matrix_history) + 1
  Mt <- numerador / denominador + get_ms_from_matrix_history(matrix_history)
  return(Mt)
}

get_ms_from_matrix_history <- function(matrix_history) {
  m_array <- get_m_array(matrix_history)
  aux <- colSums(m_array)
  aux <- head(c(0, aux), -1)
  return(unname(aux))
}

get_z_i_from_matrix_history <- function(matrix_history, index) {
  m_array <- get_m_array(matrix_history)
  get_z_i_from_m_array(m_array, index)
}
get_z_from_matrix_history <- function(matrix_history) {
  n_events <- ncol(matrix_history) - 1
  aux <- comprehenr::to_vec(for (i in 2:n_events) get_z_i_from_matrix_history(matrix_history, i))
  c(0, aux, 0)
}

get_z_i_from_m_array <- function(m_array, index) {
  sum(m_array[1:(index - 1), index:(ncol(m_array) - 1)])
}

get_r_i_from_matrix_history <- function(matrix_history) {
  m_array <- get_m_array(matrix_history)
  r_s <- rowSums(m_array[, 1:(ncol(m_array) - 1)])
  return(c(unname(r_s), 0))
}

get_m_array <- function(matrix_history) {
  m_array <- IPMbook::marray(matrix_history)
}

get_ns_from_matrix_history <- function(matrix_history) {
  colSums(matrix_history)
}
get_alphas_from_matrix_history <- function(matrix_history) {
  ms <- get_ms_from_matrix_history(matrix_history)
  ns <- get_ns_from_matrix_history(matrix_history)
  (ms + 1) / (ns + 1)
}
