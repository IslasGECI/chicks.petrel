#' @export
captures_in_matrix <- function(summary) {
  captures <- summary$capturas
  eventos <- summary$eventos
  repeticiones <- ceiling(captures / eventos)
  matriz <- matrix(
    rep(
      diag(eventos),
      repeticiones
    )[1:(captures * eventos)],
    ncol = eventos,
    byrow = T
  )
  return(matriz)
}
