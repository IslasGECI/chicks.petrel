recaptures_in_matrix <- function(summary) {
  rand_state <- get_rand_state()
  on.exit(set_rand_state(rand_state))
  random_seed <- 2
  set.seed(random_seed)
  recaptures <- summary$recapturas
  eventos <- summary$eventos
  capturas <- summary$capturas
  repeticiones <- ceiling(capturas / eventos)
  to_capturas <- rep(diag(eventos), repeticiones)[1:(capturas * eventos)]
  matriz_base <- rep(1, eventos * capturas) - to_capturas
  to_delete <- sum(matriz_base) - recaptures
  with_ones <- which(matriz_base == 1)
  index_to_delete <- sample(with_ones, to_delete)
  matriz_base[index_to_delete] <- 0
  recapturas <- matrix(matriz_base, ncol = eventos, byrow = T)
  return(recapturas)
}
