#' @import multimark

#' @export
write_population_size_from_path <- function(options) {
  data_path <- options[["data_path"]]
  summary_input <- read_csv(data_path)
  output_path <- options[["output_path"]]
  population_size <- get_population_size_from_matrix_from_summary(summary_input)
  write_csv(population_size, output_path)
}

get_population_size_from_matrix_from_summary <- function(summary_input) {
  n_seasons <- nrow(summary_input)
  population_size <- summary_input |>
    mutate(population_size_interval = character(n_seasons))
  for (i_season in 1:n_seasons) {
    matrix <- recapture_summary_2_matrix(summary_input[i_season, ])
    quantiles <- get_population_size_from_matrix(matrix)
    quantiles_interval <- print_formatted_population_size(quantiles)
    population_size[i_season, ]$population_size_interval <- quantiles_interval
  }
  return(population_size)
}

#' @export
get_population_size_from_matrix <- function(datos, scenario = "closed", seed = TRUE) {
  if (seed) {
    set.seed(2)
  }
  petrel.dot <- get_petrel_dot(datos, scenario)
  quantiles <- summary(petrel.dot$mcmc)$quantiles[2, ]
  return(quantiles)
}

#' @export
get_petrel_dot <- function(datos, scenario = "closed") {
  choose_method <- list(
    "closed" = run_closed_scenario,
    "closed_time" = run_closed_time_scenario,
    "open" = run_open_scenario
  )
  petrel.dot <- choose_method[[scenario]](datos)
  return(petrel.dot)
}

get_N_from_probabilities <- function(matrix, scenario) {
  capture_probabilities <- get_prob_from_data(matrix, scenario)
  number_of_events <- ncol(matrix)
  N_for_each_event <- colSums(matrix) / rep(capture_probabilities["p[1,2]", ], number_of_events)
  N_matrix <- matrix(N_for_each_event, nrow = number_of_events, byrow = T)
  colnames(N_matrix) <- c("97.5%", "75%", "50%", "25%", "2.5%")
  N_matrix
}

get_prob_from_data <- function(data, scenario) {
  rand_state <- get_rand_state()
  on.exit(set_rand_state(rand_state))
  random_seed <- 2
  set.seed(random_seed)
  model <- get_petrel_dot(data, scenario)
  choose_method <- list(
    "closed" = getprobsClosed,
    "closed_time" = getprobsClosed,
    "open" = getprobsCJS
  )
  probs_object <- choose_method[[scenario]](model)
  summary(probs_object)$quantiles
}

run_closed_scenario <- function(datos) {
  datos_por_temporada <- processdata(
    Enc.Mat = datos,
    data.type = "never"
  )
  multimarkClosed(
    mms = datos_por_temporada,
    mod.p = ~1
  )
}

run_closed_time_scenario <- function(datos) {
  datos_por_temporada <- processdata(
    Enc.Mat = datos,
    data.type = "never"
  )
  multimarkClosed(
    mms = datos_por_temporada,
    mod.p = ~time
  )
}

run_open_scenario <- function(datos) {
  markCJS(
    Enc.Mat = datos,
    mod.p = ~1
  )
}

print_formatted_population_size <- function(quantiles) {
  q_025 <- as.integer(quantiles["2.5%"][[1]])
  q_50 <- as.integer(quantiles["50%"][[1]])
  q_975 <- as.integer(quantiles["97.5%"][[1]])
  return(glue::glue("{q_50} ({q_025} - {q_975})"))
}
