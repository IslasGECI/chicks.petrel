describe("Get population size from probabilities", {
  set.seed(2)
  summary_peak_month <- data.frame(
    eventos = c(2, 3),
    recapturas = c(0, 5),
    capturas = c(41, 98)
  )
  it("Consider every row as a month with the peak of CPUE and every event is a netting night: 2021", {
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_2_events_41_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_N <- get_N_from_probabilities(matrix, scenario)
    obtained_50_N <- obtained_N[[1, "50%"]]
    expected_50_N <- 142.6
    expect_equal(obtained_50_N, expected_50_N, tolerance = 1e-3)
    obtained_50_N <- obtained_N[[2, "50%"]]
    expected_50_N <- 135.8
    expect_equal(obtained_50_N, expected_50_N, tolerance = 1e-3)
  })
  it("Consider every row as a month with the peak of CPUE and every event is a netting night: 2022", {
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_3_events_98_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_N <- get_N_from_probabilities(matrix, scenario)
    obtained_50_N <- obtained_N[[1, "50%"]] # TODO: extraer en una variable ¿Qué es este 1? ¿Eventos, años?
    expected_50_N <- 74.4
    expect_equal(obtained_50_N, expected_50_N, tolerance = 1e-3)
    obtained_50_N <- obtained_N[[2, "50%"]]
    expected_50_N <- 74.4
    expect_equal(obtained_50_N, expected_50_N, tolerance = 1e-3)
  })
})
