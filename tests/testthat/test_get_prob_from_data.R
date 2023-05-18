describe("test capture probability with open scenario with our data: 'resumen'", {
  set.seed(2)
  resumen <- data.frame(
    eventos = c(3, 5, 3, 3, 3),
    recapturas = c(6, 2, 5, 5, 7),
    capturas = c(83, 4, 100, 98, 98)
  )
  it("test quantile", {
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_3_events_83_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_prob <- get_prob_from_data(matrix, scenario)
    obtained_50_prob <- obtained_prob["p[1,2]", "50%"][[1]]
    expected_50_prob <- 0.224
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
  })
  it("Consider every row as a month with the peak of CPUE and every event is a netting night", {
    summary_peak_month <- data.frame(
      eventos = c(2, 3),
      recapturas = c(0, 5),
      capturas = c(41, 98)
    )
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_2_events_41_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_prob <- get_prob_from_data(matrix, scenario)
    obtained_50_prob <- obtained_prob["p[1,2]", "50%"][[1]]
    expected_50_prob <- 0.1432
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
    obtained_N_2021 <- summary_peak_month[1, 3] / obtained_50_prob
    expected_N_2021 <- 286.4
    expect_equal(obtained_N_2021, expected_N_2021, tolerance = 1e-3)
  })
  it("Consider every row as a year and every event is a month: In 2021 we have 4 month with netting, in 2022 we have 5 months with netting", {
    summary_year <- data.frame(
      eventos = c(4, 5),
      recapturas = c(9, 10),
      capturas = c(175, 350)
    )
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_4_events_175_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_prob <- get_prob_from_data(matrix, scenario)
    obtained_50_prob <- obtained_prob["p[1,2]", "50%"][[1]]
    expected_50_prob <- 0.0543
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
    obtained_N_2021 <- summary_year[1, 3] / obtained_50_prob
    expected_N_2021 <- 3222
    expect_equal(obtained_N_2021, expected_N_2021, tolerance = 1e-3)
  })
  it("Consider every row as multiple years and every event is a year: This is the resume from the last 'it'", {
    summary_multiple_years <- data.frame(
      eventos = c(2),
      recapturas = c(19),
      capturas = c(525)
    )
    summary_path <- "/workdir/chicks.petrel/tests/data/summary_2_events_525_captures.csv"
    matrix_df <- read_csv(summary_path, show_col_types = FALSE)
    matrix <- as.matrix(matrix_df)
    scenario <- "open"
    obtained_prob <- get_prob_from_data(matrix, scenario)
    obtained_50_prob <- obtained_prob["p[1,2]", "50%"][[1]]
    expected_50_prob <- 0.2844
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
    obtained_N_2021_and_2022 <- summary_multiple_years[1, 3] / obtained_50_prob
    expected_N_2021_and_2022 <- 1846
    expect_equal(obtained_N_2021_and_2022, expected_N_2021_and_2022, tolerance = 1e-3)
  })
})
describe("get probabilites from capture-recapture data", {
  set.seed(2)
  bobcat_dataframe <- read_csv("../data/bobcat.csv", show_col_types = FALSE)
  bobcat <- as.matrix(bobcat_dataframe)
  it("Closed with time effects", {
    scenario <- "closed_time"
    obtained_prob <- get_prob_from_data(bobcat, scenario)
    obtained_50_prob <- obtained_prob["p[8]", "50%"][[1]]
    expected_50_prob <- 0.274
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
  })
  it("Closed", {
    scenario <- "closed"
    obtained_prob <- get_prob_from_data(bobcat, scenario)
    obtained_50_prob <- obtained_prob["p[8]", "50%"][[1]]
    expected_50_prob <- 0.205
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-3)
  })
  it("Open", {
    scenario <- "open"
    summary_path <- "../data/simdata_for_CJS.csv"
    cjs_dataframe <- read_csv(summary_path, show_col_types = FALSE)
    data <- as.matrix(cjs_dataframe)
    obtained_prob <- get_prob_from_data(data, scenario)
    obtained_50_prob <- obtained_prob["p[2,5]", "50%"][[1]]
    expected_50_prob <- 0.444
    expect_equal(obtained_50_prob, expected_50_prob, tolerance = 1e-2)
  })
})
