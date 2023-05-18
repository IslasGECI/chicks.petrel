resumen <- data.frame(
  eventos = c(3, 5, 3, 3, 3),
  recapturas = c(6, 2, 5, 5, 7),
  capturas = c(83, 4, 100, 98, 98)
)

describe("test get_population_size_from_matrix with closed scenario", {
  matrix <- recapture_summary_2_matrix(resumen[1, ])
  scenario <- "closed"
  obtained_population_size <- get_population_size_from_matrix(matrix, scenario, seed = TRUE)
  it("test first quantile", {
    obtained_first_quantile <- obtained_population_size["2.5%"][[1]]
    expected_first_quantile <- 216
    expect_equal(obtained_first_quantile, expected_first_quantile)
  })
  it("write quantiles", {
    expected_interval <- "366 (216 - 755)"
    obtained_interval <- print_formatted_population_size(obtained_population_size)
    expect_equal(obtained_interval, expected_interval)
  })
})
resumen_anual <- data.frame(
  eventos = c(2),
  recapturas = c(50),
  capturas = c(102)
)
describe("test get_population_size_from_matrix with open scenario", {
  matrix <- recapture_summary_2_matrix(resumen_anual)
  scenario <- "open"
  obtained_population_size <- get_population_size_from_matrix(matrix, scenario, seed = TRUE)
  it("test first quantile", {
    obtained_first_quantile <- obtained_population_size["2.5%"][[1]]
    expected_first_quantile <- 216
    is_quantile_equal <- obtained_first_quantile == expected_first_quantile
    expect_false(is_quantile_equal)
  })
})

describe("test difference", {
  it("first scenario", {
    datos <- recapture_summary_2_matrix(resumen[3, ])
    obtained <- get_population_size_from_matrix(datos, seed = TRUE)
    expected_interval <- "588 (328 - 1285)"
    obtained_interval <- print_formatted_population_size(obtained)
    expect_equal(obtained_interval, expected_interval)
  })
  it("second scenario", {
    datos <- recapture_summary_2_matrix(resumen[4, ])
    obtained <- get_population_size_from_matrix(datos, seed = TRUE)
    expected_interval <- "578 (322 - 1198)"
    obtained_interval <- print_formatted_population_size(obtained)
    expect_equal(obtained_interval, expected_interval)
  })
  it("third scenario", {
    datos <- recapture_summary_2_matrix(resumen[5, ])
    obtained <- get_population_size_from_matrix(datos, seed = TRUE)
    expected_interval <- "451 (273 - 849)"
    obtained_interval <- print_formatted_population_size(obtained)
    expect_equal(obtained_interval, expected_interval)
  })
})

describe("get population size from summary file", {
  population_size <- read_csv("../data/population_size_petrel_todos_santos.csv")
  expected_interval <- population_size$population_size_interval
  summary_input_path <- "../data/redeo_petrel_todos_santos.csv"
  summary_input <- read_csv(summary_input_path, show_col_types = FALSE)
  output_path <- "../data/population_size_from_path.csv"
  options <- list("data_path" = summary_input_path, "output_path" = output_path)
  it("gets firs scenerio from file", {
    matrix <- recapture_summary_2_matrix(summary_input[1, ])
    obtained <- get_population_size_from_matrix(matrix, seed = TRUE)
    expected_interval <- "366 (216 - 755)"
    obtained_interval <- print_formatted_population_size(obtained)
    expect_equal(obtained_interval, expected_interval)
  })
  it("displays multiple lines", {
    obtained_interval <- get_population_size_from_matrix_from_summary(summary_input)$population_size_interval
    expect_equal(obtained_interval, expected_interval)
  })
  it("write_population_size_from_path", {
    write_population_size_from_path(options)
    expect_true(testtools::exist_output_file(output_path))
  })
  it("contains the column 'population_size_interval'", {
    write_population_size_from_path(options)
    obtained_interval <- read_csv(output_path)$population_size_interval
    expect_equal(obtained_interval, expected_interval)
  })
})
