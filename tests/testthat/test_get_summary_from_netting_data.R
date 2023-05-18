library(tidyverse)

describe("Summary table per season: one event per day", {
  may_data_path <- "../data/redeo_mayo_2022.csv"
  data_march <- read_csv("../data/redeo_marzo.csv", show_col_types = FALSE)
  data_may <- read_csv(may_data_path, show_col_types = FALSE)
  data_jul <- read_csv("../data/redeo_julio_2021.csv", show_col_types = FALSE)
  it("write_resume_catch_and_captures", {
    output_path <- "../data/resume_catch_and_captures_may.csv"
    options <- list("data_path" = may_data_path, "output_path" = output_path)
    write_resume_catch_and_captures(options)
    expect_true(testtools::exist_output_file(output_path))
  })
  it("count_events", {
    obtained_events_number <- count_events(data_may)
    expected_events_number <- 3
    expect_equal(obtained_events_number, expected_events_number)
    obtained_events_number <- count_events(data_jul)
    expected_events_number <- 4
    expect_equal(obtained_events_number, expected_events_number)
  })

  it("count_resighting", {
    obtained_resighting <- count_resighting(data_jul)
    expected_resighting <- 6
    expect_equal(obtained_resighting, expected_resighting)
    obtained_resighting <- count_resighting(data_may)
    expected_resighting <- 5
    expect_equal(obtained_resighting, expected_resighting)
    obtained_resighting <- count_resighting(data_march)
    expected_resighting <- 1
    expect_equal(obtained_resighting, expected_resighting)
  })
  it("count_captures", {
    obtained_captures <- count_captures(data_jul)
    expected_captures <- 80
    expect_equal(obtained_captures, expected_captures)
  })
  it("Count captures from May 2022", {
    obtained_captures <- count_captures(data_may)
    expected_captures <- 92
    expect_equal(obtained_captures, expected_captures)
  })
  it("Count captures from March 2022", {
    obtained_captures <- count_captures(data_march)
    expected_captures <- 43
    expect_equal(obtained_captures, expected_captures)
  })
  it("count_recaptures", {
    obtained_recaptures <- count_recaptures(data_jul)
    expected_recaptures <- 3
    expect_equal(obtained_recaptures, expected_recaptures)
    obtained_recaptures <- count_recaptures(data_may)
    expected_recaptures <- 6
    expect_equal(obtained_recaptures, expected_recaptures)
    obtained_recaptures <- count_recaptures(data_march)
    expected_recaptures <- 3
    expect_equal(obtained_recaptures, expected_recaptures)
  })
  it("Get year", {
    obtained_year <- get_year(data_may)
    expected_year <- 2022
    expect_equal(obtained_year, expected_year)
  })
  it("Write a row per season", {
    obtained_df <- create_summary_row_for_season(data_may)
    expected_cols <- c("temporada", "eventos", "capturas", "recapturas")
    obtained_cols <- names(obtained_df)
    expect_equal(obtained_cols, expected_cols)

    expected_first_row <- c(2022, 3, 98, 5)
    obtained_first_row <- obtained_df %>%
      as.numeric()
    expect_equal(obtained_first_row, expected_first_row)

    obtained_df_jul <- create_summary_row_for_season(data_jul)
    expected_first_row <- c(2021, 4, 83, 6)
    obtained_first_row <- obtained_df_jul %>%
      as.numeric()
    expect_equal(obtained_first_row, expected_first_row)
  })
})

describe("Summary table per season: one event per month (field trip)", {
  mar_data_path <- "../data/redeo_marzo.csv"
  data_mar <- read_csv(mar_data_path, show_col_types = FALSE)
  may_data_path <- "../data/redeo_mayo_2022.csv"
  data_may <- read_csv(may_data_path, show_col_types = FALSE)
  jul_data_path <- "../data/redeo_julio_2021.csv"
  data_jul <- read_csv(jul_data_path, show_col_types = FALSE)
  data_two_months <- bind_rows(data_may, data_jul)
  it("count_events", {
    obtained_events_number <- count_events_XXX(data_jul)
    expected_events_number <- 1
    expect_equal(obtained_events_number, expected_events_number)
    obtained_events_number <- count_events_XXX(data_two_months)
    expected_events_number <- 2
    expect_equal(obtained_events_number, expected_events_number)
    obtained_events_number <- count_events_XXX(data_mar)
    expected_events_number <- 1
    expect_equal(obtained_events_number, expected_events_number)
  })
})
