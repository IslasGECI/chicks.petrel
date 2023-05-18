library(tidyverse)
describe("Filter netting raw data by month, season and species", {
  raw_data_path <- "../data/raw_redeo_petreles.csv"
  raw_data <- read_csv(raw_data_path, show_col_types = FALSE)
  month <- "May"
  season <- 2022
  species <- "Hydrobates homochroa"
  island <- "Todos Santos"
  it("write_filtered_by_month_season_species_and_island", {
    output_path <- "../data/output_write_filtered_by_month_season_species_and_island.csv"
    options <- list("data_path" = raw_data_path, "output_path" = output_path, "month" = month, "year" = season, "species" = species, "island" = island)
    write_filtered_by_month_season_species_and_island(options)
    expect_true(testtools::exist_output_file(output_path))
    testtools::delete_output_file(output_path)
  })
  it("filter_by_month_season_species_and_island", {
    options <- list("year" = season, "species" = species, "island" = island)
    trinity <- Trinity$new(options)
    obtained <- filter_by_month_season_species_and_island(raw_data, month, trinity)
    obtained_rows <- nrow(obtained)
    expected_rows <- 6
    expect_equal(obtained_rows, expected_rows)
    island <- "Coronado"
    options <- list("year" = season, "species" = species, "island" = island)
    trinity <- Trinity$new(options)
    obtained <- filter_by_month_season_species_and_island(raw_data, month, trinity)
    obtained_rows <- nrow(obtained)
    expected_rows <- 0
    expect_equal(obtained_rows, expected_rows)
  })
  it("filter_by_year", {
    year <- 2022
    obtained <- filter_by_year(raw_data, year)
    obtained_rows <- nrow(obtained)
    expected_rows <- 9
    expect_equal(obtained_rows, expected_rows)
  })
  it("filter_by_month", {
    month <- "Ago"
    obtained <- filter_by_month(raw_data, month)
    expected_rows <- 2
    obtained_rows <- nrow(obtained)
    expect_equal(obtained_rows, expected_rows)
  })
  it("filter_by_island", {
    island <- "Todos Santos"
    obtained <- filter_by_island(raw_data, island)
    expected_rows <- 19
    obtained_rows <- nrow(obtained)
    expect_equal(obtained_rows, expected_rows)
  })
  it("filter_by_species", {
    species <- "Hydrobates melania"
    obtained <- filter_by_species(raw_data, species)
    expected_rows <- 3
    obtained_rows <- nrow(obtained)
    expect_equal(obtained_rows, expected_rows)
  })
})
