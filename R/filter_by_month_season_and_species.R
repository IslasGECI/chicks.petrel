#' @import dplyr
#' @import readr

#' @export
write_filtered_by_month_season_species_and_island <- function(options) {
  raw_data_path <- options[["data_path"]]
  month <- options[["month"]]
  output_path <- options[["output_path"]]
  trinity <- Trinity$new(options)
  raw_data <- read_csv(raw_data_path, show_col_types = FALSE)
  filtered_data <- filter_by_month_season_species_and_island(raw_data, month, trinity)
  write_csv(filtered_data, output_path)
}

Trinity <- R6::R6Class("Trinity",
  public = list(
    island = NULL,
    species = NULL,
    year = NULL,
    initialize = function(option) {
      self$species <- option[["species"]]
      self$year <- option[["year"]]
      self$island <- option[["island"]]
    }
  )
)

write_filtered_by_season_species_and_island <- function(options) {
  raw_data_path <- options[["data_path"]]
  month <- options[["month"]]
  output_path <- options[["output_path"]]
  trinity <- Trinity$new(options)
  raw_data <- read_csv(raw_data_path, show_col_types = FALSE)
  filtered_data <- filter_by_season_species_and_island(raw_data, trinity)
  write_csv(filtered_data, output_path)
}

filter_by_month_season_species_and_island <- function(raw_data, month, trinity) {
  raw_data |>
    filter_by_month(month) |>
    filter_by_season_species_and_island(trinity)
}

filter_by_season_species_and_island <- function(raw_data, trinity) {
  raw_data |>
    filter_by_year(trinity$year) |>
    filter_by_island(trinity$island) |>
    filter_by_species(trinity$species)
}

filter_by_year <- function(raw_data, year) {
  raw_data |> filter(stringr::str_detect(Fecha, as.character(year)))
}

filter_by_month <- function(raw_data, month) {
  raw_data |> filter(stringr::str_detect(Fecha, month))
}


filter_by_island <- function(raw_data, island) {
  raw_data |> filter(Isla == island)
}

filter_by_species <- function(raw_data, species) {
  raw_data |> filter(Especie == species)
}
