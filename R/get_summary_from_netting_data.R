#' @import readr
#' @export
write_resume_catch_and_captures <- function(options) {
  netting_data <- read_csv(options[["data_path"]], show_col_types = FALSE)
  summary <- create_summary_row_for_season(netting_data)
  write_csv(summary, options[["output_path"]])
}

count_events <- function(netting_data) {
  dates <- unique(netting_data$Fecha)
  length(dates)
}

count_events_XXX <- function(netting_data) {
  months <- lapply(netting_data$Fecha, substr, 4, 6)
  unique_months <- unique(months)
  length(unique_months)
}

count_captures <- function(netting_data) {
  count_event_type(netting_data, "Captura nueva")
}
count_resighting <- function(netting_data) {
  count_event_type(netting_data, "Reavistamiento")
}
count_recaptures <- function(netting_data) {
  count_event_type(netting_data, "Recaptura")
}
count_event_type <- function(netting_data, event_type) {
  event <- netting_data %>%
    filter(Tamaño_poblacional == event_type)
  nrow(event)
}

get_year <- function(netting_data) {
  years <- lubridate::year(lubridate::dmy(netting_data$Fecha))
  return(unique(years)[1])
}

create_summary_row_for_season <- function(netting_data) {
  tibble(
    temporada = get_year(netting_data),
    eventos = count_events(netting_data),
    capturas = count_captures(netting_data) + count_recaptures(netting_data),
    recapturas = count_resighting(netting_data)
  )
}
