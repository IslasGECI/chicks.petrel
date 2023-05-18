#' @import readr
#' @export
join_resume_rows <- function(options) {
  left_data_path <- options[["left_data"]]
  right_data_path <- options[["right_data"]]
  left_data <- read_csv(left_data_path, show_col_types = FALSE)
  right_data <- read_csv(right_data_path, show_col_types = FALSE)
  joined_resumes <- bind_rows(left_data, right_data)
  write_csv(joined_resumes, options[["output_path"]])
}
