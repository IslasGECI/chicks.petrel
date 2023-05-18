describe("Join resumes for captures and recaptures", {
  it("join_resume_rows", {
    output_path <- "../data/joined_resume.csv"
    left_data <- "../data/resume_jun.csv"
    right_data <- "../data/resume_may.csv"
    options <- list("left_data" = left_data, "right_data" = right_data, "output_path" = output_path)
    join_resume_rows(options)
    expect_true(testtools::exist_output_file(output_path))
    testtools::delete_output_file(output_path)
  })
})
