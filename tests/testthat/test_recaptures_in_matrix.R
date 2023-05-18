library(chicks.petrel)

describe("matriz de recapturas a partir del resumen", {
  resumen <- data.frame(
    eventos = c(2, 5, 5),
    recapturas = c(1, 2, 2),
    capturas = c(1, 4, 7)
  )
  assert_captures_equals_number_of_rows <- function(obtained_recaptures, capturas) {
    row_number <- nrow(obtained_recaptures)
    expect_equal(row_number, capturas)
  }
  it("Test first row", {
    recapturas <- 1
    eventos <- 2
    capturas <- 1
    obtained_recaptures <- recaptures_in_matrix(resumen[1, ])
    assert_captures_equals_number_of_rows(obtained_recaptures, capturas)
    expect_equal(ncol(obtained_recaptures), eventos)
    expect_equal(sum(obtained_recaptures), recapturas)
  })
  it("Test second row", {
    recapturas <- 2
    eventos <- 5
    capturas <- 4
    obtained_recaptures <- recaptures_in_matrix(resumen[2, ])
    assert_captures_equals_number_of_rows(obtained_recaptures, capturas)
    expect_equal(ncol(obtained_recaptures), eventos)
    expect_equal(sum(obtained_recaptures), recapturas)
  })
  it("Test third row", {
    recapturas <- 2
    eventos <- 5
    capturas <- 7
    obtained_recaptures <- recaptures_in_matrix(resumen[3, ])
    assert_captures_equals_number_of_rows(obtained_recaptures, capturas)
    expect_equal(ncol(obtained_recaptures), eventos)
    expect_equal(sum(obtained_recaptures), recapturas)
  })
})
