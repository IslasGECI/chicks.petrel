library(chicks.petrel)
resumen <- data.frame(
  eventos = c(5, 5),
  recapturas = c(2, 2),
  capturas = c(7, 4)
)
describe("test recapture_summary_2_matrix", {
  it("recapture_summary_2_matrix use dataframe with column names eventos, recapturas and capturas", {
    resumen <- c(5, 5)
    expect_error(recapture_summary_2_matrix(resumen))
    resumen <- data.frame(
      eventos = c(5, 5),
      recapturas = c(2, 2),
      XXcapturasXX = c(7, 4)
    )
    expect_error(recapture_summary_2_matrix(resumen))
  })
  it("recapture_summary_2_matrix es suma de capturas y recapturas", {
    nrow <- 1
    obtained <- sum(recapture_summary_2_matrix(resumen[nrow, ]))
    expected <- resumen$capturas[nrow] + resumen$recapturas[nrow]
    expect_equal(obtained, expected)
    nrow <- 2
    obtained <- sum(recapture_summary_2_matrix(resumen[nrow, ]))
    expected <- resumen$capturas[nrow] + resumen$recapturas[nrow]
    expect_equal(obtained, expected)
  })
})
